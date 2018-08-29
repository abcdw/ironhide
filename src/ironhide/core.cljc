(ns ironhide.core
  (:require [com.rpl.specter :as sp]
            #?(:cljs [orchestra-cljs.spec.test :as stest]
               :clj [orchestra.spec.test :as stest])
            [clojure.spec.alpha :as s]
            [clojure.string :as cstr]))

;; =============================================================================
;; specs

(defn- ih? [x]
  (and (keyword? x)
       (= "ih" (namespace x))))

(defn- special? [ns key x]
  (or
   (and (keyword? x) (= ns (namespace x)))
   (and (map? x) (contains? x key))))

(defn- micro? [x]
  (special? "ihm" :ih/micro x))

(defn- sight? [x]
  (special? "ihs" :ih/sight x))

(s/def ::shell
  (s/keys :req [:ih/rules :ih/direction]
          :opt [:ih/data :ih/sights :ih/micros :ih/values]))

(s/def :ih/rules (s/coll-of ::rule))
(s/def :ih/direction (s/cat :from ::charge :to ::charge))
(s/def :ih/sights (s/map-of #(= "ihs" (namespace %)) ::nested-sight))

(s/def ::nested-sight
  (s/or :shell ::shell
        :map map?))

(s/def ::rule (s/coll-of
               (s/or
                :charge
                (s/tuple ::charge ::path)
                :ih/value
                (s/tuple #(= :ih/value %) ::charge->path))
               :into []
               :min-count 2
               :max-count 3))

(s/def ::charge->path
  (s/map-of ::charge ::path))
(s/def :ih/value ::charge->path)

(s/def ::charge keyword?)

(s/def ::pmode #{:get :set})
(s/def ::path (s/coll-of ::pelem))
(s/def ::pelem (s/or :mkey ::mkey
                     :sight ::sight
                     :vnav ::vnav))
(s/def ::mkey keyword?)
(s/def ::sight sight?)
(s/def ::vnav
  (s/cat :vkey ::vkey :vfilter (s/? ::vfilter)))

(s/def ::vkey (s/or :index ::index
                    :wildcard ::wildcard))
(s/def ::wildcard #(= :* %))
(s/def ::index nat-int?)
(s/def ::vfilter map?)

(s/fdef execute
  :args (s/cat :shell ::shell)
  :ret ::shell)

(s/fdef path->sp-path
  :args (s/or
         :less-args (s/coll-of any? :max-count 2)
         :arg3 (s/cat :ctx map? :path ::path :pmode ::pmode))
  :ret sequential?)

;; =============================================================================

(defn- deep-merge [a b]
  (if (and
       (map? a)
       (map? b))
    (merge-with deep-merge a b)
    b))

(defn- match-recur [errors path x pattern]
  (cond
    (and (map? x)
         (map? pattern))
    (reduce (fn [errors [k v]]
              (let [path  (conj path k)
                    ev (get x k)]
                (match-recur errors path ev v)))
            errors pattern)

    (and (vector? pattern)
         (seqable? x))
    (reduce (fn [errors [k v]]
              (let [path (conj path k)
                    ev  (nth (vec x) k nil)]
                (match-recur errors path ev v)))
            errors
            (map (fn [x i] [i x]) pattern (range)))

    :else (let [err (when-not (= pattern x) {:expected pattern :but x})]
            (if err
              (conj errors (assoc err :path path))
              errors))))

(defn- match*
  [x & patterns]
  (reduce (fn [acc pattern] (match-recur acc [] x pattern)) [] patterns))

(defn- m-valid? [pattern x]
  (if (empty? (match* x pattern))
    true
    false))


;; =============================================================================
;; sights

(defmulti get-global-sight
  "Returns pair of parse/unparse functions"
  (fn [key args & [ctx]] key))

(defmethod get-global-sight :ihs/str<->vector [key args & [ctx]]
  (let [separator (get args :separator " ")]
    [#(if % (cstr/split % (re-pattern separator)) [])
     #(cstr/join separator %)]))

(defn- get-scoped-sight [{{sights :sights} :ih/internals :as ctx} key args]
  (if (contains? sights key)
    (key sights)
    (get-global-sight key args ctx)))

(defn- inverse-shell [shell]
  ;; TODO: move to specter
  (update shell :ih/direction reverse))

(declare get-data)

(defn- transform [shell left right & direction]
  (let [[from to :as dir] (or direction
                              (:ih/direction shell)
                              [:left :right])]
    (->
     shell
     (assoc-in [:ih/data from] left)
     (assoc-in [:ih/data to] right)
     (assoc-in [:ih/direction] dir)
     (get-data to))))

(defn- get-transformer [shell]
  (fn [data]
    (transform shell data nil)))

(defn- get-inverse-transformer [shell]
  (get-transformer (update shell :ih/direction reverse)))

(defn- inverse-map [m]
  (into {} (map (comp vec reverse) m)))

(defn- get-nested-sight-from-map [m]
  (if (contains? m :ih/rules)
    [(get-transformer m) (get-inverse-transformer m)]
    [m (inverse-map m)]))

(defn- add-nested-sights [shell]
  (assoc-in shell [:ih/internals :sights]
            (sp/transform [sp/MAP-VALS]
                          get-nested-sight-from-map
                          (get-in shell [:ih/sights]))))

;; =============================================================================

(def ^:private ALL-INDEXED [sp/INDEXED-VALS (sp/collect-one 0) 1])

(defn- wildcard? [pelem]
  (m-valid? [:*] pelem))

(defn- has-wildcards? [path]
  (not-empty (filter wildcard? path)))

(defn- vfilter->template [vfilter next-pelem]
  (if vfilter
    vfilter
    (if (vector? next-pelem) [] nil)))

(defn- get-vfilter-fn [vfilter]
  (if vfilter
    (fn [x] (m-valid? vfilter x))
    (constantly true)))

(defn- vec->specter [[navigator & [vfilter] :as pelem] next-pelem]
  (let [filter-fn (get-vfilter-fn vfilter)
        template  (vfilter->template vfilter next-pelem)]
    [sp/NIL->VECTOR

     (if (wildcard? pelem)
       [(sp/filterer filter-fn) ALL-INDEXED]

       (if vfilter
         (sp/if-path #(empty? (filter filter-fn %))
                     [sp/END 0 (sp/nil->val template)]
                     [(sp/filterer filter-fn) navigator])
         [(sp/nil->val template) navigator]))]))

;; (sp/setval [(vec->specter [0] []) 0] :test [])
;; => [[:test]]

(defn- get-special-name [type pelem]
  (cond
    (keyword? pelem) pelem
    (map? pelem)     (type pelem)))

(defn- get-micro-name [pelem]
  (get-special-name :ih/micro pelem))

(defn- get-sight-name [pelem]
  (get-special-name :ih/sight pelem))

(defn- get-args-from-pelem [pelem]
  (if (map? pelem) (into {} (remove #(ih? (first %)) pelem)) {}))

(defn- get-sight-from-pelem [ctx pelem]
  (let [sight-name      (get-sight-name pelem)
        args            (get-args-from-pelem pelem)
        [parse unparse] (get-scoped-sight ctx sight-name args)]
    (sp/parser parse unparse)))

(defn- sight->specter [ctx pelem]
  [(get-sight-from-pelem ctx pelem)])

(defn- pelems->specter [ctx pelem next-pelem mode]
  (let [a :a]
    (condp s/valid? pelem
      ::vnav  (vec->specter pelem next-pelem)
      ::sight (sight->specter ctx pelem)
      ::mkey  [pelem])))

(defn path->sp-path
  "Generates a specter path from ironhide path"
  ([path] (path->sp-path {} path))
  ([ctx path] (path->sp-path ctx path :set))
  ([ctx path pmode]
   (reduce
    (fn [acc [pelem next-pelem]]
      (concat acc (pelems->specter ctx pelem next-pelem pmode)))
    []
    (partition 2 1 [nil] path))))

(defn get-values [ctx path]
  (let [sp-path (path->sp-path ctx path)]
    (if (has-wildcards? path)
      (sp/select sp-path ctx)
      [(sp/select sp-path ctx)])))

(defn- path->value [values]
  (->>
   (for [value values]
     [(butlast value) (last value)])
   (into {})))

(defn set-values [ctx path values]
  (let [values  (if (vector? values)
                  (path->value values)
                  values)
        sp-path (path->sp-path ctx path)]
    (sp/transform
     sp-path
     (fn [& v]
       (let [path  (butlast v)
             new-value (values path)
             old-value (last v)]
         (if (and (map? new-value)
                  (map? old-value))
           (deep-merge old-value new-value)
           (or new-value old-value))))
     ctx)))

(defn- get-templates [path]
  (for [[[_ vfilter] next-pelem]
        (filter (comp wildcard? first) (partition 2 1 [nil] path))]
    (vfilter->template vfilter next-pelem)))

;; (get-templates [[:* {:a :b}] :a [:*] [:*]])
;; => ({:a :b} [] nil)

(defn- splitcat-by-wildcard [path]
  (get
   (reduce
    (fn [{:keys [curr-path] :as acc} pelem]
      (let [new-path (conj curr-path pelem)]
        (cond-> (assoc acc :curr-path new-path)
          (wildcard? pelem) (update :paths conj new-path))))
    {:curr-path []
     :paths     []}
    path)
   :paths))

;; (splitcat-by-wildcard [[:*] :a :b [:*] :v])
;; => [[[:*]] [[:*] :a :b [:*]]]

(defn- counting-sp-path [ctx path]
  (let [[_ vfilter] (last path)]
    (concat
     (path->sp-path ctx (butlast path))
     (if vfilter [(sp/filterer (get-vfilter-fn vfilter))] []))))

(defn- count-matched-nodes [ctx path]
  (let [res (sp/select (counting-sp-path ctx path) ctx)]
    (if (has-wildcards? (butlast path))
      (mapv #(count (last %)) res)
      [(count (first res))])))

(defn- tree-shape [ctx path]
  (map #(count-matched-nodes ctx %)
       (splitcat-by-wildcard path)))

(defn- count-shape-diff [source-shape sink-shape]
  (map #(map - %1 (concat %2 (repeat 0)))
       source-shape
       sink-shape))

(defn- add-level-elems [ctx path elem ncounts]
  (let [wc?        (wildcard? (last path))
        first-part (butlast path)
        vfilter-fn    (get-vfilter-fn (if wc? (second (last path))))

        insert-path
        (path->sp-path
         ctx
         (if wc? first-part path))

        select-path
        (if wc?
          (concat
           (path->sp-path ctx (butlast path))
           [(sp/filterer vfilter-fn)])
          path)

        insert-fn
        (into {} (mapv (fn [[& args] ncount]
                         [(butlast args) (repeat ncount elem)])
                       (sp/select select-path ctx)
                       ncounts))]

    (sp/transform
     [insert-path sp/END]
     (fn [& args]
       (insert-fn (butlast args)))
     ctx)))

(defn- add-templates [source-data sink-data [source-path sink-path]]
  (let [diff (count-shape-diff
              (tree-shape source-data source-path)
              (tree-shape sink-data sink-path))

        defaults        (get-templates sink-path)
        branching-paths (splitcat-by-wildcard sink-path)]

    (loop [res                  sink-data
           [path & rpaths]      branching-paths
           [elem & relems]      defaults
           [ncounts & rncounts] diff]
      (if ncounts
        (recur
         (add-level-elems res path elem ncounts)
         rpaths
         relems
         rncounts)
        res))))

(defn- get-full-path [path & [prefix]]
  (if (ih? (first path))
    path
    (into prefix path)))

(defn- apply-rule [ctx rule]
  (let [[from to] (or (:ih/direction rule)
                      (:ih/direction ctx)
                      [:data :data])

        {source-path from
         sink-path   to} rule

        full-source-path (get-full-path source-path [:ih/data from])
        full-sink-path   (get-full-path sink-path [:ih/data to])

        value-path (get-in rule [:ih/value to])

        full-value-path (get-full-path value-path [:ih/values])

        source-values (get-values ctx full-source-path)
        value         (get-values ctx full-value-path)

        ;; TODO: write a proper empty-result? fn
        empty-values? (fn [x] (m-valid? [[nil]] x))
        values        (if (or (empty-values? source-values) (nil? source-path))
                        nil
                        source-values)]
    (if sink-path
      (cond
        values
        (set-values
         (add-templates ctx ctx [full-source-path full-sink-path])
         full-sink-path
         values)

        (and (not (empty-values? value)) (not (nil? value-path)))
        (set-values
         ctx
         full-sink-path
         (fn [k] (first (first value))))

        :else
        ctx)
      ctx)))

(defn- microexpand [micro pelem]
  (let [values   (get-args-from-pelem pelem)
        keys-set (set (remove ih? (keys values)))]
    (sp/transform [(sp/walker #(keys-set %))] values micro)))

;; (microexpand [:telecom [:%1] :value] {:%1 :*})
;; => [:telecom [:*] :value]

(defn- pred-from-micro-name [micro-name]
  (fn [x]
    (or (and (keyword? x) (= micro-name x))
        (and (map? x) (m-valid? {:ih/micro micro-name} x)))))

(defn- microexpand-path [{micros :ih/micros} path]
  (reduce
   (fn [path pelem]
     (let [micro-name (get-micro-name pelem)
           micro      (get micros micro-name)]
       (if micro
         (into path (microexpand micro pelem))
         (conj path pelem))))
   []
   path))

;; (microexpand-path
;;  {:ih/micros #:ihm {:name [:name [0]]
;;                     :given [:given [:ind]]}}
;;  [:ihm/name {:ih/micro :ihm/given :ind 1}])
;; => [:name [0] :given [1]]

(defn microexpand-shell [{micros :ih/micros :as ctx}]
  (let []
    (sp/transform
     [:ih/rules sp/ALL (sp/filterer #(not (ih? (first %)))) sp/ALL 1]
     (fn [path]
       (microexpand-path ctx path))
     ctx)))

(defn execute [shell]
  (let [expanded-shell    (-> shell
                              microexpand-shell
                              add-nested-sights)
        {rules :ih/rules} expanded-shell]
    (reduce
     (fn [ctx rule]
       (apply-rule ctx rule))
     expanded-shell
     rules)))

(defn get-data [shell & [key]]
  (->
   (execute shell)
   :ih/data
   (#(get % key))))


(when *assert*
  (stest/instrument))
