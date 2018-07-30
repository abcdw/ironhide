(ns ironhide.core
  (:require [com.rpl.specter :as sp]
            [matcho.core :as m]
            [clojure.string :as cstr]))

;; Ironhide, the data transformer
;; language agnostic bidirectional data transformation dsl

;; Additional features:
;; * Leaf update function str split/join
;; * Default values?
;; * IDS in templates and default values
;; * range navigator, take part of the vector by index instead of filter
;; * recursive mappings
;; * Coercion?
;; * Destructuring syntax
;; * Filter using value from source
;; * Use maps instead of vectors and convert them to vectors after transformation
;; {2 {:a :b}} => [nil nil {:a :b}]

;; Grammar:
;; key: keyword
;; index: integer
;; wildcard: ':*'
;; template: map // matcho pattern without predicates
;; vfilter: template
;; navigator: (wildcard | index)
;; vec: '[' navigator vfilter? ']'
;; pelem: (key | vec)
;; path: '[' pelem* ']'
;;
;; Example:
;; [:telecom [:* {:system "phone"}] :value]

;; (sp/setval [:a (sp/nil->val {:b :c}) :some-key] sp/NONE {})

;; [{:val    5
;;   :system "phone"}
;;  {:val    "mail@mail.com"
;;   :system "email"}]

;; [{"phone" 5} {"email" "mail@mail.com"}]

;; {:form [:phones]
;;  :fhir [:telecom]
;;  :inner [{:form [[0]]
;;           :fhir [[0 {:system "phone"}] :value]}
;;          {:form [[1]]
;;           :fhir [[1 {:system "phone" :use "home"} :value]]}]}

;; {::ih/alias {:telecom [:telecom [:%1] :value]}
;;  :form {}
;;  :fhir {}
;;  ::ih/values {:test "Patient/UUID"} ;; should be attacheable
;;  ::ih/rules
;;  [{::ih/transform [[:form :fhir]]}
;;   {:form [:name {:ironhide/parser :str-space}]
;;    ::ih/name :patient-id
;;    ::ih/defaults {:fhir :fhir/patient-id
;;                   :form :form/patient-id}
;;    ::ih/default [:name]
;;    :fhir [{::ih/ref :telecom :args [:*]}]}]}

;; (sp/transform [(sp/walker #(m/valid? {:c :d} %))] #(println %) {:a [:b {:c :d :e :f}]})

(defmulti get-parser
  "Returns pair of parse/unparse functions"
  (fn [key & args] key))

(defmethod get-parser :str<->vector [key args]
  (let [separator (or (first args) " ")]
    [#(if % (cstr/split % (re-pattern separator)) [])
     #(cstr/join separator %)]))

(defmethod get-parser :default [key args]
  [identity identity])

(defn- wildcard? [pelem]
  (m/valid? [:*] pelem))

(defn- has-wildcards? [path]
  (not-empty (filter wildcard? path)))

(def ^:private ALL-INDEXED [sp/INDEXED-VALS (sp/collect-one 0) 1])

(defn- vfilter->template [vfilter next-pelem]
  (if vfilter
    vfilter
    (if (vector? next-pelem) [] nil)))

(defn- get-vfilter-fn [vfilter]
  (if vfilter
    (fn [x] (m/valid? vfilter x))
    (constantly true)))

(defn- vec->specter [[navigator & [vfilter] :as pelem] next-pelem]
  (let [filter-fn (get-vfilter-fn vfilter)
        template (vfilter->template vfilter next-pelem)]
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

(defn- get-parser-from-pelem [pelem]
  (let [parser-name     (:ironhide/parser pelem)
        args            (:args pelem)
        [parse unparse] (get-parser parser-name args)]
    (sp/parser parse unparse)))

(defn- map->specter [pelem]
  (cond
    (contains? pelem :ironhide/parser) [(get-parser-from-pelem pelem)]
    :else [pelem]))

(defn- pelems->specter [pelem next-pelem]
  (cond
    (vector? pelem) (vec->specter pelem next-pelem)
    (map? pelem) (map->specter pelem)
    (keyword? pelem) [pelem]))

(defn path->sp-path [path]
  (reduce
   (fn [acc [pelem next-pelem]]
     (concat acc (pelems->specter pelem next-pelem)))
   []
   (partition 2 1 [nil] path)))

(defn get-values [data path]
  (let [sp-path (path->sp-path path)]
    (if (has-wildcards? path)
      (sp/select sp-path data)
      [(sp/select sp-path data)])))

(defn- path->value [values]
  (->>
   (for [value values]
     [(butlast value) (last value)])
   (into {})))

(defn set-values [data path values]
  (let [values  (path->value values)
        sp-path (path->sp-path path)]
    (sp/transform
     sp-path
     (fn [& v]
       (let [path  (butlast v)
             value (last v)]
         (get values path value)))
     data)))

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

(defn- counting-sp-path [path]
  (let [[_ vfilter] (last path)]
    (concat
     (path->sp-path (butlast path))
     (if vfilter [(sp/filterer (get-vfilter-fn vfilter))] []))))

(defn- count-matched-nodes [data path]
  (let [res (sp/select (counting-sp-path path) data)]
    (if (has-wildcards? (butlast path))
      (mapv #(count (last %)) res)
      [(count (first res))])))

(defn- tree-shape [data path]
  (map #(count-matched-nodes data %)
       (splitcat-by-wildcard path)))

(defn- count-shape-diff [source-shape sink-shape]
  (map #(map - %1 (concat %2 (repeat 0)))
       source-shape
       sink-shape))

(defn- add-level-elems [acc path elem ncounts]
  (let [wc?        (wildcard? (last path))
        first-part (butlast path)
        filter     (if wc? (last (last path)))

        insert-path
        (path->sp-path
         (if wc? first-part path))

        select-path
        (if wc?
          (concat
           (path->sp-path (butlast path))
           [(sp/filterer filter)])
          path)

        insert-fn
        (into {} (mapv (fn [[& args] ncount]
                         [(butlast args) (repeat ncount elem)])
                       (sp/select select-path acc)
                       ncounts))]

    (sp/transform
     [insert-path sp/END]
     (fn [& args]
       (insert-fn (butlast args)))
     acc)))

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

;; (defn attach-parsers [mapping parsers]
;;   (assoc mapping :parsers (merge default-parsers parsers)))

(defn- ih? [x]
  (= "ih" (namespace x)))

(defn- get-full-path [data path]
  (if (ih? (first path))
    path
    (into [:ih/data data] path)))

(defn- transform-once [ctx rule]
  (let [[from to] (or (:ih/direction rule) (:ih/direction ctx))

        {source-path from
         sink-path   to} rule

        full-source-path (get-full-path from source-path)
        full-sink-path   (get-full-path to sink-path)

        default-path (get-in rule [:ih/defaults to])

        get-path (if source-path full-source-path default-path)

        values (get-values ctx get-path)]

    (if (and values sink-path)
      (set-values
       (add-templates ctx ctx [full-source-path full-sink-path])
       full-sink-path
       values)
      ctx)))

(defn transform [source-data sink-data mapping [from to]]
  (reduce
   (fn [acc {source-path        from
            sink-path          to
            :as                rule}]
     (let [values (get-values source-data source-path)]
       (if (and values sink-path)
         (set-values
          (add-templates source-data acc [source-path sink-path])
          sink-path
          values)
         acc)))
   sink-data
   mapping))



(defn- special? [ns key x]
  (or
   (and (keyword? x) (= ns (namespace x)))
   (and (map? x) (contains? x key))))

(defn- micro? [x]
  (special? "ihm" :ih/micro))

(defn- parser? [x]
  (special? "ihp" :ih/parser))

(defn- microexpand [micromacro values]
  (let [keys-set (set (keys values))]
    (sp/transform [(sp/walker #(keys-set %))] values micromacro)))

;; (microexpand [:telecom [:%1] :value] {:%1 :*})
;; => [:telecom [:*] :value]

;; (defn expand-alias [ctx pelem]
;;   (let [ (::ih/micro)])
;;   )



(defn- do-it [{[from to] :ih/direction
               data      :ih/data
               micros    :ih/micros
               :as       ctx}]
  (let [])
  )


(def transformation
  #:ih{:micros #:ihm {:telecom [:telecom [:%1] :value]
                      :name    [:name [0]]}

       :data {:form {:name "Test Name"}
              :fhir {}}

       :values {:patient-id "Patient/UUID"} ;; should be attacheable

       :direction [:form :fhir]

       :rules
       [{:form        [:name]
         :ih/defaults {:fhir [:ih/values :patient-id]
                       :form [:ih/values :patient-id]}
         :fhir        [:name]}]})


;; #spy/p
;; (transform-once transformation (first (:ih/rules transformation)))



