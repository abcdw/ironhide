(ns ih-demo.core
  (:require [ironhide.core :as ih]
            [reagent.core :as r]
            [cljs.reader :refer [read-string]]
            [cljs.pprint]
            [com.rpl.specter :as sp]
            ))

(def init-shell
  #:ih{:rules [{:left [:name :ihs/str<->vector [0]]
                :right [:name [0] :given [0]]}
               {:left [:name :ihs/str<->vector [1]]
                :right [:name [0] :family]}]
       :direction [:left :right]
       :data  {:left {:name "Firstname Lastname"}
               :right {}}})

(def shells
  {:sight-for-string init-shell

   :field-to-field-mapping
   #:ih{:direction [:left :right]

        :rules [{:left   [:name]
                 :right [:fullname]}]
        :data  {:left   {:name "Full Name"}
                :right {:fullname "Old Name"}}}

   :create-name
   #:ih{:direction [:left :right]

        :rules [{:left   [:name]
                 :right [:fullname]}]
        :data  {:left {:name "Full Name"}}}
   :update-and-create-phone

   #:ih{:direction [:left :right]

        :rules [{:left [:phones [:*]]
                 :right [:telecom [:* {:system "phone"}] :value]}]
        :data  {:left {:phones ["+1 111" "+2 222"]}
                :right {:telecom [{:system "phone"
                                  :use    "home"
                                  :value  "+3 333"}
                                 {:system "email"
                                  :value  "test@example.com"}]}}}

   :micro-example
   #:ih{:direction [:right :left] ;; !!!

        :micros #:ihm {:name<->vector [:name {:ih/sight  :ihs/str<->vector
                                              :separator ", "}]}

        :rules [{:left [:ihm/name<->vector [0]]
                 :right [:name [0] :given [0]]}
                {:left [:ihm/name<->vector [1]]
                 :right [:name [0] :family]}]
        :data  {:left {:name "Full, Name"}
                :right {:name [{:given ["First"] :family "Family"}]}}}
   })

(defn cljs-pp [x]
  (with-out-str (cljs.pprint/pprint x)))

(defonce db (r/atom {}))

(defn load-from-template [shell]
  (let [test-shell shell
        ;; (ih/execute shell)
        ]
    (reset! db
            {:left  {:str   (cljs-pp (get-in test-shell [:ih/data :left]))
                     :value (get-in test-shell [:ih/data :left])}
             :right {:str   (cljs-pp (get-in test-shell [:ih/data :right]))
                     :value (get-in test-shell [:ih/data :right])}
             :shell {:str   (cljs-pp (dissoc test-shell :ih/data :ih/direction))
                     :value (dissoc test-shell :ih/data :ih/direction)}})))

(defn print-shell! []
  (println
   (ih/execute #:ih{:rules [{:left [[:* {:a :b}] :c]
                             :right [[:*]]}]
                    :direction [:right :left]
                    :data {:right [1 2 3]}}))
  (println
   (ih/set-values [{:a :c}] [[:* {:a :c}] :d] [[0 :test]]))
  ;; (println
  ;;  (sp/select [:a :b] {:a {:b :c}}))
  )

(defn compute-everything! [source sink]
  (let [everything (-> (get-in @db [:shell :value])
                       (assoc-in [:ih/data source] (get-in @db [source :value]))
                       (assoc-in [:ih/data sink] (get-in @db [sink :value]))
                       (assoc :ih/direction [source sink])
                       ih/execute)
        value      (get-in everything [:ih/data sink])]

    (swap! db assoc-in [sink :value] value)
    (swap! db assoc-in [sink :str] (cljs-pp value))))

(defn prettify-db [db]
   (sp/transform [sp/MAP-VALS (sp/collect-one :value) :str] (fn [a _] (cljs-pp a)) db))

(defn input-comp [name label col-cl]
  (fn []
    [:div {:class col-cl}
     [:label label]
     [:textarea.form-control
      {:value (get-in @db [name :str])
       :class (when (get-in @db [name :error]) "is-invalid")
       :style {:height      "250px"
               :font-family "monospace"}

       :on-change #(let [value (-> % .-target .-value)]
                     (swap! db assoc-in [name :str] value)
                     (try
                       (swap! db assoc-in [name :value] (read-string value))
                       (swap! db update-in [name] dissoc :error)
                       (case name
                         :left  (compute-everything! :left :right)
                         :right (compute-everything! :right :left)
                         :true)
                       (catch :default e
                         (swap! db assoc-in [name :error] (.-message e)))))}]

     [:div.invalid-feedback (get-in @db [name :error])]]))

(defn home-page []
  [:div.container
   [:br]
   [:div.row
    [:div.col-sm-4
     [:h2 [:a {:href "https://github.com/healthsamurai/ironhide"}
           "Ironhide"] " demo"]]
    [:div.col-sm-8
     [:select.custom-select
      {:on-change #(load-from-template
                    (get shells (-> % .-target .-value keyword)))}
      (for [[k v] shells]
        ^{:key k} [:option {:value k} k])]]]
   [:form
    [:div.form-row
     [input-comp :left "Left data:" "col-sm-6"]
     [input-comp :right "Right data:" "col-sm-6"]]

    [:div.form-row
     [input-comp :shell "Shell:" "col-sm-12"]

     ;; [:div.col-sm-2
     ;;  [:a.btn.btn-block.btn-danger
     ;;   {:on-click #(print-shell!)} "Update rules"]]
     ]

    [:div.form-row
     [:div.col-sm-12
      [:a.btn.btn-block.btn-success
       {:on-click #(swap! db prettify-db)} "Prettify"]]
     ]]])

(defn ^:export run []
  (load-from-template (:sight-for-string shells))
  (r/render [home-page]
            (js/document.getElementById "app")))

(run)

