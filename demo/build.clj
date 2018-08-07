(require '[cljs.build.api :as api]
         '[clojure.string :as string])

(def source-dir "src")

(def compiler-config
  {:main          "ih-demo.core"
   :output-to     "build/js/app.js"
   ;; :source-map    "build/js/app.js.map"
   :output-dir    "build/js/out"
   ;; :infer-externs true
   :optimizations :advanced
   ;; :externs ["resources/stripe-externs.js"
   ;;           "resources/codemirror-externs.js"]
   })

(def dev-config
  (merge compiler-config
         {:optimizations :none
          :source-map    true}))

(defmulti task first)

(defmethod task :default [_]
  (task ["build"]))

(defmethod task "build" [_]
  (api/build source-dir compiler-config))

(task *command-line-args*)
