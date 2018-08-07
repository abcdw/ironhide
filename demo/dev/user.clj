(ns dev (:require [figwheel-sidecar.repl-api :as repl]))

(def nrepl-options
  {:nrepl-port       7890
   :nrepl-middleware ["cider.nrepl/cider-middleware"
                      "refactor-nrepl.middleware/wrap-refactor"
                      "cemerick.piggieback/wrap-cljs-repl"]})

(def figwheel-options
  {:figwheel-options (merge nrepl-options {:css-dirs ["resources/public/css"]})
   :all-builds       [{:id "app"
                       :source-paths ["src"]
                       :compiler
                       {:main "ih-demo.core"
                        :asset-path "/js/out"
                        :output-to "resources/public/app.js"
                        :output-dir "resources/public/js/out"
                        :source-map true
                        :optimizations :none
                        :pretty-print  true}}]})

(def ensure-nrepl-port! #(spit ".nrepl-port" (:nrepl-port nrepl-options)))

(defn start []
  (ensure-nrepl-port!)
  (repl/start-figwheel! figwheel-options)
  (repl/cljs-repl))

(comment
  (start)

  (repl/start-figwheel! figwheel-options)

  )
