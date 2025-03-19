(ns acme.frontend.app
  (:require
   ["compostjs" :refer [compost] :rename {compost c}]
   [acme.compost :as cj]))

#_(def d (c/axes "left bottom"
                 (c/line [[1 1] [2 4] [3 9] [4 16] [5 25] [6 36]])))

(defn ^:export init []
  (println "Hello World")

  ; (c/render "root" d)

  (let [d (.axes c "left bottom"
                 (.line c (clj->js [[1 1] [2 4] [3 9] [4 16] [5 25] [6 36]])))]
    (.render c "demo" d))

  (let [demo (.axes c "left bottom"
                    (.overlay c #js [(.fillColor c "#2CA02C" (.column c "Positive" 39))
                                     (.fillColor c "#D62728" (.column c "Negative" 43))
                                     (.fillColor c "#1F77B4" (.column c "Neutral" 17))]))]
    (.render c "demo" demo)))

(let [d (cj/axes "left bottom"
                 (cj/line [[1 1] [2 4] [3 9] [4 16] [5 25] [6 36]]))]
  (cj/render "demo" d))

(let [demo (cj/axes "left bottom"
                    (cj/overlay [(cj/fill-color "#2CA02C" (cj/column "Positive" 39))
                                 (cj/fill-color "#D62728" (cj/column "Negative" 43))
                                 (cj/fill-color "#1F77B4" (cj/column "Neutral" 17))]))]
  (cj/render "demo" demo))
