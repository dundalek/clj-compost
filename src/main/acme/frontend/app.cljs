(ns acme.frontend.app
  (:require
   ["compostjs" :refer [compost] :rename {compost c}]
   ["compostjs/dist/core" :refer [Scales$$$calculateScales Compost$$$defstyle Compost$$$createSvg]]
   ["compostjs/dist/fable-library.2.10.1/Types" :refer [Union]]
   [acme.compost :as cj]))

(defn union? [x]
  (instance? Union x))

(defn element->hiccup [el]
  (assert (union? el))
  (assert (= (.-name el) "Element"))
  (let [[namespace tag attrs children] (.-fields el)]
    (into [(keyword tag)
           (->> attrs
                (keep (fn [[k v]]
                        ;; selecting only Attributes, ignoring other types like Event for now
                        (when (and (union? v)
                                   (= (.-name v) "Attribute"))
                          [(keyword k) (first (.-fields v))])))
                (into {}))]
          (map element->hiccup children))))

(defn ^:export ^:dev/after-load init []
  #_(let [d (.axes c "left bottom"
                   (.line c (clj->js [[1 1] [2 4] [3 9] [4 16] [5 25] [6 36]])))]
      (.render c "demo" d))

  #_(let [demo (.axes c "left bottom"
                      (.overlay c #js [(.fillColor c "#2CA02C" (.column c "Positive" 39))
                                       (.fillColor c "#D62728" (.column c "Negative" 43))
                                       (.fillColor c "#1F77B4" (.column c "Neutral" 17))]))]
      (.render c "demo" demo))

  #_(let [demo (cj/axes "left bottom"
                        (cj/overlay [(cj/fill-color "#2CA02C" (cj/column "Positive" 39))
                                     (cj/fill-color "#D62728" (cj/column "Negative" 43))
                                     (cj/fill-color "#1F77B4" (cj/column "Neutral" 17))]))]
      (cj/render "demo" demo))

  #_(let [d (cj/axes "left bottom"
                     (cj/line [[1 1] [2 4] [3 9] [4 16] [5 25] [6 36]]))]
      (cj/render "demo" d))

  (let [d (cj/line [[1 1] [2 4] [3 9] [4 16] [5 25] [6 36]])]
    (cj/render "demo" d)

    (let [svg (Compost$$$createSvg false false 600 300 d)]
      (prn (element->hiccup svg)))

    #_(let [[[sx sy] shape] (Scales$$$calculateScales Compost$$$defstyle d)]
        (js/console.log "shape" shape))))
