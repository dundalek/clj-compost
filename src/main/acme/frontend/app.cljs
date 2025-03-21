(ns acme.frontend.app
  (:require
   ["compostjs/dist/core" :refer [Scales$$$calculateScales Compost$$$defstyle Compost$$$createSvg Drawing$$$drawShape Drawing$002EDrawingContext Svg$$$renderSvg Svg$002ERenderingContext Svg$$$formatPath]]
   ["compostjs/dist/fable-library.2.10.1/Types" :refer [Union List]]
   [acme.compost :as c]
   [clojure.string :as str]
   [hiccups.runtime :as hr]
   [acme.compost.core-new :as cc]))

(defn union? [x]
  (instance? Union x))

(defn iterator? [x]
  (and (object? x) (fn? (aget x js/Symbol.iterator))))

(defn union->clj [x]
  (cond
    (union? x) (with-meta
                 (into
                  [(keyword "acme.compost.core-new" (.-name x))]
                  (map union->clj)
                  (.-fields x))
                 {::cc/original x})

    (instance? List x) (map union->clj x)

    (iterator? x) (map union->clj x)

    (array? x) (.map x union->clj)
    #_(with-meta
        (mapv union->clj x)
        {::cc/original x})

    :else x))

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

(defn create-svg [rev-x rev-y width height viz]
  (js/console.log "viz" viz)
  (js/console.log "viz-clj" (union->clj viz))
  #_(element->hiccup (Compost$$$createSvg rev-x rev-y width height viz))
  (let [;; calculateScales
        [[sx sy] shape-fs] (Scales$$$calculateScales Compost$$$defstyle viz)
        _ (js/console.log "shape-fs" shape-fs)
        _ (js/console.log "shape-fs-clj" (union->clj shape-fs))
        _ (js/console.log "sx-fs-clj" (union->clj sx))
        [[sx-clj sy-clj] shape-clj] (cc/calculate-scales {} (union->clj viz))
        _ (js/console.log "shape-clj" shape-clj)
        _ (js/console.log "sx-clj" sx-clj)
        ;; drawShape
        defs #js []
        draw-ctx (Drawing$002EDrawingContext. Compost$$$defstyle defs)
        svg-fs (Drawing$$$drawShape draw-ctx 0 0 width height sx sy shape-fs)
        _ (js/console.log "svg-fs" svg-fs)
        _ (js/console.log "svg-fs-clj" (union->clj svg-fs))
        ;; it seems definitions are used for animations and gradients definitions
        defs-clj (atom [])
        draw-ctx-clj {::cc/definitions defs-clj ::cc/style cc/defstyle}
        ; svg-clj (cc/draw-shape draw-ctx-clj 0 0 width height (union->clj sx) (union->clj sy) (union->clj shape-fs))
        svg-clj (cc/draw-shape draw-ctx-clj 0 0 width height sx-clj sy-clj shape-clj)
        _ (js/console.log "svg-clj" svg-clj)
        ;; renderSvg
        render-ctx (Svg$002ERenderingContext. defs)
        ; (->> (Svg$$$renderSvg ctx svg
        ;       (map element->hiccup)))
        body (cc/render-svg render-ctx svg-clj)]
        ; body (cc/render-svg render-ctx (union->clj svg-fs))]
    (into [:svg {:style "overflow:visible"
                 :width width
                 :height height}]
          body)))

(defn render [id viz]
  (let [svg (create-svg false false 600 300 viz)
        el (js/document.getElementById id)]
    (set! (.-innerHTML el) (hr/render-html svg))))

(def examples
  [["line"
    (c/line [[1 1] [2 4] [3 9] [4 16] [5 25] [6 36]])]
   ["out1a"
    (c/column "Positive" 39)]
   ["out1b"
    (c/overlay
     [(c/column "Positive" 39)
      (c/column "Negative" 43)
      (c/column "Neutral" 17)])]])

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

  #_(let [d (c/axes "left bottom"
                    (c/line [[1 1] [2 4] [3 9] [4 16] [5 25] [6 36]]))]
      (cj/render "demo" d))

  #_(let [d (c/line [[1 1] [2 4] [3 9] [4 16] [5 25] [6 36]])]
      (render "demo" d))

  ; (js/console.log "clj" (union->clj (c/column "Positive" 39)))

  #_(js/console.log "project-single"
                    (cc/project-one false [0 500]
                                    [::cc/Categorical [[::cc/CA "Positive"]]]
                                    [::cc/CAR [::cc/CA "Positive"] 1]))

  (set! (.-innerHTML (js/document.getElementById "demo"))
        (->>
         examples
         ; [(last examples)]
         (map (fn [[label viz]]
                (hr/render-html
                 [:div
                  [:h2 label]
                  (create-svg false false 500 200 viz)])))
         (str/join "\n"))))
