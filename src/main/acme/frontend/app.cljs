(ns acme.frontend.app
  (:require
   ["compostjs/dist/core" :refer [Compost$$$defstyle Drawing$$$drawShape
                                  Drawing$002EDrawingContext
                                  Scales$$$calculateScales Svg$$$renderSvg
                                  Svg$002ERenderingContext]]
   [acme.compost :as c]
   [acme.compost.core-new :as cc :refer [union->clj union?]]
   [acme.fish :as fish]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [hiccups.runtime :as hr]
   [acme.compost.util :refer [from-hiccup from-hiccup-crossplatform]]))

(defn element->hiccup [el]
  (assert (union? el))
  (case (.-name el)
    "Element"
    (let [[namespace tag attrs children] (.-fields el)]
      (into [(keyword tag)
             (->> attrs
                  (keep (fn [[k v]]
                         ;; selecting only Attributes, ignoring other types like Event for now
                          (when (and (union? v)
                                     (= (.-name v) "Attribute"))
                            [(keyword k) (first (.-fields v))])))
                  (into {}))]
            (map element->hiccup children)))

    "Text"
    (let [[text] (.-fields el)]
      text)))

(defn from-hiccup-fs [viz]
  (from-hiccup c/kw->constructor viz))

(def kw->constructor-clj
  (merge
   c/kw->constructor
   {:fill-color cc/fill-color
    :stroke-color cc/stroke-color
    :overlay (fn [sh] [::cc/Layered sh])
    :axes (fn [a s]
            [::cc/Axes
             (str/includes? a "top") (str/includes? a "right") (str/includes? a "bottom") (str/includes? a "left")
             s])}))

(defn from-hiccup-clj [viz]
  (->>
   (from-hiccup kw->constructor-clj viz)
   (walk/prewalk union->clj)))

(defn create-svg [rev-x rev-y width height viz]
  (js/console.log "viz" viz)
  (let [viz-fs (from-hiccup-fs viz)
        _ (js/console.log "viz-fs" viz-fs)
        [[sx sy] shape-fs] (Scales$$$calculateScales Compost$$$defstyle viz-fs)
        _ (js/console.log "shape-fs" shape-fs)
        _ (js/console.log "shape-fs-clj" (union->clj shape-fs))
        defs #js []
        draw-ctx (Drawing$002EDrawingContext. Compost$$$defstyle defs)
        svg-fs (Drawing$$$drawShape draw-ctx 0 0 width height sx sy shape-fs)
        render-ctx (Svg$002ERenderingContext. defs)
        _ (js/console.log "svg-fs" svg-fs)
        _ (js/console.log "svg-fs-clj" (union->clj svg-fs))
        body (->> (Svg$$$renderSvg render-ctx svg-fs)
                  (map element->hiccup))
        ;; ===

        sx-clj (union->clj sx)
        sy-clj (union->clj sy)
        shape-clj (union->clj shape-fs)
        svg-clj (union->clj svg-fs)
        viz-clj (from-hiccup-clj viz)
        _ (js/console.log "viz-clj" viz-clj)
        viz-clj (from-hiccup-crossplatform viz)
        _ (js/console.log "viz-clj-crossplatform" viz-clj)
        defs-clj (atom [])
        draw-ctx-clj {::cc/definitions defs-clj ::cc/style cc/defstyle}
        render-ctx-clj {}

        ;; ===

        [[sx-clj sy-clj] shape-clj] (cc/calculate-scales cc/defstyle viz-clj)
        svg-clj (cc/draw-shape draw-ctx-clj 0 0 width height sx-clj sy-clj shape-clj)

        _ (js/console.log "viz-clj" viz-clj)
        _ (js/console.log "shape-clj" shape-clj)
        _ (js/console.log "svg-clj" svg-clj)

        body (cc/render-svg render-ctx-clj svg-clj)]
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
    [:line [[1 1] [2 4] [3 9] [4 16] [5 25] [6 36]]]]
   ["axes-simple"
    [:axes "left bottom"
     [:column "Positive" 39]]]
   ["out1a"
    [:column "Positive" 39]]
   ["out1b"
    [:overlay
     [[:column "Positive" 39]
      [:column "Negative" 43]
      [:column "Neutral" 17]]]]
   ["out2a"
    [:overlay
     [[:fill-color "#2CA02C" [:column "Positive" 39]]
      [:fill-color "#D62728" [:column "Negative" 43]]
      [:fill-color "#1F77B4" [:column "Neutral" 17]]]]]
   ["out2b"
    [:axes "left bottom"
     [:overlay
      [[:fill-color "#2CA02C" [:column "Positive" 39]]
       [:fill-color "#D62728" [:column "Negative" 43]]
       [:fill-color "#1F77B4" [:column "Neutral" 17]]]]]]
   ["out3a"
    [:axes "left bottom"
     [:overlay
      [[:fill-color "#2CA02C"
        [:shape [[["Positive" 0] 0] [["Positive" 1] 0]
                 [["Positive" 1] 39] [["Positive" 0] 39]]]]
       [:fill-color "#D62728"
        [:shape [[["Negative" 0] 0] [["Negative" 1] 0]
                 [["Negative" 1] 43] [["Negative" 0] 43]]]]]]]]
   ["out3b"
    [:axes "left bottom"
     [:overlay
      [[:fill-color "#2CA02C"
        [:shape [[0 ["Positive" 0]] [0 ["Positive" 1]]
                 [39 ["Positive" 1]] [39 ["Positive" 0]]]]]
       [:fill-color "#D62728"
        [:shape [[0 ["Negative" 0]] [0 ["Negative" 1]]
                 [43 ["Negative" 1]] [43 ["Negative" 0]]]]]]]]]
   ["out4a"
    [:axes "left bottom"
     [:overlay
      [[:fill-color "#2CA02C"
        [:shape [[["Positive" 0] 0] [["Positive" 1] 0]
                 [["Positive" 1] 39] [["Positive" 0] 39]]]]
       [:stroke-color "black"
        [:line [[["Positive" 0.5] 32] [["Positive" 0.5] 46]]]]
       [:fill-color "#D62728"
        [:shape [[["Negative" 0] 0] [["Negative" 1] 0]
                 [["Negative" 1] 43] [["Negative" 0] 43]]]]
       [:stroke-color "black"
        [:line [[["Negative" 0.5] 32] [["Negative" 0.5] 46]]]]]]]]
   ["bubbles"
    [:overlay
     (for [[x y] [[1 1] [2 4] [3 9] [4 16] [5 25] [6 36]]]
       [:bubble x y 1 1])]]
   ["fish"
    [:stroke-color "green" fish/fish]]])

(defn draw-canvas [ctx width height viz]
  (let [defs-clj (atom [])
        draw-ctx-clj {::cc/definitions defs-clj ::cc/style cc/defstyle}
        viz-clj (from-hiccup-clj viz)
        [[sx-clj sy-clj] shape-clj] (cc/calculate-scales cc/defstyle viz-clj)
        svg-clj (cc/draw-shape draw-ctx-clj 0 0 width height sx-clj sy-clj shape-clj)]
    (js/console.log "svg-clj" svg-clj)
    (cc/render-canvas ctx svg-clj)))

(def example-width 500)
(def example-height 200)

(defn render-compost-examples []
  (let [container (.getElementById js/document "compost-examples")]
    (set! (.-innerHTML container) "")
    (.appendChild container
                  (let [heading (.createElement js/document "h1")]
                    (set! (.-innerText heading) "Js SVG")
                    heading))
    (doseq [[label viz] (->> examples
                             reverse)]
      (let [div (.createElement js/document "div")
            id (str "compost-example-" label)
            _ (.appendChild container
                            (doto (.createElement js/document "div")
                              (.appendChild
                               (doto (.createElement js/document "h2")
                                 (set! -innerText label)))
                              (.appendChild div)))]
        (set! (.-id div) id)
        (set! (.-style.width div) (str example-width "px"))
        (set! (.-style.height div) (str example-height "px"))
        (c/render id (from-hiccup-fs viz))))))

(defn render-canvas-examples []
  (let [container (.getElementById js/document "canvas-examples")]
    (set! (.-innerHTML container) "")
    (.appendChild container
                  (let [heading (.createElement js/document "h1")]
                    (set! (.-innerText heading) "Clj Canvas")
                    heading))
    (doseq [[label viz] (->> examples
                             reverse)]
      (let [canvas (.createElement js/document "canvas")
            _ (.appendChild container
                            (doto (.createElement js/document "div")
                              (.appendChild
                               (doto (.createElement js/document "h2")
                                 (set! -innerText label)))
                              (.appendChild canvas)))
            ctx (.getContext canvas "2d")]

        (set! (.-width canvas) example-width)
        (set! (.-height canvas) example-height)
        (.clearRect ctx 0 0 (.-width canvas) (.-height canvas))
        (set! (.-strokeStyle ctx) "#FF0000")
        (set! (.-lineWidth ctx) 2)

        (draw-canvas ctx example-width example-height viz)))))

(defn render-svg-examples []
  (let [container (js/document.getElementById "svg-examples")]
    (set! (.-innerHTML container) "")
    (set! (.-innerHTML container)
          (->>
           examples
           ; [(last examples)]
           reverse
           (map (fn [[label viz]]
                  [:div
                   [:h2 label]
                   (create-svg false false example-width example-height viz)]))
           (cons [:h1 "Clj SVG"])
           (map hr/render-html)
           (str/join "\n")))))

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

  (render-compost-examples)
  (render-svg-examples)
  (render-canvas-examples))


