(ns examples.membrane
  (:require
   [example.tutorial :refer [examples]]
   [membrane.ui :as ui]
   [membrane.basic-components :as basic]
   [membrane.component :as component :refer [defui defeffect]]
   [io.github.dundalek.compost.core :as cc]
   [io.github.dundalek.compost :as cu]
   [io.github.dundalek.compost.svg :as csvg]
   [membrane.skia :as skia]
   [hiccup2.core :as h]))

(defn render-membrane [ctx shape]
  (case (first shape)
    ::cc/Path
    (let [[_ path style] shape
          [start & other] path]
      ;; To simplify for now assuming there is only one MoveTo at the start and then only LineTos
      ;; If we get multiple MoveTo's we'll have to implement it properly
      (apply
       ui/path
       (let [[kw [x y]] start]
         (assert (= kw ::cc/MoveTo))
         [x y])
       (for [[kw [x y]] other]
         (do
           (assert (= kw ::cc/LineTo))
           [x y]))))))

(defui graph [{}]
  (ui/with-style :membrane.ui/style-stroke
    (ui/with-stroke-width 3
      (ui/with-color #_[1 0 0] [0 0 1]
        (apply
         ui/path
         (for [i (range 10)]
           [(* 30 i)
            (if (even? i) 0 30)]))))))

;; Display a "more!" button next to label showing num
;; clicking on "more!" will dispatch a ::counter-increment effect
(defui counter [{:keys [num]}]
  (ui/horizontal-layout
   (ui/on :mouse-down (fn [_]
                        [[::counter-increment $num]])
          (ui/button "more!"))
   (ui/label num)))

(defeffect ::counter-increment [$num]
  (dispatch! :update $num inc))

(defui compost [_]
  (let [width 600
        height 300
        viz [:line [[1 1] [2 4] [3 9] [4 16] [5 25] [6 36]]]
        defs-clj (atom [])
        draw-ctx-clj {::cc/definitions defs-clj ::cc/style cc/defstyle}
        render-ctx-clj {}
        viz-clj (cu/from-hiccup-crossplatform viz)
        [[sx-clj sy-clj] shape-clj] (cc/calculate-scales cc/defstyle viz-clj)
        shape-clj (cc/draw-shape draw-ctx-clj 0 0 width height sx-clj sy-clj shape-clj)]
    (ui/with-style :membrane.ui/style-stroke
      (ui/with-stroke-width 3
        (ui/with-color [1 0 0]
          (render-membrane render-ctx-clj shape-clj))))))

(defui compost-svg [{:keys [viz width height]}]
  (let [width 600
        height 300
        defs-clj (atom [])
        draw-ctx-clj {::cc/definitions defs-clj ::cc/style cc/defstyle}
        render-ctx-clj {}
        viz-clj (cu/from-hiccup-crossplatform viz)
        [[sx-clj sy-clj] shape-clj] (cc/calculate-scales cc/defstyle viz-clj)
        shape-clj (cc/draw-shape draw-ctx-clj 0 0 width height sx-clj sy-clj shape-clj)
        body (csvg/render-svg render-ctx-clj shape-clj)
        svg (into [:svg {:style "overflow:visible"
                         :width width
                         :height height}]
                  body)]
    (skia/svg (.getBytes (str (h/html svg))
                         "utf-8")
              [width height])))

(defui svg [{:keys [path]}]
  (skia/svg (.getBytes (slurp path) "utf-8")))

(defui app [_]
  (let [body (apply ui/vertical-layout
                    (->> examples
                         (mapcat (fn [[label viz]]
                                   [(ui/label label)
                                    (compost-svg {:viz viz
                                                  :width 600
                                                  :height 300})]))))
        container-size [800 800]

        body (if container-size
               (basic/scrollview
                {:body body
                 :$body nil

                 :scroll-bounds container-size})
               body)]
    body))
  ; (ui/vertical-layout))
   ; (counter {:num 10})
   ; (graph {})
   ; (compost-svg {:viz [:line [[1 1] [2 4] [3 9] [4 16] [5 25] [6 36]]]})
   ; (compost-svg {:viz [:axes "left bottom"
   ;                     [:overlay
   ;                      [[:fill-color "#2CA02C" [:column "Positive" 39]]
   ;                       [:fill-color "#D62728" [:column "Negative" 43]]
   ;                       [:fill-color "#1F77B4" [:column "Neutral" 17]]]]]})))
   ; (svg {:path "/home/me/Downloads/text.svg"})
   ; (svg {:path "/home/me/Downloads/text-simple.svg"})
   ; (svg {:path "/home/me/Downloads/x.svg"})))

(comment
  (skia/run (component/make-app #'app {})))
            ; {:include-container-info true
            ;  :window-title "Easel"})
