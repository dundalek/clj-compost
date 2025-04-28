(ns io.github.dundalek.compost.core
  (:require
   [clojure.string :as str]
   #?(:cljs [goog.string :refer [format]])
   #?(:cljs ["compostjs/dist/fable-library.2.10.1/Types" :refer [List Union]])))

#?(:clj
   (defn style-fs->clj [style]
     style)
   :cljs
   (do
     (defn union? [x]
       (instance? Union x))

     (defn iterator? [x]
       (and (object? x) (fn? (aget x js/Symbol.iterator))))

     (defn union->clj [x]
       (cond
         (union? x) (-> (into
                         [(keyword "io.github.dundalek.compost.core" (.-name x))]
                         (map union->clj)
                         (.-fields x))
                        #_(with-meta
                            {::original x}))

         (instance? List x) (map union->clj x)

         (iterator? x) (map union->clj x)

         (array? x) (mapv union->clj x)

         #_(with-meta
             (mapv union->clj x)
             {::original x})

         :else x))

     (defn style-fs->clj [style]
       (if (map? style)
         style
         {::fill (union->clj (.-Fill style))
          ::stroke-color (union->clj  (.-StrokeColor style))
          ::stroke-dash-array (union->clj (.-StrokeDashArray style))
          ::stroke-width (union->clj (.-StrokeWidth style))
          ::animation (.-Animation style)
          ::cursor (.-Cursor style)
          ::font (.-Font style)
          ::format-axis-x-label (.-FormatAxisXLabel style)
          ::format-axis-y-label (.-FormatAxisYLabel style)}))))

;; ------------------------------------------------------------------------------------------------
;; Domain that users see
;; ------------------------------------------------------------------------------------------------

;; ------------------------------------------------------------------------------------------------
;; SVG stuff
;; ------------------------------------------------------------------------------------------------

(declare format-color)

(defn apply-canvas-style [ctx style]
  (let [style-clj (style-fs->clj style)
        {::keys [animation cursor font stroke-color stroke-width fill text-vertical text-horizontal]} style-clj]
    (assert (nil? animation) "Animation not implemented")
    (assert (= cursor "default") "Non-default cursor not implemented")
    (set! (.-font ctx) font)
    (let [[so clr] stroke-color
          [_ sw] stroke-width]
      (set! (.-strokeStyle ctx) (format-color clr))
      (set! (.-lineWidth ctx) sw)
      (when (not= so 0.0)
        (.stroke ctx)))
    (case (first fill)
      ::Solid (let [[_ [fo clr]] fill]
                (set! (.-fillStyle ctx) (format-color clr))
                (when (not= fo 0.0)
                  (.fill ctx))))
    (when text-vertical
      (let [va (case (first text-vertical)
                 ;; not sure if "baseline" from SVG maps well, trying alphabetic because it is the default for canvas
                 ::Baseline "alphabetic"
                 ::Hanging "hanging"
                 ::Middle "middle")]
        (set! (.-textBaseline ctx) va)))
    (when text-horizontal
      (let [ha (case (first text-horizontal)
                 ::Start "start"
                 ::Center "center"
                 ::End "end")]
        (set! (.-textAlign ctx) ha)))))

(defn render-canvas [ctx shape]
  (case (first shape)
    ::Text
    (let [[_ [x y] t rotation style] shape]
      (.save ctx)
      (apply-canvas-style ctx style)
      (if (= rotation 0.0)
        (.fillText ctx t x y)
        (throw (ex-info "Rotation not implemented yet" {})))
      (.restore ctx))

    ::Combine
    (let [[_ ss] shape]
      (doseq [s ss]
        (render-canvas ctx s)))

    ::Ellipse
    (let [[_ [cx cy] [rx ry] style] shape]
      (.save ctx)
      (.beginPath ctx)
      (.ellipse ctx cx cy rx ry 0 0 (* 2 Math/PI))
      (apply-canvas-style ctx style)
      (.restore ctx))

    ::Path
    (let [[_ path style] shape]
      (.save ctx)
      (.beginPath ctx)
      (doseq [segment path]
        (case (first segment)
          ::MoveTo (let [[_ [x y]] segment]
                     (.moveTo ctx x y))
          ::LineTo (let [[_ [x y]] segment]
                     (.lineTo ctx x y))))

      (apply-canvas-style ctx style)
      (.restore ctx))))

;; ------------------------------------------------------------------------------------------------
;; Calculating scales
;; ------------------------------------------------------------------------------------------------

(defn union-scales [s1 s2]
  (case [(first s1) (first s2)]
    [::Continuous ::Continuous]
    (let [[_ [_ l1] [_ h1]] s1
          [_ [_ l2] [_ h2]] s2]
      [::Continuous [::CO (min l1 l2)] [::CO (max h1 h2)]])

    [::Categorical ::Categorical]
    (let [[_ v1] s1
          [_ v2] s2]
      [::Categorical (distinct (concat v1 v2))])

    (throw (ex-info "Cannot union continuous with categorical" {}))))

(defn calculate-shape-scale [vs]
  ;; TODO: implement mismatched scales checking
  (let [first-val (first vs)]
    (if (= (first first-val) ::COV)
      ;; For continuous values (COV), find min and max
      [::Continuous
       [::CO (->> vs (map (fn [[_ [_ v]]] v)) (apply min))]
       [::CO (->> vs (map (fn [[_ [_ v]]] v)) (apply max))]]
      ;; For categorical values (CAR), collect unique categories
      [::Categorical
       (->> vs
            (reverse)
            (map (fn [[_ [_ cat]]] [::CA cat]))
            distinct
            vec)])))

(defn calculate-shape-scales [points]
  (let [xs (map first points)
        ys (map second points)]
    [(calculate-shape-scale xs) (calculate-shape-scale ys)]))

(defn get-extremes [scale]
  (case (first scale)
    ::Continuous (let [[_ l h] scale]
                   [[::COV l] [::COV h]])
    ::Categorical (let [[_ vs] scale]
                    [[::CAR (first vs) 0.0] [::CAR (last vs) 1.0]])))

(defn calculate-magnitude-and-range [lo hi]
  (let [magnitude (Math/pow 10 (Math/round (Math/log10 (- hi lo))))
        magnitude (/ magnitude 2.0)]
    [magnitude [(* (Math/floor (/ lo magnitude)) magnitude)
                (* (Math/ceil (/ hi magnitude)) magnitude)]]))

(defn generate-steps [cnt k [lo hi]]
  (let [[magnitude [nlo nhi]] (calculate-magnitude-and-range lo hi)
        dividers [0.2 0.5 1.0 2.0 5.0 10.0 20.0 40.0 50.0 60.0 80.0 100.0]
        magnitudes (map #(/ magnitude %) dividers)
        step (->> magnitudes
                  (filter #(>= (/ (- hi lo) %) cnt))
                  first)
        step (or step (/ magnitude 100.0))
        k-step (* step k)]
    (->> (range nlo (+ nhi k-step) k-step)
         (filter #(and (>= % lo) (<= % hi))))))

(defn generate-axis-steps [scale]
  (case (first scale)
    ::Continuous (let [[_ [_ l] [_ h]] scale]
                   (->> (generate-steps 6.0 1.0 [l h])
                        (map (fn [x] [::COV [::CO x]]))))

    ::Categorical (let [[_ vs] scale]
                    (for [[_ s] vs]
                      [::CAR [::CA s] 0.5]))))

(defn generate-axis-labels [fmt s]
  (let [sunit s]
    (case (first s)
      ::Continuous (let [[_ [_ l] [_ h]] s]
                     (->> (generate-steps 6.0 2.0 [l h])
                          (map (fn [x]
                                 [[::COV [::CO x]]
                                  (fmt sunit [::COV [::CO x]])]))))

      ::Categorical (let [[_ vs] s]
                      (for [[_ s] vs]
                        [[::CAR [::CA s] 0.5]
                         (fmt sunit [::CAR [::CA s] 0.5])])))))

(defn calculate-scales [style shape]
  (let [calculate-scales-style calculate-scales
        calculate-scales (partial calculate-scales style)]
    (case (first shape)

      ::Style
      (let [[_ f shape] shape
            [scales shape] (calculate-scales-style (f style) shape)]
        [scales [::ScaledStyle f shape]])

      ::InnerScale
      (let [[_ sx sy shape] shape
            [[isx isy] shape] (calculate-scales shape)
            sx (or sx isx)
            sy (or sy isy)]
        [[sx sy] shape])

      ::Offset
      (let [[_ offs shape] shape
            [scales shape] (calculate-scales shape)]
        [scales [::ScaledOffset offs shape]])

      ::Padding
      (let [[_ pads shape] shape
            [[sx sy] shape] (calculate-scales shape)]
        [[sx sy] [::ScaledPadding pads sx sy shape]])

      ::Bubble
      (let [[_ x y rx ry] shape]
        (letfn [(make-singleton-scale [val]
                  (case (first val)
                    ::COV (let [[_ v] val] [::Continuous v v])
                    ::CAR (let [[_ v _] val] [::Categorical [v]])))]
          (let [scales [(make-singleton-scale x) (make-singleton-scale y)]]
            [scales [::ScaledBubble x y rx ry]])))

      ::Text
      (let [[_ x y va ha r t] shape]
        (letfn [(make-singleton-scale [val]
                  (case (first val)
                    ::COV (let [[_ v] val] [::Continuous v v])
                    ::CAR (let [[_ v _] val] [::Categorical [v]])))]
          (let [scales [(make-singleton-scale x) (make-singleton-scale y)]]
            [scales [::ScaledText x y va ha r t]])))

      ::Line
      (let [[_ line] shape]
        [(calculate-shape-scales line) [::ScaledLine line]])

      ::Shape
      (let [[_ line] shape]
        [(calculate-shape-scales line) [::ScaledShape line]])

      ::Axes
      (let [[_ show-top show-right show-bottom show-left shape] shape
            [orig-scales _] (calculate-scales shape)
            [sx sy] orig-scales
            [[lx hx] [ly hy]] [(get-extremes sx) (get-extremes sy)]
            line-style (fn [clr alpha width shape]
                         [::Style #(assoc %
                                          ::fill [::Solid [1.0 [::HTML "transparent"]]]
                                          ::stroke-width [::Pixels width]
                                          ::stroke-color [alpha [::HTML clr]])
                          shape])
            font-style (fn [style shape]
                         [::Style #(assoc %
                                          ::font style
                                          ::fill [::Solid [1.0 [::HTML "black"]]]
                                          ::stroke-color [0.0 [::HTML "transparent"]])
                          shape])
            shape [::Layered
                   (concat
                    [[::InnerScale sx sy
                      [::Layered
                       (concat
                        (for [x (generate-axis-steps sx)]
                          (line-style "#e4e4e4" 1.0 1
                                      [::Line [[x ly] [x hy]]]))
                        (for [y (generate-axis-steps sy)]
                          (line-style "#e4e4e4" 1.0 1
                                      [::Line [[lx y] [hx y]]])))]]]
                    (when show-top
                      (cons
                       (line-style "black" 1.0 2 [::Line [[lx hy] [hx hy]]])
                       (for [[x l] (generate-axis-labels (::format-axis-x-label style) sx)]
                         (font-style "9pt sans-serif"
                                     [::Offset [0 -10]
                                      [::Text x hy [::Baseline] [::Center] 0.0 l]]))))
                    (when show-right
                      (cons
                       (line-style "black" 1.0 2 [::Line [[hx hy] [hx ly]]])
                       (for [[y l] (generate-axis-labels (::format-axis-y-label style) sy)]
                         (font-style "9pt sans-serif"
                                     [::Offset [10 0]
                                      [::Text hx y [::Middle] [::Start] 0.0 l]]))))
                    (when show-bottom
                      (cons
                       (line-style "black" 1.0 2 [::Line [[lx ly] [hx ly]]])
                       (for [[x l] (generate-axis-labels (::format-axis-x-label style) sx)]
                         (font-style "9pt sans-serif"
                                     [::Offset [0 10]
                                      [::Text x ly [::Hanging] [::Center] 0.0 l]]))))
                    (when show-left
                      (cons
                       (line-style "black" 1.0 2 [::Line [[lx hy] [lx ly]]])
                       (for [[y l] (generate-axis-labels (::format-axis-y-label style) sy)]
                         (font-style "9pt sans-serif"
                                     [::Offset [-10 0]
                                      [::Text lx y [::Middle] [::End] 0.0 l]]))))
                    [shape])]
            padding [(if show-top 30 0)
                     (if show-right 50 0)
                     (if show-bottom 30 0)
                     (if show-left 50 0)]]
        (calculate-scales [::Padding padding shape]))

      ::Layered
      (let [[_ shapes] shape
            scaled (map calculate-scales shapes)
            sxs (map (fn [[[sx _]]] sx) scaled)
            sys (map (fn [[[_ sy]]] sy) scaled)
            scales [(reduce union-scales sxs) (reduce union-scales sys)]]
        [scales [::ScaledLayered (map second scaled)]]))))

;; ------------------------------------------------------------------------------------------------
;; Projections
;; ------------------------------------------------------------------------------------------------

(defn project-one [reversed [tlv thv] scale coord]
  (case [(first scale) (first coord)]
    [::Categorical ::CAR]
    (let [[_ cats] scale
          [_ [_ v] f] coord
          size (/ (- thv tlv) (count cats))
          i (->> cats
                 (keep-indexed (fn [i [_ vv]]
                                 (when (= vv v) i)))
                 (first))
          i (+ i f)]
      (if reversed
        (- thv (* i size))
        (+ tlv (* i size))))

    [::Continuous ::COV]
    (let [[_ [_ slv] [_ shv]] scale
          [_ [_ v]] coord]
      (if reversed
        (- thv (* (- thv tlv) (/ (- v slv) (- shv slv))))
        (+ tlv (* (- thv tlv) (/ (- v slv) (- shv slv))))))

    [::Categorical ::COV]
    (throw (ex-info (str "Cannot project continuous value " coord " on a categorical scale " scale) {}))

    [::Continuous ::CAR]
    (throw (ex-info (str "Cannot project categorical value " coord " on a continuous scale " scale) {}))))

(def project-one-x (partial project-one false))
(def project-one-y (partial project-one true))

;; ------------------------------------------------------------------------------------------------
;; Drawing
;; ------------------------------------------------------------------------------------------------

(defn hide-fill-clj [{::keys [animation] :as style}]
  (assert (nil? animation) "Animation not implemented")
  (assoc style ::fill [::Solid [0.0 [::RGB 0 0 0]]]))

(defn hide-fill [style]
  (hide-fill-clj (style-fs->clj style))
  #_(if (map? style)
      (hide-fill-clj style)
      (Drawing$$$hideFill style)))

(defn hide-stroke [style]
  (let [{::keys [animation] :as style} (style-fs->clj style)]
    (assert (nil? animation) "Animation not implemented")
    (assoc style ::stroke-color [0.0 (second (::stroke-color style))])))

(defn format-color [c]
  (case (first c)
    ::RGB (let [[_ r g b] c]
            (format "rgb(%d, %d, %d)" r g b))
    ::HTML (let [[_ clr] c]
             clr)))

(defn format-style [defs style]
  (let [style-clj (style-fs->clj style)
        {::keys [animation cursor font stroke-color stroke-width fill text-vertical text-horizontal]} style-clj]
    (assert (nil? animation) "Animation not implemented")
    (str
     "cursor:" cursor ";"
     "font:" font ";"
     (let [[so clr] stroke-color
           [_ sw] stroke-width]
       (format "stroke-opacity:%f; stroke-width:%dpx; stroke:%s; " so sw (format-color clr)))
     (case (first fill)
       ::Solid (let [[_ [fo clr]] fill]
                 (format "fill-opacity:%f; fill:%s; " fo (format-color clr))))
     (when text-vertical
       (let [va (case (first text-vertical)
                  ::Baseline "baseline"
                  ::Hanging "hanging"
                  ::Middle "middle")]
         (str "alignment-baseline:" va ";")))
     (when text-horizontal
       (let [ha (case (first text-horizontal)
                  ::Start "start"
                  ::Center "middle"
                  ::End "end")]
         (str "text-anchor:" ha ";"))))))

(defn draw-shape [ctx x1 y1 x2 y2 sx sy [tag :as shape]]
  (let [project (fn [[vx vy]]
                  (let [result [(project-one-x [x1 x2] sx vx)
                                (project-one-y [y1 y2] sy vy)]]
                    result))]
    (case tag
      ::ScaledOffset
      (let [[_ [dx dy] shape] shape]
        (draw-shape ctx (+ x1 dx) (+ y1 dy) (+ x2 dx) (+ y2 dy) sx sy shape))

      ::ScaledLayered
      (let [[_ shapes] shape]
        [::Combine (map #(draw-shape ctx x1 y1 x2 y2 sx sy %) shapes)])

      ::ScaledStyle
      (let [[_ f shape] shape]
        (draw-shape (update ctx ::style f) x1 y1 x2 y2 sx sy shape))

      ::ScaledShape
      (let [[_ points] shape
            path (-> (into [[::MoveTo (project (first points))]]
                           (map (fn [pt] [::LineTo (project pt)]))
                           (rest points))
                     (conj [::LineTo (project (first points))]))
            style (hide-stroke (::style ctx))]
        [::Path path style])

      ::ScaledPadding
      (let [[_ [t r b l] isx isy shape] shape]
        (letfn [(calculate-nested-range [rev [v1 v2] ins outs]
                  (case (first ins)
                    ::Continuous
                    (let [[_ [_ l] [_ h]] ins
                          pos [(project-one rev [v1 v2] outs [::COV [::CO l]])
                               (project-one rev [v1 v2] outs [::COV [::CO h]])]]
                      [(apply min pos) (apply max pos)])

                    ::Categorical
                    (let [[_ vals] ins
                          pos (mapcat (fn [[_ v]]
                                        [(project-one rev [v1 v2] outs [::CAR [::CA v] 0.0])
                                         (project-one rev [v1 v2] outs [::CAR [::CA v] 1.0])])
                                      vals)]
                      [(apply min pos) (apply max pos)])))]

          (let [[x1' x2'] (calculate-nested-range false [x1 x2] isx sx)
                [y1' y2'] (calculate-nested-range true [y1 y2] isy sy)]
            (draw-shape ctx (+ x1' l) (+ y1' t) (- x2' r) (- y2' b) isx isy shape))))

      ::ScaledLine
      (let [[_ line] shape
            path (into [[::MoveTo (project (first line))]]
                       (map (fn [pt] [::LineTo (project pt)]))
                       (rest line))
            style (hide-fill (::style ctx))]
        [::Path path style])

      ::ScaledText
      (let [[_ x y va ha r t] shape
            xy (project [x y])
            style (assoc (::style ctx)
                         ::text-vertical va
                         ::text-horizontal ha)]
        [::Text xy t r style])

      ::ScaledBubble
      (let [[_ x y rx ry] shape]
        [::Ellipse (project [x y]) [rx ry] (::style ctx)]))))

;; ------------------------------------------------------------------------------------------------
;; Event handling
;; ------------------------------------------------------------------------------------------------

;; ------------------------------------------------------------------------------------------------
;; Derived
;; ------------------------------------------------------------------------------------------------

(defn stroke-color [clr s]
  [::Style (fn [style] (assoc style ::stroke-color [1.0 [::HTML clr]])) s])

(defn fill-color [clr s]
  [::Style (fn [style] (assoc style ::fill [::Solid [1.0 [::HTML clr]]])) s])

(defn column [x y]
  [::Shape
   [[[::CAR x 0.0] [::COV y]]
    [[::CAR x 1.0] [::COV y]]
    [[::CAR x 1.0] [::COV [::CO 0.0]]]
    [[::CAR x 0.0] [::COV [::CO 0.0]]]]])

;; ------------------------------------------------------------------------------------------------
;; integration
;; ------------------------------------------------------------------------------------------------

(defn default-format [scale value]
  (case (first value)
    ::CAR (let [[_ [_ s]] value] s)
    ::COV (let [[_ [_ v]] value]
            ;; todo niceNumber
            v)))

(def defstyle
  {::fill [::Solid [1.0 [::RGB 196 196 196]]]
   ::stroke-color [1.0 [::RGB 255 0 0]]
   ::stroke-dash-array []
   ::stroke-width [::Pixels 2]
   ::animation nil
   ::cursor "default"
   ::font "10pt sans-serif"
   ::format-axis-x-label default-format
   ::format-axis-y-label default-format

   ;; introduced during style refactor
   ::text-vertical nil
   ::text-horizontal nil})

; create-svg
