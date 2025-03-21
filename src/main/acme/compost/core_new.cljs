(ns acme.compost.core-new
  (:require
   [acme.compost.core-new :as-alias cc]
   [clojure.string :as str]
   [goog.string :refer [format]]))

;; ------------------------------------------------------------------------------------------------
;; Domain that users see
;; ------------------------------------------------------------------------------------------------

;; ------------------------------------------------------------------------------------------------
;; SVG stuff
;; ------------------------------------------------------------------------------------------------

(defn svg-format-path [path]
  (->> path
       (map (fn [[tag :as segment]]
              (case tag
                ::cc/MoveTo (let [[_ x y] segment]
                              (str "M" x " " y " "))
                ::cc/LineTo (let [[_ x y] segment]
                              (str "L" x " " y " ")))))
       (str/join "")))

(defn render-svg [ctx [tag :as svg]]
  (case tag
    ::cc/Combine
    (let [[_ ss] svg]
      (mapcat #(render-svg ctx %) ss))

    ::cc/Path
    (let [[_ p style] svg]
      [[:path {:d (svg-format-path p)
               :style style}]])))

;; ------------------------------------------------------------------------------------------------
;; Calculating scales
;; ------------------------------------------------------------------------------------------------

(defn union-scales [s1 s2]
  (case [(first s1) (first s2)]
    [::cc/Continuous ::cc/Continuous]
    (let [[_ l1 h1] s1
          [_ l2 h2] s2]
      [::cc/Continuous (min l1 l2) (max h1 h2)])

    [::cc/Categorical ::Categorical]
    (let [[_ v1] s1
          [_ v2] s2]
      [::cc/Categorical (distinct (concat v1 v2))])

    (throw (js/Error. "Cannot union continuous with categorical"))))

(defn calculate-shape-scale [vals]
  ;; TODO: implement mismatched scales checking
  (let [first-val (first vals)]
    (if (= (first first-val) ::cc/COV)
      ;; For continuous values (COV), find min and max
      [::cc/Continuous
       [::cc/CO (->> vals (map (fn [[_ [_ v]]] v)) (apply min))]
       [::cc/CO (->> vals (map (fn [[_ [_ v]]] v)) (apply max))]]
      ;; For categorical values (CAR), collect unique categories
      [::cc/Categorical
       (->> vals
            (reverse)
            (map (fn [[_ [_ cat]]] [::cc/CA cat]))
            distinct
            vec)])))

(defn calculate-shape-scales [points]
  (let [xs (map first points)
        ys (map second points)]
    [(calculate-shape-scale xs) (calculate-shape-scale ys)]))

(defn calculate-scales [style shape]
  (let [calculate-scales (partial calculate-scales style)]
    (case (first shape)
      ::Line
      (let [[_ line] shape]
        [(calculate-shape-scales line) [::cc/ScaledLine line]])

      ::Shape
      (let [[_ line] shape]
        [(calculate-shape-scales line) [::cc/ScaledShape line]])

      ::Layered
      (let [[_ shapes] shape
            scaled (map calculate-scales shapes)
            sxs (map (fn [[[sx _]]] sx) scaled)
            sys (map (fn [[[_ sy]]] sy) scaled)
            scales [(reduce union-scales sxs) (reduce union-scales sys)]]
        [scales [::cc/ScaledLayered (map second scaled)]]))))

;; ------------------------------------------------------------------------------------------------
;; Projections
;; ------------------------------------------------------------------------------------------------

(defn project-one [reversed [tlv thv] scale coord]
  (case [(first scale) (first coord)]
    [::cc/Categorical ::cc/CAR]
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

    [::cc/Continuous ::cc/COV]
    (let [[_ [_ slv] [_ shv]] scale
          [_ [_ v]] coord]
      (if reversed
        (- thv (* (- thv tlv) (/ (- v slv) (- shv slv))))
        (+ tlv (* (- thv tlv) (/ (- v slv) (- shv slv))))))

    [::cc/Categorical ::cc/COV]
    (throw (js/Error. (str "Cannot project continuous value " coord " on a categorical scale " scale)))

    [::cc/Continuous ::cc/CAR]
    (throw (js/Error. (str "Cannot project categorical value " coord " on a continuous scale " scale)))))

(def project-one-x (partial project-one false))
(def project-one-y (partial project-one true))

;; ------------------------------------------------------------------------------------------------
;; Drawing
;; ------------------------------------------------------------------------------------------------

(defn hide-fill [{::cc/keys [animation] :as style}]
  (assert (nil? animation) "Animation not implemented")
  (assoc style ::cc/fill [::cc/Solid [0.0 [::cc/RGB 0 0 0]]]))

(defn hide-stroke [{::cc/keys [animation] :as style}]
  (assert (nil? animation) "Animation not implemented")
  (assoc style ::cc/stroke-color [0.0 (second (::cc/stroke-color style))]))

(defn format-color [c]
  (case (first c)
    ::cc/RGB (let [[_ r g b] c]
               (format "rgb(%d, %d, %d)" r g b))
    ::cc/HTML (let [[_ clr] c]
                clr)))

(defn format-style [defs {::cc/keys [animation cursor font stroke-color stroke-width fill]}]
  (assert (nil? animation) "Animation not implemented")
  (str
   "cursor:" cursor ";"
   "font:" font ";"
   (let [[so clr] stroke-color
         [_ sw] stroke-width]
     (format "stroke-opacity:%f; stroke-width:%dpx; stroke:%s; " so sw (format-color clr)))
   (case (first fill)
     ::cc/Solid (let [[_ [fo clr]] fill]
                  (format "fill-opacity:%f; fill:%s; " fo (format-color clr))))))

(defn draw-shape [ctx x1 y1 x2 y2 sx sy [tag :as shape]]
  (let [project (fn [[vx vy]]
                  (let [result #js[(project-one-x [x1 x2] sx vx)
                                   (project-one-y [y1 y2] sy vy)]]
                    result))]
    (case tag
      ::cc/ScaledLayered
      (let [[_ shapes] shape]
        [::cc/Combine (map #(draw-shape ctx x1 y1 x2 y2 sx sy %) shapes)])

      ::cc/ScaledLine
      (let [[_ line] shape
            path (into [[::cc/MoveTo (project (first line))]]
                       (map (fn [pt] [::cc/LineTo (project pt)]))
                       (rest line))
            style (format-style (::cc/definitions ctx) (hide-fill (::cc/style ctx)))]
        [::cc/Path path style])

      ::cc/ScaledShape
      (let [[_ points] shape
            path (-> (into [[::cc/MoveTo (project (first points))]]
                           (map (fn [pt] [::cc/LineTo (project pt)]))
                           (rest points))
                     (conj [::cc/LineTo (project (first points))]))
            style (format-style (::cc/definitions ctx) (hide-stroke (::cc/style ctx)))]
        [::cc/Path path style]))))

;; ------------------------------------------------------------------------------------------------
;; Event handling
;; ------------------------------------------------------------------------------------------------

;; ------------------------------------------------------------------------------------------------
;; Derived
;; ------------------------------------------------------------------------------------------------

;; ------------------------------------------------------------------------------------------------
;; integration
;; ------------------------------------------------------------------------------------------------

(defn default-format [scale value]
  (throw (js/Error. "Not implemented")))

(def defstyle
  {::cc/fill [::cc/Solid [1.0 [::cc/RGB 196 196 196]]]
   ::cc/stroke-color [1.0 [::cc/RGB 256 0 0]]
   ::cc/stroke-dash-array []
   ::cc/stroke-width [::cc/Pixels 2]
   ::cc/animation nil
   ::cc/cursor "default"
   ::cc/font "10pt sans-serif"
   ::cc/format-axis-x-label default-format
   ::cc/format-axis-y-label default-format})

; create-svg
