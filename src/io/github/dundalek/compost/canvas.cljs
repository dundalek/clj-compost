(ns io.github.dundalek.compost.canvas
  (:require
   [io.github.dundalek.compost.core :as cc :refer [format-color style-fs->clj]]))

(defn apply-canvas-style [ctx style]
  (let [style-clj (style-fs->clj style)
        {::cc/keys [animation cursor font stroke-color stroke-width fill text-vertical text-horizontal]} style-clj]
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
      ::cc/Solid (let [[_ [fo clr]] fill]
                   (set! (.-fillStyle ctx) (format-color clr))
                   (when (not= fo 0.0)
                     (.fill ctx))))
    (when text-vertical
      (let [va (case (first text-vertical)
                 ;; not sure if "baseline" from SVG maps well, trying alphabetic because it is the default for canvas
                 ::cc/Baseline "alphabetic"
                 ::cc/Hanging "hanging"
                 ::cc/Middle "middle")]
        (set! (.-textBaseline ctx) va)))
    (when text-horizontal
      (let [ha (case (first text-horizontal)
                 ::cc/Start "start"
                 ::cc/Center "center"
                 ::cc/End "end")]
        (set! (.-textAlign ctx) ha)))))

(defn render-canvas [ctx shape]
  (case (first shape)
    ::cc/Text
    (let [[_ [x y] t rotation style] shape]
      (.save ctx)
      (apply-canvas-style ctx style)
      (if (= rotation 0.0)
        (.fillText ctx t x y)
        (throw (ex-info "Rotation not implemented yet" {})))
      (.restore ctx))

    ::cc/Combine
    (let [[_ ss] shape]
      (doseq [s ss]
        (render-canvas ctx s)))

    ::cc/Ellipse
    (let [[_ [cx cy] [rx ry] style] shape]
      (.save ctx)
      (.beginPath ctx)
      (.ellipse ctx cx cy rx ry 0 0 (* 2 Math/PI))
      (apply-canvas-style ctx style)
      (.restore ctx))

    ::cc/Path
    (let [[_ path style] shape]
      (.save ctx)
      (.beginPath ctx)
      (doseq [segment path]
        (case (first segment)
          ::cc/MoveTo (let [[_ [x y]] segment]
                        (.moveTo ctx x y))
          ::cc/LineTo (let [[_ [x y]] segment]
                        (.lineTo ctx x y))))

      (apply-canvas-style ctx style)
      (.restore ctx))))
