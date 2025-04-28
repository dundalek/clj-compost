(ns io.github.dundalek.compost.svg
  (:require
   [clojure.string :as str]
   [io.github.dundalek.compost.core :as cc :refer [format-style]]
   #?(:cljs [goog.string :refer [format]])))

(defn svg-format-number [x]
  (double x))

(defn svg-format-path [path]
  (->> path
       (map (fn [[tag :as segment]]
              (case tag
                ::cc/MoveTo (let [[_ [x y]] segment]
                              (str "M" (svg-format-number x) " " (svg-format-number y) " "))
                ::cc/LineTo (let [[_ [x y]] segment]
                              (str "L" (svg-format-number x) " " (svg-format-number y) " ")))))
       (str/join "")))

(defn render-svg [ctx [tag :as svg]]
  (case tag
    ::cc/Text
    (let [[_ [x y] t rotation style] svg
          attrs (-> (if (= rotation 0.0)
                      {:x (str x) :y (str y)}
                      {:x "0" :y "0"
                       :transform (format "translate(%f,%f) rotate(%f)" x y rotation)})
                    (assoc :style (format-style (::cc/definitions ctx) style)))]
      [[:text attrs t]])

    ::cc/Combine
    (let [[_ ss] svg]
      (mapcat #(render-svg ctx %) ss))

    ::cc/Ellipse
    (let [[_ [cx cy] [rx ry] style] svg]
      [[:ellipse {:cx (str cx) :cy (str cy) :rx (str rx) :ry (str ry)
                  :style (format-style (::cc/definitions ctx) style)}]])

    ::cc/Path
    (let [[_ p style] svg]
      [[:path {:d (svg-format-path p)
               :style (format-style (::cc/definitions ctx) style)}]])))
