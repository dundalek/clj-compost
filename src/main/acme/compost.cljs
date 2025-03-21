(ns acme.compost
  (:require ["compostjs" :refer [compost]]))

; (defn line [a] (.line compost (clj->js a)))
; (defn column [xp yp] (.column compost xp yp))
; (defn overlay [sh] (.overlay compost (clj->js sh)))
; (defn axes [a s] (.axes compost a (clj->js s)))
; (defn render [id viz] (.render compost id viz))
; (defn fill-color [c s] (.fillColor compost c (clj->js s)))

(defn scaleX [& args] (.apply (.-scaleX compost) compost (clj->js args)))
(defn scaleY [& args] (.apply (.-scaleY compost) compost (clj->js args)))
(defn scale [& args] (.apply (.-scale compost) compost (clj->js args)))
(defn nestX [& args] (.apply (.-nestX compost) compost (clj->js args)))
(defn nestY [& args] (.apply (.-nestY compost) compost (clj->js args)))
(defn nest [& args] (.apply (.-nest compost) compost (clj->js args)))
(defn overlay [& args] (.apply (.-overlay compost) compost (clj->js args)))
(defn padding [& args] (.apply (.-padding compost) compost (clj->js args)))
(defn fill-color [& args] (.apply (.-fillColor compost) compost (clj->js args)))
(defn stroke-color [& args] (.apply (.-strokeColor compost) compost (clj->js args)))
(defn font [& args] (.apply (.-font compost) compost (clj->js args)))
(defn column [& args] (.apply (.-column compost) compost (clj->js args)))
(defn bar [& args] (.apply (.-bar compost) compost (clj->js args)))
(defn bubble [& args] (.apply (.-bubble compost) compost (clj->js args)))
(defn text [& args] (.apply (.-text compost) compost (clj->js args)))
(defn shape [& args] (.apply (.-shape compost) compost (clj->js args)))
(defn line [& args] (.apply (.-line compost) compost (clj->js args)))
(defn axes [& args] (.apply (.-axes compost) compost (clj->js args)))
(defn on [& args] (.apply (.-on compost) compost (clj->js args)))
(defn svg [& args] (.apply (.-svg compost) compost (clj->js args)))
(defn html [& args] (.apply (.-html compost) compost (clj->js args)))
(defn interactive [& args] (.apply (.-interactive compost) compost (clj->js args)))
(defn render [& args] (.apply (.-render compost) compost (clj->js args)))

(def kw->constructor
  {:scaleX scaleX
   :scaleY scaleY
   :scale scale
   :nestX nestX
   :nestY nestY
   :nest nest
   :overlay overlay
   :padding padding
   :fill-color fill-color
   :stroke-color stroke-color
   :font font
   :column column
   :bar bar
   :bubble bubble
   :text text
   :shape shape
   :line line
   :axes axes
   :on on
   :svg svg
   :html html
   :interactive interactive})
   ; :render render})
