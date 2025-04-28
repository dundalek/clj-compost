(ns acme.compost.util
  (:require
   [acme.compost.core-new :as cc]
   [clojure.string :as str]
   [clojure.walk :as walk]))

(defn from-hiccup [kw->constructor viz]
  (walk/postwalk (fn [x]
                   (if-not (and (vector? x) (keyword? (first x)))
                     x
                     (let [[viz-kw & args] x]
                       ; (println "viz-kw" viz-kw)
                       (apply (get kw->constructor viz-kw) args))))
                 viz))

(defn parseValue [v]
  (if (number? v)
    [::cc/COV [::cc/CO v]]
    (if (sequential? v)
      (if (not= (count v) 2)
        (throw (ex-info "Cannot parse value: Expected a number or an array with two elements." {:value v}))
        (let [[a0 a1] v]
          (when-not (number? a1)
            (throw (ex-info "Cannot parse value: The second element should be a number." {:value v})))
          [::cc/CAR [::cc/CA a0] a1]))
      (throw (ex-info "Cannot parse value: Expected a number or an array with two elements." {:value v})))))

(def kw->constructor-crossplatform
  {; :scaleX     (fn [sc sh] [::cc/InnerScale (some-> sc some) nil sh])
   ; :scaleY     (fn [sc sh] [::cc/InnerScale nil (some-> sc some) sh])
   ; :scale      (fn [sx sy sh] [::cc/InnerScale (some-> sx some) (some-> sy some) sh])
   ; :nestX      (fn [lx hx s] [::cc/NestX (parseValue lx) (parseValue hx) s])
   ; :nestY      (fn [ly hy s] [::cc/NestY (parseValue ly) (parseValue hy) s])
   ; :nest       (fn [lx hx ly hy s]
   ;               [::cc/NestY
   ;                (parseValue ly)
   ;                (parseValue hy)
   ;                [::cc/NestX (parseValue lx) (parseValue hx) s]])
   :overlay (fn [sh] [::cc/Layered sh])
   ; :padding    (fn [t r b l s] [::cc/Padding [t r b l] s])
   :fill-color cc/fill-color
   :stroke-color cc/stroke-color
   ; :font       (fn [f c s] [::cc/Font f c s])
   :column (fn [xp yp] (cc/column [::cc/CA xp] [::cc/CO yp]))
   ; :bar (fn [xp yp] [::cc/Bar [:cc/CA xp] [::cc/CO yp]])
   :bubble (fn [xp yp w h] [::cc/Bubble (parseValue xp) (parseValue yp) w h])
   ; :text       (fn [xp yp t s r]
   ;               (let [r (if (nil? r) 0.0 r)
   ;                     s (if (nil? s) "" s)
   ;                     va (cond
   ;                          (str/includes? s "baseline") ::Baseline
   ;                          (str/includes? s "hanging") ::Hanging
   ;                          :else ::Middle)
   ;                     ha (cond
   ;                          (str/includes? s "start") ::Start
   ;                          (str/includes? s "end") ::End
   ;                          :else ::Center)]
   ;                 [::cc/Text (parseValue xp) (parseValue yp) va ha r t]))
   :shape (fn [a] [::cc/Shape (mapv (fn [[p0 p1]] [(parseValue p0) (parseValue p1)]) a)])
   :line (fn [a] [::cc/Line (mapv (fn [[p0 p1]] [(parseValue p0) (parseValue p1)]) a)])
   :axes (fn [a s]
           [::cc/Axes
            (str/includes? a "top") (str/includes? a "right") (str/includes? a "bottom") (str/includes? a "left")
            s])})

(defn from-hiccup-crossplatform [viz]
  (from-hiccup kw->constructor-crossplatform viz))
