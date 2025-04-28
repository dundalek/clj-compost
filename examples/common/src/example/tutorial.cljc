(ns example.tutorial
  (:require
   [example.fish :as fish]))

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
