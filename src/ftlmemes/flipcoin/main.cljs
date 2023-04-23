(ns ftlmemes.flipcoin
  (:require
   [reagent.core :as r]
   [reagent.dom :as rdom]))

(defn rand-with-prop [p]
  (< (rand) p))

(defonce state (r/atom {:chance 1/2
                        :current-coin (rand-with-prop 1/2)}))

(defn flip-coin [{:keys [chance] :as state}]
  (assoc state :current-coin (rand-with-prop chance)))

(defn ui
  "The main UI component for the flipcoin."
  []
  (fn []
    (let [v @state]
      [:div
       [:h1 {:style {:display "flex" :justify-content "center"}} "Just flip a coin!"]
       [:div
        [:div {:style
               {:display "flex"
                :justify-content "center"
                :align-items "center"
                :flex-direction :column
                :height "20vh"}}
         (if
             (:current-coin v)
             [:svg {:xmlns "http://www.w3.org/2000/svg" :width "100" :height "100" :viewBox "0 0 100 100"}
              [:circle {:cx "50" :cy "50" :r "50" :fill "magenta"}]
              [:text {:x "50%" :y "50%" :text-anchor "middle" :dy ".3em" :font-size "20" :fill "#000"} "HEADS"]]
             [:svg {:xmlns "http://www.w3.org/2000/svg" :width "100" :height "100" :viewBox "0 0 100 100"}
              [:circle {:cx "50" :cy "50" :r "50" :fill "#90AFC5"}]
              [:text {:x "50%" :y "50%" :text-anchor "middle" :dy ".3em" :font-size "20" :fill "#000"} "TAILS"]])
         [:button
          {:style {:margin-top "1rem"}
           :on-click
           (fn [_] (swap! state flip-coin))}
          "Flip again"]]
        [:div
         {:style {:display "flex"
                  :justify-content "center"
                  :align-items "center"}}
         [:div
          {:style {:margin-right "2rem"}}
          [:input
           {:type "range" :min 0 :max 100 :value (* 100 (:chance @state))
            :onChange #(swap! state assoc :chance (/ (js/parseInt (-> % .-target .-value)) 100))}]
          [:div
           [:span "Heads Chance: "]
           [:strong (str (:chance v))]]]
         [:div {:style {:margin-left "1rem"}}
          [:button
           {:on-click
            (fn [_] (swap! state assoc :chance 1/2))}
           "Reset to 1/2 chance"]]]]])))

(rdom/render [ui] (.getElementById js/document "app"))
