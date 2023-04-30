(ns ftlmemes.flipcoin
  (:require
   [reagent.core :as r]
   [reagent.dom :as rdom]))

(defn rand-with-prop [p]
  (< (rand) p))

(defn flip-coin [{:keys [chance] :as state}]
  (let [next-coin (if (rand-with-prop chance) :heads :tails)]
    (-> state
        (assoc :current-coin next-coin)
        (update :history conj next-coin)
        (update :counter inc))))

(defonce state (r/atom
                (flip-coin {:chance 1/2 :counter 0 :history []})))

(def
  coin-fill
  {:heads {:fill "magenta" :text "HEADS"}
   :tails {:fill "#90AFC5" :text "TAILS"}})

(defn coin-ui [heads?]
  (let [{:keys [fill text]} (coin-fill heads?)]
    [:svg
     {:xmlns "http://www.w3.org/2000/svg" :width "200" :height "200" :viewBox "0 0 100 100"}
     [:circle {:cx "50" :cy "50" :r "50" :fill fill}]
     [:text {:x "50%" :y "50%" :text-anchor "middle" :dy ".3em" :font-size "20" :fill "#000"} text]
     [:rect
      {:x "0" :y "0" :width "150" :height "150" :fill-opacity "0"
       :on-click (fn [_] (swap! state flip-coin))}]]))

(defn preview-coin [heads?]
  (let [{:keys [fill]} (coin-fill heads?)]
    [:svg
     {:xmlns "http://www.w3.org/2000/svg" :width "25" :height "25" :viewBox "0 0 100 100"}
     [:circle {:cx "50" :cy "50" :r "50" :fill fill}]]))

(defn how-many-ui [{:keys [history]}]
  (let [{:keys [heads tails]} (frequencies history)]
    [:div
     {:style
      {:display :flex :justify-content :space-between
       :margin-top "1rem"}}
     [:div "Heads: " [:strong (or heads 0)]]
     [:div {:style {:margin-left "1rem"}} "Tails: " [:strong (or tails 0)]]]))

(defn ui
  "The main UI component for the flipcoin."
  []
  (fn []
    (let [v @state]
      [:div
       [:style
        ".scale-in-animation {
   animation: bounce-in 0.2s ease-in-out;
 }

 @keyframes bounce-in {
   0%, 100% {
     transform: scale(1);
   }
   40% {
     transform: scale(0.4);
   }
   60% {
     transform: scale(1.1);
   }
 }"]


       [:h2 {:style {:display "flex" :justify-content "center"}} "Just flip a coin!"]
       [:div
        [:div
         {:key (:counter v)
          :style
          {:display "flex"
           :justify-content "center"
           :align-items "center"
           :flex-direction :column
           :height "25vh"
           :margin-top "10rem"}}
         [:div
          {:class "scale-in-animation"}
          (coin-ui (:current-coin v))]
         [:button
          {:style {:margin-top "1rem"}
           :on-click
           (fn [_] (swap! state flip-coin))} "Flip!"]
         [how-many-ui v]
         [:div
          {:style
           {:display "flex"
            :justify-content "center"
            :max-width "100%"
            :overflow-x "hidden"
            :overflow "hidden"
            :margin-top "1rem"
            :margin-bottom "5rem"
            :padding "2rem"}}
          (doall
           (map
            (fn [coin]
              [preview-coin coin])
            (take-last 20 (:history v))))]]
        [:div
         {:style {:margin-top "5rem"
                  :display "flex"
                  :justify-content "center"
                  :align-items "center"}}
         [:div
          [:input
           {:type "range" :min 0 :max 100 :value (* 100 (:chance @state))
            :onChange #(swap! state assoc :chance (/ (js/parseInt (-> % .-target .-value)) 100))}]
          [:div
           [:span "Heads Chance: "]
           [:strong
            (.toFixed (js/Number (:chance v)) 2)]]]
         [:div {:style {:margin-left "3rem"}}
          [:button
           {:on-click
            (fn [_] (swap! state assoc :chance 1/2))}
           "Reset to 1/2 chance"]]]]
       [:div
        {:style {:margin-top "2rem"}}
        [:a {:href "https://faster-than-light-memes.xyz/emacs-clojure-lispy-mind-meld.html"}
         "Blog post"]]])))

(rdom/render [ui] (.getElementById js/document "app"))
