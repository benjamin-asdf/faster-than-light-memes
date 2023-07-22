(ns ftlmemes.hire-me)

(def state (atom {}))

(def toggle
  (fn [id]
    (let [new-state (swap! state update-in [:collapsables id] not)
          open? (get-in new-state [:collapsables id] false)]
      (set!
       (.-display (.-style (js/document.getElementById id)))
       (if open? "block" "none")))))

(set! (.-toggleCollapsable js/window) toggle)

(comment (toggle "bio"))
