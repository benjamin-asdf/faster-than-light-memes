(ns ftlmemes.page.hire-me
  (:require [hiccup.page :as hp]))

(def primary "#00FF00")
(def secondary "#00FFFF")
(def tert "#FF00FF")
(def button-text-color "#FFFFFF")

(defn button-style []
  {:color primary
   :background-color "#003300"
   :font-size "1.2rem"
   :font-weight :bold})

(defn
  page
  []
  (hp/html5
   [:html
    [:head
     [:meta {:charset "UTF-8"}]
     [:meta
      {:name "viewport"
       :content "width=device-width, initial-scale=1"}]
     [:link
      {:rel "shortcut icon"
       :href "data:,"}]
     [:link
      {:rel "apple-touch-icon"
       :href "data:,"}]
     [:link
      {:rel "stylesheet"
       :href "https://cdn.simplecss.org/simple.min.css"}]
     [:style]
     [:style
      (format
       "a,
a:visited {
    color: %s
}
"
       tert)]
     [:title "Hire Benjamin"]]
    [:body
     [:div.content
      [:div
       {:style {:text-align "center"
                :font-size "1.3rem"}}
       [:a
        {:style {:margin "1.5rem"}
         :href "https://faster-than-light-memes.xyz/"}
        "Blog"]
       [:a
        {:href "https://faster-than-light-memes.xyz/"}
        "Contact"]
       [:hr]
       [:div
        [:h1
         {:style {:color primary}}
         "Benjamin Schwerdtner"]
        [:div
         "Driven Clojure Programmer, contractor for inspired projects."]
        [:div
         {:style {:color "#666"}}
         "University dropout because I decided to become a wizard instead."]]
       [:div
        {:sytle {:margin-top "2rem"}}]
       [:div
        [:a
         {:href "/contact.html"}
         [:button
          {:style (button-style)}
          "Hire Benjamin"]]
        [:button
         {:style (merge
                  (button-style)
                  {:margin "1.5rem"})
          :onclick "scrollToBottom()"}
         "Not convinced"]]]]
     [:div
      [:img
       {:src "https://i.imgur.com/GeM8EQM.jpg"}]]
     [:div
      ;; make a grid with 4 cards
      {:style {:display "grid"
               :grid-template-columns "min(45rem, 50%)"
               :grid-gap "1rem"}}
      [:div.card
       {:style {:grid-column 1}}
       [:h2
        [:div
         [:span
          {:style (merge
                   (button-style)
                   {:padding "0.8rem"
                    :margin "0.4rem"
                    :font-weight "800"
                    :font-size "2rem"})}
          "()"]
         [:span
          {:style {:color tert}}
          "Clojure"]]]
       [:p
        "Preaching and practicing interactive programing. I don't believe in any set of limitations.
Ancient Lisp wisdom combinend with next generation ideas.
The REPL, Emacs and Clojure together with functional, bottom-up programming allow
me to deliver more robust systems quicker."]]
      [:div.card
       {:style {:grid-column 2}}
       [:h2
        [:div
         [:span
          {:style (merge
                   (button-style)
                   {:padding "0.8rem"
                    :margin "0.4rem"
                    :font-size "2rem"})}
          "Ψ"]
         [:span
          {:style {:color tert}}
          "Thought"]]]
       [:p
        "Thoughtfulness, careful design. Empathy for users and programmers alike.
I understand that technology has to do with humans."]]
      [:div.card
       {:style {:grid-column 1}}
       [:h2
        [:div
         [:span
          {:style (merge
                   (button-style)
                   {:padding "0.8rem"
                    :margin "0.4rem"
                    :font-size "2rem"})}
          "🫀"]
         [:span
          {:style {:color tert}}
          "Passion"]]]
       [:p
        "Real interest in science and technology.
Driven to contribute to the world.
Discounts when your project contributes to human flourishing.
"]]
      [:div.card
       {:style {:grid-column 2}}
       [:h2
        [:div
         [:span
          {:style (merge
                   (button-style)
                   {:padding "0.8rem"
                    :margin "0.4rem"
                    :font-size "2rem"})}
          "🔍"]
         [:span
          {:style
           {:color tert :font-size "1.5rem"}}
          "Scientist Programmer"]]]
       [:p
        "Scientist at heart. I challenge my assumptions and go where the evidence leads me.
Code is a series of best guesses. Imagination and note-taking are powerful tools of the mind.
I am not ashamed of using anything to help me understand a problem.
I speak up when I see anything amiss and document limitations with the software I produce.
I keep records of the issues I have encountered and their fixes."]]]
     [:script
      "function scrollToBottom() {
          window.scrollTo(0,
 document.body.scrollHeight);
      };
"]]]))

[{:gen/file "hire-benjamin.html"
  :gen/content (page)}]

