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
   :font-weight "bold"})

(defn icon [unicode]
  [:span {:style (merge (button-style) {:padding "0.8rem" :margin "0.4rem" :font-size "2.3rem"})} unicode])

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

@media only screen and (max-width: 425px) {
    h3 {
        font-size: 1rem;
    }
    h3 .long {
        font-size: 0.8rem;
    }

}"
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
         "Clojure Programmer, contractor, pushing the limits."]
        [:div
         {:style {:color "#666"}}
         "University dropout because I wanted to become a wizard."]]
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
          :onclick "scrollToSecondPart()"}
         "Not convinced"]]]]
     [:div
      [:img
       {:src "https://i.imgur.com/GeM8EQM.jpg"}]]
     [:div#pitch-div
      {:style {:display "grid"
               :grid-template-columns "min(45rem, 50%)"
               :grid-gap "1rem"}}
      [:div.card
       {:style {:grid-column 1}}
       [:h3
        [:div (icon "λ")
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
       [:h3
        [:div (icon "Ψ") [:span {:style {:color tert}} "Thought"]]]
       [:p
        "Thoughtfulness, careful design.
Empathy for your users and future programmers alike.
I try to share your vision of your product.
I understand that technology has to do with humans on all levels."]
       [:p "I am constantly learning and improving my understanding of what makes a "
        [:a {:href "/extending-your-reach.html"} "good tool"]
        [:span "."]]]
      [:div.card
       {:style {:grid-column 1}}
       [:h3
        [:div
         (icon "🫀")
         [:span
          {:style {:color tert}}
          "Passion"]]]
       [:p
        [:span
         "Genuine interest in science and technology. "
         [:a
          {:href "/mid-term-goals.html"}
          "Real goals for humantity"]
         [:span ". "]]
        [:span "I give you a discount when I believe your project contributes to human flourishing.
I don't accept buggy software.
FOSS bias because it leads to more useful, focused tools and it it alignes more strongly with my values."]]]
      [:div.card
       {:style {:grid-column 2}}
       [:h3.long [:div (icon "🔍") [:span.long {:style {:color tert}} "Technologist"]]]
       [:p
        "Scientist at heart. I challenge my assumptions and go where the evidence leads me.
Code is a series of best guesses. Imagination and note-taking are powerful tools of the mind.
I am not ashamed of using anything to help me understand a problem.
I speak up when I see anything amiss and document limitations with the software I produce."]]
      [:div.card
       {:style {:grid-column 1}}
       [:h3.long [:div (icon "🪙") [:span.long {:style {:color tert}} "Track Record"]]]
       [:ul
        [:li "5 years activism in the fight against poverty,"
         [:span " raised $25M+ for charity"]
         [:span "."]]
        [:li
         [:span "Products: "]
         [:span
          [:a {:href "https://play.google.com/store/apps/details?id=com.gamingforgood.clashofstreamers"}
           "Mobile Minigames"]
          [:span ", "]]
         [:span [:a {:href "https://hotreload.net/"} "Unity Hot Reload"]
          [:span "."]]]
        [:li "Countless Emacs and Clojure tooling FOSS contributions. "
         [:a {:href "https://github.com/benjamin-asdf"} "Github"]
         [:span "."]]
        [:li "Worked with Borkdude (Babashka, Nbb, Scittle) and Bozhidar (Emacs Cider), sponsored by Nextjournal."]]]
      [:div.card
       {:style {:grid-column 2}}
       [:h3.long [:div (icon "📚") [:span.long {:style {:color tert}} "Stacks"]]]
       [:p
        "Clojure(script), Emacs Lisp, Linux, shell, Babashka, Python, Javascript, Typescript, Golang, HTML, CSS, Git, Datomic, postgres, AWS, graphql, docker, linode,
structural code editing, REPL, datastructures and algorithms, logic programming, functional programming."]]
      [:div.card
       {:style {:grid-column 1}}
       [:h3.long [:div (icon "⛏") [:span.long {:style {:color tert}} "The Plan"]]]
       [:ul
        [:li "1. You "
         [:a {:href "/contact.html"} "contact me"]
         [:span " with an initial pitch of your project."]]
        [:li "2. We have a chat and see if I can share your vision and whether I can help you build it, free for you."]
        [:li "3. We might have mutliple rounds of this, because I might discover that xyz is hard but xyb is feasible etc."]
        [:li "4. I come back to you with an offer 💰, on per-project basis, depending on complexity and operational requirements of the project."]
        [:li
         "5. If you are already a startup or Clojure workshop we can skip 3/4 and talk on per hour basis."]
        [:div
         [:span {:style {:font-weight "bold"}} "Q:"]
         " What is human flourishing?"]
        [:div
         [:span {:style {:font-weight "bold"}} "A:"]
         " Something like a cool science tool, or the website for your NGO. I know it when I see it."]
        [:div "I preserve the right to decide that seeminlgy decoupled from any objective or subjective measures."]]]]
     [:script
      "function scrollToSecondPart() {
         var divToScrollTo = document.getElementById(\"pitch-div\");
         divToScrollTo.scrollIntoView({
                     behavior: 'smooth',
                     block: 'start'
});
      };"]]]))

[{:gen/file "hire-benjamin.html"
  :gen/content (page)}]

