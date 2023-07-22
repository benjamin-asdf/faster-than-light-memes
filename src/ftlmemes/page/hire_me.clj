(ns ftlmemes.page.hire-me
  (:require
   [hiccup.util :as html]
   [hiccup.page :as hp]))

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
                :margin "0.4rem"
                :font-size "1.3rem"}}
       [:a
        {:style {:margin "1.5rem"}
         :href "https://faster-than-light-memes.xyz/"}
        "Blog"]
       [:a
        {:href "/contact.html"}
        "Contact"]
       [:hr]
       [:div
        [:h1
         {:style {:color primary}}
         "Benjamin Schwerdtner"]
        [:h3
         {:style {:color secondary}}
         [:span "Clojure Programmer"]
         [:span
          ",  Contractor, Artisan, Crafter"]]
        [:div
         "Dreaming big, I want to build great software and have a real impact."]]
       [:div
        {:style {:margin-top "2rem"}}]
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
     (let [id "bio"]
       [:div
        [:div
         {:style {:display "flex" :align-items "center"}}
         [:h3 "Short Bio"]
         [:button
          {:style
           {:margin-left "1rem"
            :margin-top "1.5rem"
            :background-color "transparent"
            :color primary
            :text-decoration "underline"}
           :onClick (format "toggleCollapsable('%s')" id)}
          "toggle"]]
        [:div
         {:id id :style {:display "none"}}
         [:div
          [:p
           "I am excited about technology and programming. Especially Lisp, Clojure, Emacs and Linux.
Many think I am joyful and curious. I like simple, straightforward and playful ideas. This fits with working at a Clojure Repl."]
          [:p
           "I am committed to building great software, understanding the problem and thinking about the design.
I choose Clojure as my weapon because it is an amazingly practical Lisp with a great community and an evolved data-oriented paradigm."]
          [:p
           "I have spent much of the last 5 years programming all day with a team of philanthropists.
I am inspired by Paul Graham, Rich Hickey, Eric Normand, Sherlock Holmes and other thinkers around Lisp and Clojure.
"]
          [:p
           "I was born in the lovely German town Esslingen am Neckar, known for its historical city core and a big medieval market.
From my dad, a master baker and probably one of the finest gingerbread makers in Germany, I learned to put love into my craft
and the value of having the heart in the right place.
I studied biology and got to appreciate clear and simple thinking,
"
           [:span "starting by reading "
            [:span {:style {:font-style "italic"}} "The Selfish Gene."]
            " I do not want to go back to the lab after knowing how fast I can make experiments with the computer."]]
          [:p
           "In my free time, I like to think about how the brain, evolution and computers work.
I do Inline-skating, juggling, and Rubik's Cube. I like finding little things that I am not good at yet and then dominating them.
"]
          [:p "I read a lot of science fiction and science, especially about neuroscience, computers and big history."]
          [:p "I express my ideas and document my growth on my blog: " [:a {:href " https://faster-than-light-memes.xyz/"} "faster-than-light-memes"]]]]
        [:hr]])
     [:div#pitch-div
      [:div.card
       [:h3
        [:div
         (icon "(λ☯)")
         [:span
          {:style {:color tert}}
          "Clojure"]]]
       [:p
        "Preaching and practicing interactive programming. I
don't believe in any set of limitations.
Clojure combines the wisdom of the ancient hackers, who discovered Lisp, with real-world tradeoffs and practicalities.
The REPL, Emacs and Clojure together with functional, bottom-up, data oriented programming allow
me to deliver more robust systems more quickly.
"]
       [:p
        "Did I mention I love the "
        [:a
         {:href "/the-joy-of-clojure.html"}
         "Joy of Clojure"]
        [:span "?"]]]
      [:div.card
       {:style {:grid-column 2}}
       [:h3
        [:div
         (icon "Ψ")
         [:span
          {:style {:color tert}}
          "Thought"]]]
       [:p
        "Thoughtfulness. Careful design.
Empathy for your users and the next programmer alike.
I try to share your vision of your product.
I understand that technology is used by humans and built by humans."]
       [:p
        "I am constantly learning and improving my understanding of what makes a "
        [:a
         {:href "/extending-your-reach.html"}
         "good tool"]
        [:span "."]
        [:span
         " I believe software that works well is beautiful."]]]
      [:div.card
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
        [:span
         "I give you a discount when I believe your project contributes to human flourishing.
I don't accept buggy software and want to rid the world of it.
I have "
         [:strong "FOSS"]
         " bias because it leads to more useful, focused tools and it
it aligns more strongly with my values."]]]
      [:div.card
       [:h3.long
        [:div
         (icon "🔍")
         [:span.long
          {:style {:color tert}}
          "Technologist"]]]
       [:p
        "Scientist at heart. I challenge my assumptions and go where the evidence leads me.
Code is a series of best guesses. Imagination and note-taking are powerful tools of the mind.
I am not ashamed of using anything to help me understand a problem.
I speak up when I see anything amiss and document limitations with the software I produce."]]
      [:div.card
       [:h3.long
        [:div
         (icon "🪙")
         [:span.long
          {:style {:color tert}}
          "Track Record"]]]
       [:ul
        [:li
         "5 years activism in the fight against poverty as programer in an idealistic team."]
        [:li
         [:span "Products: "]
         [:ul
          [:li
           [:a
            {:href "https://play.google.com/store/apps/details?id=com.gamingforgood.clashofstreamers"}
            "Mobile Minigames"]
           [:div
            "Mobile app game development, AWS cloud backend, microservices"]]
          [:li
           [:a
            {:href "https://hotreload.net/"}
            "Unity Hot Reload"]
           [:div "Epic hot reload tool on unity using Roslyn and Cecil for which I contributed as Sr. .Net developer"]]
          [:li
           [:a
            {:href "https://www.united-signals.com/en/"}
            "Sr. Clojure Developer at United Signals "]
           [:div "Clojure fullstack web application development, dynamic forms"]]]]
        [:li
         "Countless Emacs and Clojure tooling FOSS contributions. "
         [:a
          {:href "https://github.com/benjamin-asdf"}
          "Github"]
         [:span "."]]
        [:li
         "Worked with Borkdude (Babashka, Nbb, Scittle) and Bozhidar (Emacs Cider), sponsored by Nextjournal."]]]
      [:div
       [:a {:href "https://faster-than-light-memes.xyz/resume.html"} [:h4 "Resume with buzz words"]]]]
     [:script
      "function scrollToSecondPart() {
         var divToScrollTo = document.getElementById(\"pitch-div\");
         divToScrollTo.scrollIntoView({
                     behavior: 'smooth',
                     block: 'start'
});
      };"]
     (html/raw-string
      "<script src=\"https://cdn.jsdelivr.net/npm/scittle@0.6.15/dist/scittle.js\" type=\"application/javascript\"></script>
    <script>var SCITTLE_NREPL_WEBSOCKET_PORT = 1340;</script>
    <script src=\"https://cdn.jsdelivr.net/npm/scittle@0.6.15/dist/scittle.nrepl.js\" type=\"application/javascript\"></script>
")
     [:script {:type "application/x-scittle" :src "hire-me.cljs"}]]]))

[{:gen/file "hire-benjamin.html"
  :gen/content (page)}]
