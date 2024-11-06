(ns ftlmemes.page.pages.pareidolia
  (:require
   ;; [clojure.data.json :as json]
   [shadow.css :refer (css)]
   ;; [hiccup.util :as html]
   [ftlmemes.page.ui.ui :as ui]
   [hiccup.page :as hp]))

(defn divider
  []
  [:div
   {:class (css :my-2
                :min-h-1 :w-full
                :rounded-lg :bg-slate-800)}])

(defn home-button
  []
  [:a
   {:class (css :rounded
                :p-3
                {:background "#feb48f"
                 :color "black"
                 :height "25px"
                 :width "25px"})
    :href "/"} "Home"])

(defn link-style [] (css :font-bold {:color "#feb48f"}))

(defn picture-iframe
  [width height]
  [:iframe
   (let [width 300
         height 350]
     {:height height
      :src
        (format
          "https://vehicles.faster-than-light-memes.xyz/art/pe/pareidolia/0?width=%s&height=%s"
          width
          height)
      :width width})])

(defn picture
  []
  [:div
   {:class (css :flex :w-full
                :items-center :justify-center)}
   [:div {:class (css :border :bg-black :rounded)}
    [:div
     {:class (css [:md {:display "none"}]
                  {:display "block"})}
     (picture-iframe 300 300)]
    ;; ------------------------------------------------
    [:div
     {:class (css [:md {:display "block"}]
                  {:display "none"})}
     (picture-iframe 600 500)]]])

(defn bottom-part
  []
  (list
    [:p {:class (css :my-2 :text-xl)}
     "Seeing the minds everywhere?"]
    [:p {:class (css :my-2)}
     " The spirits, memes, software agents that animate the world are having certain comebacks in computational science, cognitive science and cybernetics. "]
    [:p {:class (css :my-2)}
     "Perhaps it is exactly this agentness-detector that allows us to perceive (and make ad-hoc models of) information processing systems. "
     "What it all means is unclear, but it seems to me it points towards space where we would understand intelligence, creativity."]
    " How that relates to life and the physical world and so forth."
    (divider)
    [:p {:class (css :text-lg)} "Josha Bach: "
     [:a
      {:class (link-style)
       :href
         "https://youtu.be/3MkJEGE9GRY?si=5BW5dRM5oyqSP4Sh"}
      "Cyberanimism"] " "]
    [:p
     [:span {:class (css :my-2 :text-lg)}
      "Steven Wolfram: "]
     [:span
      "Some people say 'The weather has mind of it's own.'"]]
    [:p
     [:span {:class (css :my-2 :text-lg)} "Micheal Levin: "]
     [:span
      "Intelligence might be everywhere, don't make assumptions."]
     ;; [:span
     ;;  " In Levin's bio-philosophy, biological
     ;;  systems are a kind of software." " 'Cognition'
     ;;  is a spectrum. And the question of mind is one
     ;;  of interoperability." " Building a tower out
     ;;  of bricks is a different thing from building a
     ;;  tower out of dogs." " If you build a tower out
     ;;  of dogs, you probably need to treat your
     ;;  substrate in terms of" " Dennets 'intentional
     ;;  stance', Pavlovian
     ;;  conditioning and so forth."]
    ]
    [:p
     [:span {:class (css :my-2 :text-lg)}
      "Valentino Braitenberg: "]
     [:span
      "In his philosophical book 'Information, Der Geist in der Natur', the talks about the spirits in the world."
      ;; " Made from the arrangement of things, "
      " "] [:span]]
    [:p {:class (css :my-2)}
     "For me, they are memes, replicators with causal power, made from information."
     " More on this elsewhere."
     ;; " My hunch is that it is not coincidence,  we are made from sub-agents and we perceive agents."
     ;; [:p {:class (css :my-2)}
     ;;  " Perhaps modeling agents is 'for free' in brains, because it is a primitive."
     ;;  [:p {:class (css :my-2)}
     ;;   " The idea that perceptions, Gestalts, Memories, mental datastructures are software animals is what I called "
     ;;   [:a
     ;;    {:class (link-style)
     ;;     :href
     ;;     "biological-notes-on-the-vehicles-cell-assemblies.html"}
     ;;    "Neuronal Ensemble Memetics"]
     ;;   " (the link is kinda my early neurophilosophy stuff. Now want to make software using some of such ideas.)"
     ;;   " In that idea, the basic datastructure of the mind "
     ;;   [:strong "is an agent"] ". "]]
     ]))

(defn experiment-setup
  []
  (list
    [:p {:class (css :my-2 :text-xl)} "Experiment - Setup"]
    [:p
     [:p "Start some "
      [:a
       {:class (link-style)
        :href
          "https://courtesy707.bandcamp.com/track/courtesy-im-happy-i-am-not-susan-sontag"}
       "Music"] ", any music. "]
     [:p
      [:a {:class (link-style) :href "/pareidolia.html"}
       "Refresh"] " for best results. " "Or visit the "
      [:a {:class (link-style) :href ""} "fullscreen"
       " version"] "."]
     [:p {:class (css :mt-1)}
      "Why shouldn't it decide to dance with the music?"]]
    [:p {:class (css :non-italic :my-2)} "Note: "
     "A typical reaction to these is thinking there is some signal between music and picture."
     " Let me reveal a kind of magic trick, there are only random timers nothing else."]
    [:p "I want to  call this "
     [:span {:class (css :italic :font-bold)}
      "rhythm pareidolia"] " or something."]))

(defn character-detection
  []
  (list
    [:p {:class (css :my-2 :text-xl)} "Character Detection"]
    [:p {:class (css :my-2 :italic)}
     "If there is a dance, there is a dancer. A non-zero character having entity. "]
    [:p "During the "
     [:a
      {:class (link-style)
       :href
         "https://youtu.be/8FIEZXMUM2I?si=L3YscxtvuHYiff-X"}
      "Heider-Simmel Illusion"]
     ", the user perceives the shapes to be characters that follow a kind of storyline."
     " 'Big triangle is angry', 'small triange is scared', and so on. "
     [:p {:class (css :my-2)}
      "Our perception of character, and 'personhood' is not tied to sensor data corresponding to humans nor animals."
      " We readily attribute mind to shapes moving on a screen."
      " It is as if we have another pair of eyes in our minds, that see not the visual scene but that see animal-like or person-like, "
      "goal-having, feeling-having, relationship-having 'agents'."]]
    (divider)
    [:p {:class (css :my-2 :text-xl)}
     "Hyperactive Agent Detection"]
    [:p {:class (css :my-2)}
     "Dawkins mused in one of his books: "
     [:p {:class (css :italic :font-bold :my-2)}
      "From all animals that perceived rustling of savanna grass, the ones that erred on the side
      of caution and perceived a tiger were better off. "]
     [:p
      "A false negative (real: wind, perceived: tiger) makes you anxious and jumping around a little to much. "
      "A false positive (real: tiger, perceived: wind) makes you tiger dinner, statistically speaking."]
     [:p {:class (css :my-2)}
      " This is empirical. A bias of animal and human cognition towards agentness exists: "
      [:a
       {:class (link-style)
        :href
          "https://en.wikipedia.org/wiki/Agent_detection"}
       "Hyperactive Agent Detection"] "."]
     [:p {:class (css :my-2)}
      "(Incidentally this is a beautiful example of theoretical evolutionary biology contributing to the explanation of an aspect of the psyche)."]]
    (divider)
    [:p {:class (css :my-2 :text-xl)} "Interpretation"]
    [:p {:class (css :my-2)}
     "My interpretation is that we can attribute music to static noise and see faces in random clouds (Pareidolia)."]
    [:p {:class (css :font-bold)}
     " So too, we can perceive 'agentness' in random movement. 👈"]
    [:p "(This is the main outcome of this blog post)."]
    [:p {:class (css :my-2)}
     "Just like our 'internal agent eyes' see characters in the Heider-Simmel Illusion, and just like our 'sensor' eyes see faces in clouds."
     " Our 'internal agent eyes' can also see 'agents' in random noice. "]
    [:p {:class (css :my-2)}
     "That we have some kind of 'agent-perceiving' machinery lies at hand. "]
    [:p {:class (css :my-2)}
     "Just like the machinery that detects faces, even in clouds, the machinery that detects agents is 'on', and perhaps it is 'eager' to "
     "contribute to the story-line we use to make sense to the world."]))

(defn experience-report
  []
  (list
    [:p {:class (css :my-2 :text-xl)}
     "Experiment - Self report / Reflections"]
    [:p {:class (css :italic :font-bold :my-2)}
     "When I watch the picture while listening to music, I feel as if the circle-triangle-square knows when to move, and it does it with a joyful character."]
    [:p {:class (css :italic :font-bold :my-2)}
     "It is like there are small little stories in the fabric between the notes, the movement, the colors. "
     " Or perhaps it is sort of like additional music. "
     "The rhythm is the common language between visuals and music, they are polyphonic voices, contributing to a Gestalt, made from more than either the visuals or the music alone."
     " It's made from their relationship, their dance."]
    ;; ------------------
    [:p {:class (css :italic :font-bold :my-2)}
     "At times, the picture seems to play with us. As if to feign a move. Then perhaps reluctantly, then perhaps all in, then again dipping or then 'deliberately' out of sync."]
    [:p {:class (css :italic :font-bold :my-2)}
     "It is the fact that the relationship is weaved by the rhythm of the movements, together with this 'character' that I perceive the picture to have, it's non-zero 'expressiveness', that licenses me to describe this as a 'dance'."]))

(defn page
  []
  (hp/html5
    [:html
     [:head [:meta {:charset "UTF-8"}]
      [:meta
       {:content "width=device-width, initial-scale=1"
        :name "viewport"}]
      [:link {:href "data:," :rel "shortcut icon"}]
      [:link {:href "data:," :rel "apple-touch-icon"}]
      [:link {:href "/css/ui.css" :rel "stylesheet"}]
      [:script
       {:src
          "https://cdn.jsdelivr.net/npm/scittle@0.6.19/dist/scittle.js"}]
      [:title "FTLM - Pareidolia"]]
     [:body
      {:class (css :w-full
                   :h-full
                   :bg-black
                   {:color "white"}
                   :font-mono)}

      [:div {:class (css :hidden [:md :block]
                         :absolute {:left "5%" :top "5%"})}
       (home-button)]
      [:div
       {:class (css :flex :items-center
                    :justify-end :w-full)}
       [:div
        ;; title area
        {:class (css :flex :items-center
                     :justify-center :w-full)}
        [:div
         {:class (css :mt-6 :mb-4
                      :flex :flex-col
                      :items-center :justify-center)}
         [:div {:class (css :font-bold :text-4xl)}
          "Pareidolia"]
         [:div {:class (css :mt-2 :font-bold)}
          "'Rhythmic' pareidolia?"]]]]
      ;; -----------------------------------------
      (picture)
      ;; ------------------------------------------------
      [:div
       {:class
        (css :mt-2 :p-4 :flex :w-full :justify-center)}
       [:div {:class (css {:max-width "650px"})} (divider)
        [:p "Welcome to a new version you."]
        [:p
         "The you that has checked out the fascinating Wikipedia article to "
         [:a
          {:class (link-style)
           :href "https://en.wikipedia.org/wiki/Pareidolia"}
          "Pareidolia"] "."
         " Seeing faces in clouds, the Führer teapot, hearing 'music' in static noice."
         " Pareidolia shows how our meaning detectors are tuned. As if they are dialed up just a notch."]
        (divider)
        [:span
         {:class (css :font-bold
                      ;; {:color "#F689FF"}
                      )} "Leonardo da Vinci"]
        [:span " Talks about a way to make art: "]
        [:p
         {:class (css :p-2
                      :font-semibold
                      :italic
                      ;; {:color "#F689FF"}
                      )}
         "If you look at any walls spotted with various stains or with a mixture of different kinds of stones, if you are about to invent some scene you will be able to see in it a resemblance to various different landscapes adorned with mountains, rivers, rocks, trees, plains, wide valleys, and various groups of hills. You will also be able to see divers combats and figures in quick movement, and strange expressions of faces, and outlandish costumes, and an infinite number of things which you can then reduce into separate and well conceived forms."]
        (divider) (experiment-setup)
        ;; ------------------
        (divider) (experience-report)
        ;; ------------------
        (divider) (character-detection)
        ;; ---------------------------------
        ;; [:p {:class (css :my-2)} ""]
        ;; ---------------------------------------
        (divider)
        [:p "There is also the concept of "
         [:span {:class (css :font-bold)}
          "auditory pareidolia"] ". "
         "I forget the details, but many people reported they hear 'a Christmas carol' when prompted to listen for it "
         "in "
         [:a
          {:class (link-style)
           :href
           "https://youtu.be/ubFq-wV3Eic?si=YZLQAznD7QKTyMud"}
          "static noise"] ". " "Very eerie isn't it?"
         " There is something ghostly and perhaps insubstantial to it."
         " Not to mention the association with the voices of the psychotic."
         " It is the price we pay for the kind of perception we are endowed with. "
         " That weaves the fabric of the world by stories and the ideas that are not there. "
         " We see ideas, not sensor data. That much is certain."]
        (divider) (bottom-part) (divider) [:p "Also: "]
        [:p {:class (css :my-2)}
         [:a
          {:class (link-style)
           :href
           "https://en.wikipedia.org/wiki/Sensory_deprivation"}
          "Sensory Deprivation"]]
        [:p {:class (css :my-2)}
         [:a
          {:class (link-style)
           :href "https://en.wikipedia.org/wiki/Nocebo"}
          "Nocebo"]]]]]]))

(do
  (require '[ftlmemes.page.gen])
  (ftlmemes.page.gen/gen-html!
   {:gen/file "pareidolia.html"
    :gen/content (page)}))

[{:gen/file "pareidolia.html"
  :gen/content (page)}]
