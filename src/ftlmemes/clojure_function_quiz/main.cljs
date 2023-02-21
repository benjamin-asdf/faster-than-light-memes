(ns ftlmemes.clojure-function-quiz.main
  (:require
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [re-frame.core :as rf]))

(rf/reg-event-fx ::set-db (fn [_ [_ db]] {:db db}))

(def bender->clojure-function
  {:element/air :conj
   :element/fire :reduce
   :element/water :juxt
   :element/tinkerer :swap!})

(def
  quiz-data-1
  [{:quiz/question "How do you approach a new challenge?"
    :quiz/answers [{:quiz/text "By brainstorming multiple solutions and picking the best one."
                    :quiz/type :element/air}
                   {:quiz/text "By breaking the challenge down into smaller parts and tackling each part individually."
                    :quiz/type :element/fire}
                   {:quiz/text "By trying out different approaches until you find one that works."
                    :quiz/type :element/water}
                   {:quiz/text "By gathering all available information and analyzing it before making a plan."
                    :quiz/type :element/earth}
                   {:quiz/text "By thinking outside the box and using innovative solutions."
                    :quiz/type :element/tinkerer}]}
   {:quiz/question "When working on a project with others, how do you contribute?"
    :quiz/answers [{:quiz/text "By coming up with new and creative ideas."
                    :quiz/type :element/air}
                   {:quiz/text "By focusing on the details and making sure everything is done perfectly."
                    :quiz/type :element/fire}
                   {:quiz/text "By adapting to changes and being flexible when things don't go according to plan."
                    :quiz/type :element/water}
                   {:quiz/text "By using your expertise to gather and organize information for the project."
                    :quiz/type :element/earth}
                   {:quiz/text "By leveraging technology to make the project more efficient."
                    :quiz/type :element/tinkerer}]}
   {:quiz/question "Which of the following best describes your leadership style?"
    :quiz/answers [{:quiz/text "You inspire and motivate others with your vision and creativity."
                    :quiz/type :element/air}
                   {:quiz/text "You focus on efficiency and getting things done quickly."
                    :quiz/type :element/fire}
                   {:quiz/text "You listen to others and collaborate with them to find solutions."
                    :quiz/type :element/water}
                   {:quiz/text "You use your expertise and experience to guide and direct others."
                    :quiz/type :element/earth}
                   {:quiz/text "You use technology to streamline processes and optimize productivity."
                    :quiz/type :element/tinkerer}]}
   {:quiz/question "How do you deal with conflict?"
    :quiz/answers [{:quiz/text "You try to find a solution that works for everyone involved."
                    :quiz/type :element/water}
                   {:quiz/text "You take charge and try to resolve the conflict quickly."
                    :quiz/type :element/fire}
                   {:quiz/text "You use your communication skills to express your feelings and find common ground."
                    :quiz/type :element/air}
                   {:quiz/text "You gather information and weigh all the options before making a decision."
                    :quiz/type :element/earth}
                   {:quiz/text "You use technology to identify the root cause of the conflict and develop a solution."
                    :quiz/type :element/tinkerer}]}
   {:quiz/question "Which of the following best describes your problem-solving process?"
    :quiz/answers [{:quiz/text "You like to generate lots of ideas and then pick the best one."
                    :quiz/type :element/air}
                   {:quiz/text "You focus on efficiency and finding the most streamlined solution."
                    :quiz/type :element/fire}
                   {:quiz/text "You adapt and change your approach as you go along to find the best solution."
                    :quiz/type :element/water}
                   {:quiz/text "You gather as much information as possible before making a decision."
                    :quiz/type :element/earth}
                   {:quiz/text "You use technology to solve problems in innovative ways."
                    :quiz/type :element/tinkerer}]}])

(def outcome-page
  {:element/tinkerer
   [:div
    [:h1 "Congratulations, you are a skilled Tinkerer!"]
    [:p "Like Asami from the world of Avatar, you have a talent for building powerful machines and tackling complex problems with ingenuity and determination. You possess a unique ability to analyze problems from different angles and develop innovative solutions."]
    [:p "Your Clojure core function of choice is "
     [:span {:style {:font-weight :bold :font-size "1.5rem" :color "orange"}} "swap!"]
     [:span ", which allows you to build scalable and efficient solutions to even the most complex challenges. With swap!, you can easily update values and reorganize data in a way that suits your needs."]]
    [:p "Just like Asami, you have a strong sense of resourcefulness and are not afraid to take on new challenges. Keep up the great work and continue using your skills to overcome any obstacle that comes your way."]]
   :element/fire
   [:div
    [:h1 "Congratulations, you are aligned with Firebending!"]
    [:p "Like a Firebender, you have a talent for breaking down problems to their essential components and finding efficient solutions. You are practical and methodical in your approach to problem-solving."]
    [:p "Your Clojure core function is "
     [:span {:style {:font-weight :bold :font-size "1.5rem" :color "red"}} "reduce"]
     [:span ", which allows you to break down complex problems into smaller parts and solve each one individually. With reduce, you can efficiently analyze large amounts of data and make informed decisions."]]
    [:p "Just like Firebenders, you have a strong sense of determination and are not afraid to take on new challenges. Keep up the great work and continue using your skills to overcome any obstacle that comes your way."]]
   :element/air
   [:div
    [:h1 "Congratulations, you are a visionary problem solver like the Airbenders!"]
    [:p "Known for their imagination and ability to conjure up new ideas, Airbenders are always seeking to explore new possibilities."]
    [:p "The clojure.core function of your heart is "
     [:span {:style {:font-weight :bold :font-size "1.5rem" :color "#1E90FF"}} "conj"]
     [:span ", which allows you to combine different ideas and perspectives to create something new and innovative. Just like the Airbenders, you have a natural talent for creativity and are always seeking to explore new possibilities."]]
    [:p "Keep up the great work and continue to use your imagination and innovative spirit to create amazing solutions with Clojure's powerful conj function."]]
   :element/water
   [:div
    [:h1 "Congratulations, you are an adaptive problem solver like the Waterbenders!"]
    [:p "Known for their ability to take multiple approaches and adapt to changing situations, Waterbenders have a natural talent for flexibility and can navigate complex situations with ease."]
    [:p "The clojure.core function of your heart is "
     [:span {:style {:font-weight :bold :font-size "1.5rem" :color "#0074D9"}}  "juxt"]
     [:span  ", which allows you to approach problems from multiple angles and adapt your strategy as needed. Just like the Waterbenders, you have a natural talent for flexibility and can navigate complex situations with ease."]]
    [:p "Keep up the great work and continue to use your adaptive skills to find creative solutions with Clojure's powerful juxt function."]]
   :element/earth
   [:div
    [:h1 "Congratulations, you are an analytical problem solver like the Earthbenders!"]
    [:p "Known for their ability to gather and process information to make informed decisions, Earthbenders have a natural talent for analysis and are able to make informed decisions based on the data available."]
    [:p "The clojure.core function of your heart is "
     [:span {:style {:font-weight :bold :font-size "1.5rem" :color "green"}} "into"]
     [:span ", which allows you to gather and organize information to make sense of complex problems. Just like the Earthbenders, you have a natural talent for analysis and are able to make informed decisions based on the data available."]]
    [:p "Keep up the great work and continue to use your analytical skills to make informed decisions with Clojure's powerful into function."]]})


;; on load/refresh, we shuffle a fresh quiz-data

(def quiz-data
  (into []
        (map
         #(update % :quiz/answers (comp vec shuffle)))
        quiz-data-1))

(defn
  game-init-db []
  {::answers (into
              []
              (repeat (count quiz-data) nil))
   ::page 0
   :last-history[]})

(defn intro []
  [:div
   [:h1 "Welcome to the Avatar Elements and Clojure Core Functions Quiz!"]
   [:p "In this quiz, you'll answer questions about your personality and problem-solving style to determine which Avatar element and Clojure core function best fit you."]
   [:p "There are five questions in total, and for each question, you'll select the answer that best describes you. Don't overthink it - go with your gut instinct."]
   [:p "Ready to get started? Click the button below to begin."]
   [:button
    {:style {:margin-top "2rem"
             :width "40rem"
             :height "100%"
             :text-align "center"
             :font-size "2rem"
             }
     :on-click
     (fn [_] (rf/dispatch [::set-db (game-init-db)]))}
    "Play"]])

(def page ::page)

(defn
  current-page-answer
  [db]
  (when-let [answers (::answers db)]
    (some->> db page answers)))

(comment
  (current-page-answer
   {::answers [nil nil]
    ::page 0})
  (current-page-answer
   {::answers [:foo nil]
    ::page 0}))

(rf/reg-sub ::viewing-all-outcomes (fn [db _] (::viewing-all-outcomes db)))
(rf/reg-event-db
 ::set-viewing-all-outcomes
 (fn [db [_ v]]
   (if v
     (assoc db ::viewing-all-outcomes true)
     (dissoc db ::viewing-all-outcomes))))


(rf/reg-sub
 ::current-page-answer
 (fn [db _] (current-page-answer db)))

(rf/reg-sub ::page (fn [db _] (page db)))

(rf/reg-event-db
 :forward-page
 (fn
   [db [_ n]]
   (let [new-page (-> db ::page (+ (or n 1)))
         quiz-count (count quiz-data)]
     (cond
       (<= (count quiz-data) new-page)
       (assoc db ::won? true)
       (>= new-page 0)
       (assoc db ::page new-page)
       :else
       db))))

(rf/reg-sub ::intro? :<- [::page] not)
(rf/reg-sub ::won? (fn [db _] (::won? db)))
(rf/reg-sub ::answers (fn [db _] (::answers db)))

(rf/reg-sub
 ::playing?
 :<- [::intro?]
 :<- [::won?]
 (fn [intro? won?]
   (and (not intro?)
        (not won?))))

(rf/reg-event-db
 ::set-answer
 (fn [db [_ [page answer]]]
   (update db ::answers assoc page answer)))

(rf/dispatch [::set-db {}])

(defn question-page
  [{:keys [page answer]}]
  [:div 
   (let [{:quiz/keys [question answers]} (quiz-data page)]
     [:div [:h3 question]
      [:div
       (doall
        (for [[i {:quiz/keys [text type]}] (map-indexed vector answers)]
          [:button
           {:style
            {:width "90%"
             :background-color "gainsboro"
             :color "black"
             :font-size "2rem"
             :border (when (== answer i) 
                       "0.6rem solid greenyellow")}
            :on-click
            (fn
              [_]
              (rf/dispatch [:forward-page 1])
              (rf/dispatch [::set-answer [page i]]))}
           (str text " "
                ;; type " "
                (when (== answer i) 
                  " (selected)"))]))]])])

(defn user-answers [current-answers]
  (into
   []
   (map
    (fn
      [{:quiz/keys [answers]}
       user-answer]
      (->
       user-answer
       answers
       :quiz/type))
    quiz-data
    current-answers)))

(rf/reg-sub
 ::user-answers
 :<- [::answers]
 (fn [answers]
   (user-answers (keep identity answers))))

(rf/reg-sub
 ::user-outcome
 :<- [::user-answers]
 (fn
   [user-answers]
   (ffirst (sort-by (comp - val) (frequencies user-answers)))))


(defn all-outcomes []
  (into [:div] (map (fn [text] [:div text])) (vals outcome-page)))

(defn ui []
  (let [intro? @(rf/subscribe [::intro?])
        won? @(rf/subscribe [::won?])
        page @(rf/subscribe [::page])
        viewing-all-outcomes @(rf/subscribe [::viewing-all-outcomes])
        user-outcome @(rf/subscribe [::user-outcome])
        answer @(rf/subscribe [::current-page-answer])]
    (cond
      viewing-all-outcomes
      [:div
       [:div "(Refresh to play again)"]
       [all-outcomes]]
      intro?
      [intro]
      (and won? user-outcome)
      [:div
       (outcome-page user-outcome)
       [:button
        {:style {:font-size "2rem"}
         :on-click
         (fn
           [_]
           (rf/dispatch [::set-viewing-all-outcomes true]))}
        "View all possible outcomes"]
       [:div "(Refresh to play again)"]]
      :else
      [:div
       [question-page {:page page :answer answer}]])))

(comment
  (rf/reg-sub ::whole-db (fn [db _] db))
  (rf/reg-sub ::user-answers (fn [db _] (user-answers db)))
  @(rf/subscribe [::whole-db])

  (rf/dispatch
   [::set-db 
    {:ftlmemes.clojure-function-quiz.main/answers [0 0 0 0 1]
     :ftlmemes.clojure-function-quiz.main/page 0,
     :ftlmemes.clojure-function-quiz.main/won? true}])

  (second (max-key second (frequencies @(rf/subscribe [::user-answers]))))
  (ffirst
   (sort-by
    (comp - val)
    (frequencies  [:element/tinkerer
                   :element/tinkerer
                   :element/air
                   :element/air
                   :element/air
                   :element/air])))

  
  (let [user-answers (repeat 5 0)]
    (into
     []
     (map
      (fn
        [{:quiz/keys [answers]}
         user-answer]
        (-> user-answer answers :quiz/type))
      quiz-data
      user-answers)))

  (user-answers
   @(rf/subscribe [::answers]))

  
  @(rf/subscribe [::user-answers])
  (rf/dispatch [:forward-page 5])
  (rf/dispatch [:forward-page -1])
  (rf/dispatch [::set-db (game-init-db)]))

(rdom/render [ui] (.getElementById js/document "app"))
