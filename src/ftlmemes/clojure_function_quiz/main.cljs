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
   :element/asami :swap!})

(def
  quiz-data
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
                    :quiz/type :element/asami}]}
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
                    :quiz/type :element/asami}]}
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
                    :quiz/type :element/asami}]}
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
                    :quiz/type :element/asami}]}
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
                    :quiz/type :element/asami}]}])


(defn
  game-init-db []
  {::answers (into
             []
             (repeat (count quiz-data) nil))
   ::page 0
   ::q-answer-shuffle
   (let [[q-count a-count]
         [(count quiz-data)
          (count (-> quiz-data peek ::answers))]]
     (into {}
           (map
            (juxt identity
                  (fn [_] (shuffle (range a-count)))))
           (range q-count)))
   :last-history[]})

(comment
  (game-init-db))

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

(rf/reg-sub ::q-answer-shuffle (fn [db _] (::q-answer-shuffle db)))

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

(rf/reg-sub
 ::current-page-answer
 (fn [db _] (current-page-answer db)))

(rf/reg-sub ::page (fn [db _] (page db)))

(rf/reg-event-db
 :forward-page
 (fn [db [_ n]]
   (let [new-page (-> db ::page (+ (or n 1)))
         quiz-count (count quiz-data)]
     (if (and (>= new-page 0) (< new-page quiz-count))
       (assoc db ::page new-page)
       db))))

(rf/reg-sub
 ::intro?
 :<-
 [::page]
 not)

(rf/reg-sub
 ::won?
 :<-
 [::page]
 (fn [page]
   (println page (<= (count quiz-data) page))
   (when page (<=
               (count quiz-data) page))))

(rf/reg-event-db
 ::set-answer
 (fn [db [_ [page answer]]]
   (update db ::answers assoc page answer)))

;; forward arrow

;; back arrow

;; undo button

;; redo button

(rf/dispatch [::set-db {}])

(defn question-page
  [{:keys [page answer q-answer-shuffle]}]
  (def my-page page)
  (def answer answer)
  [:div 
   (let [{:quiz/keys [question answers]} (quiz-data page)]
     [:div [:h3 question]
      [:div
       (doall
        (for [[i {:quiz/keys [text]}] (map-indexed vector answers)]
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
           (str text (when (== answer i) 
                       " (selected)"))]))]])])

(defn ui []
  [:h1 "hurr"]
  (let [intro? @(rf/subscribe [::intro?])
        won? @(rf/subscribe [::won?])
        page @(rf/subscribe [::page])
        answer @(rf/subscribe [::current-page-answer])
        q-answer-shuffle @(rf/subscribe [::q-answer-shuffle])]
    (cond
      intro?
      [intro]
      won?
      [:div "won"]
      :else
      [:div
       [question-page {:page page :answer answer :q-answer-shuffle q-answer-shuffle}]])))

(comment
  (rf/dispatch [:forward-page 1])
  (rf/dispatch [:forward-page -1])
  (rf/dispatch
   [::set-db (game-init-db)])
  (rf/dispatch
   [::set-answer [0 0]]))

(rdom/render [ui] (.getElementById js/document "app"))






