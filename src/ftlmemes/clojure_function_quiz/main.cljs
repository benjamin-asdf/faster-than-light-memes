(ns ftlmemes.clojure-function-quiz.main
  (:require
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [re-frame.core :as rf]))

(rf/reg-event-fx ::set-db (fn [_ [_ db]] {:db db}))

(def bender->clojure-function
  {:airbender :conj
   :firebender :reduce
   :waterbender :juxt
   :asami :swap!})

(def quiz-data
  [{:question "How do you approach a new challenge?"
    :answers [{:answer "By brainstorming multiple solutions and picking the best one."
               :type :airbender}
              {:answer "By breaking the challenge down into smaller parts and tackling each part individually."
               :type :firebender}
              {:answer "By trying out different approaches until you find one that works."
               :type :waterbender}
              {:answer "By gathering all available information and analyzing it before making a plan."
               :type :earthbender}
              {:answer "By thinking outside the box and using innovative solutions."
               :type :asami}]}

   {:question "When working on a project with others, how do you contribute?"
    :answers [{:answer "By coming up with new and creative ideas."
               :type :airbender}
              {:answer "By focusing on the details and making sure everything is done perfectly."
               :type :firebender}
              {:answer "By adapting to changes and being flexible when things don't go according to plan."
               :type :waterbender}
              {:answer "By using your expertise to gather and organize information for the project."
               :type :earthbender}
              {:answer "By leveraging technology to make the project more efficient."
               :type :asami}]}

   {:question "Which of the following best describes your leadership style?"
    :answers [{:answer "You inspire and motivate others with your vision and creativity."
               :type :airbender}
              {:answer "You focus on efficiency and getting things done quickly."
               :type :firebender}
              {:answer "You listen to others and collaborate with them to find solutions."
               :type :waterbender}
              {:answer "You use your expertise and experience to guide and direct others."
               :type :earthbender}
              {:answer "You use technology to streamline processes and optimize productivity."
               :type :asami}]}

   {:question "How do you deal with conflict?"
    :answers [{:answer "You try to find a solution that works for everyone involved."
               :type :waterbender}
              {:answer "You take charge and try to resolve the conflict quickly."
               :type :firebender}
              {:answer "You use your communication skills to express your feelings and find common ground."
               :type :airbender}
              {:answer "You gather information and weigh all the options before making a decision."
               :type :earthbender}
              {:answer "You use technology to identify the root cause of the conflict and develop a solution."
               :type :asami}]}

   {:question "Which of the following best describes your problem-solving process?"
    :answers [{:answer "You like to generate lots of ideas and then pick the best one."
               :type :airbender}
              {:answer "You focus on efficiency and finding the most streamlined solution."
               :type :firebender}
              {:answer "You adapt and change your approach as you go along to find the best solution."
               :type :waterbender}
              {:answer "You gather as much information as possible before making a decision."
               :type :earthbender}
              {:answer "You use technology to solve problems in innovative ways."
               :type :asami}]}])


(defn
  game-init-db []
  {:answers (into
             []
             (repeat (count quiz-data) nil))
   :page 1
   :q-answer-shuffle
   (let [[q-count a-count]
         [(count quiz-data)
          (count (-> quiz-data peek :answers))]]
     (into {}
           (map
            (juxt identity
                  (fn [_] (shuffle (range a-count)))))
           (range q-count)))
   :last-history[]})

(comment
  (game-init-db)
  {:answers [nil nil nil nil nil], :page 1, :q-answer-shuffle {0 [0 2 1 4 3], 1 [1 4 3 2 0], 2 [4 2 1 0 3], 3 [1 4 0 3 2], 4 [2 4 3 1 0]}, :last-history[]})

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

(rf/reg-sub :q-answer-shuffle (fn [db _] (:q-answer-shuffle db)))

(rf/reg-event-db :forward-page (fn [db [_ n]] (update db :page (fnil + 0) n)))

(def page :page)

(defn
  current-page-answer
  [db]
  (when-let [answers (:answers db)]
    (some->> db page answers)))

(comment
  (current-page-answer
   {:answers [nil nil]
    :page 0})
  (current-page-answer
   {:answers [:foo nil]
    :page 0}))

(rf/reg-sub
 :get-current-page-answer
 (fn [db _] (current-page-answer db)))

(rf/reg-sub :page (fn [db _] (page db)))

(rf/reg-sub
 :get-intro?
 :<-
 [:page]
 not)

;; forward arrow

;; back arrow

;; undo button

;; redo button

(defn answer-button [text on-click]
  [:button.answer-btn {:on-click on-click}
   text])


(rf/dispatch [::set-db {}])


(defn ui []
  [:h1 "hurr"]
  (let [intro? @(rf/subscribe [:get-intro?])
        page @(rf/subscribe [:page])
        answer @(rf/subscribe [:get-current-page-answer])
        ]
    (if
        intro?
        [:div [intro]]
        
        
        )))

(rdom/render [ui] (.getElementById js/document "app"))
