(ns navbar)

(def rand-button (js/document.getElementById "rand-page-button"))

(defn fetch-text [file cb]
  (->
   (js/fetch file)
   (.then (fn [x] (.text x)))
   (.then (fn [x] (cb x)))))

(defn random-page [cb]
  (fetch-text
   "posts-list.edn"
   (fn [s]
     (let [lst (into [] (read-string s))]
       (cb (rand-nth lst))))))

(defn rand-page! [_]
  (random-page
   (fn [{:keys [path]}]
     (set! (.-location js/window) path))))

(.addEventListener rand-button "click" rand-page!)
