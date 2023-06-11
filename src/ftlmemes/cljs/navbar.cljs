(ns navbar)

(def rand-button (js/document.getElementById "rand-page-button"))

(defn fetch-text [file cb]
  (->
   (js/fetch file)
   (.then (fn [x] (.text x)))
   (.then (fn [x] (cb x)))))

(defn pages [cb]
  (fetch-text
   "posts-list.edn"
   (fn [s]
     (cb (read-string s)))))

(defn rand-page! [_]
  (pages
   (fn [lst]
     (let [path
           (:path
            (rand-nth
             (into [] (remove (comp #{"contact.html"} :path)) lst)))]
       (set! (.-location js/window) path)))))

(.addEventListener rand-button "click" rand-page!)
