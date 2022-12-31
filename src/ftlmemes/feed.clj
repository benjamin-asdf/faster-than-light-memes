(ns ftlmemes.feed
  (:require
   [babashka.fs :as fs]
   [babashka.process :as p]
   [clojure.walk]
   [clojure.string :as str]
   [clojure.data.xml :as xml]))

(defn blog-link [opts path]
  (str (:blog-root opts) path))

(defn sort-posts [posts]
  (sort-by :date (comp - compare) posts))

;;;; Generate atom feeds

(xml/alias-uri 'atom "http://www.w3.org/2005/Atom")
(import java.time.format.DateTimeFormatter)

(defn- rfc-3339-now []
  (let [fmt (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ssxxx")
        now (java.time.ZonedDateTime/now java.time.ZoneOffset/UTC)]
    (.format now fmt)))

(defn- rfc-3339 [emacs-id]
  (let [in-fmt (DateTimeFormatter/ofPattern "yyyyMMdd'T'HHmmss")
        local-date (java.time.LocalDate/parse emacs-id in-fmt)
        fmt (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ssxxx")
        now (java.time.ZonedDateTime/of (.atTime local-date 23 59 59) java.time.ZoneOffset/UTC)]
    (.format now fmt)))

(defn- atom-feed
  ;; validate at https://validator.w3.org/feed/check.cgi
  [{:keys [blog-title blog-author blog-root] :as opts} posts]
  (-> (xml/sexp-as-element
       [::atom/feed
        {:xmlns "http://www.w3.org/2005/Atom"}
        [::atom/title blog-title]
        [::atom/link {:href (blog-link opts "atom.xml") :rel "self"}]
        [::atom/link {:href blog-root}]
        [::atom/updated (rfc-3339-now)]
        [::atom/id blog-root]
        [::atom/author
         [::atom/name blog-author]]
        (for [{:keys [title file html identifier]} posts
              :let [link (blog-link opts file)]]
          (do
            (println title file identifier)
            [::atom/entry
             [::atom/id link]
             [::atom/link {:href link}]
             [::atom/title title]
             [::atom/updated (rfc-3339 identifier)]
             [::atom/content {:type "html"}
              [:-cdata html]]]))])
      xml/indent-str))

(defn- spit-feeds [{:keys [out-dir new-files posts] :as opts}]
  (let [feed-file (fs/file out-dir "atom.xml")
        clojure-feed-file (fs/file out-dir "planetclojure.xml")
        all-posts (sort-posts posts)
        clojure-post? (fn [{:keys [tags]}] (some tags ["clojure" "clojurescript"]))
        clojure-posts
        (->> (vals posts) (filter clojure-post?))
        new-clojure-posts
        (->> clojure-posts (filter (comp new-files :file)))
        clojure-posts-modified? (seq new-clojure-posts)]
    (if (and (not clojure-posts-modified?) (fs/exists? clojure-feed-file))
      (println "No new Clojure posts; skipping Clojure feed")
      (do
        (println "Writing Clojure feed" (str clojure-feed-file))
        (spit clojure-feed-file (atom-feed opts clojure-posts))))
    (if (and (empty? new-files) (fs/exists? feed-file))
      (println "No posts modified; skipping main feed")
      (do
        (println "Writing feed" (str feed-file))
        (spit feed-file (atom-feed opts (vals all-posts)))))))

(defn
  parse-date-1
  [fmt s]
  (->
   (java.text.SimpleDateFormat. fmt)
   (.parse s)))

(defn ->post [opts org-data]
  (let [{:keys [identifier filetags EXPORT_FILE_NAME] :as d}
        (clojure.walk/keywordize-keys org-data)
        file (str EXPORT_FILE_NAME ".html")
        html-file (fs/file (:out-dir opts) file)]
    (println file identifier EXPORT_FILE_NAME)
    (assoc
     d
     :date (rfc-3339 identifier)
     :file file
     :tags (into #{} (remove str/blank? (str/split filetags #":")))
     :html (slurp html-file))))

(defn ->posts [opts sx]
  (into
   {}
   (comp
    (map #(->post opts %))
    (map (juxt :file identity)))
   sx))

(defn
  new-files
  [dir]
  (into
   #{}
   (filter
    #(str/ends-with? % ".html")
    (str/split-lines
     @(:out
       (p/process
        ["git" "ls-files" "-o"]
        {:dir dir :out :string}))))))

(comment
  (def a-post '{:path "/home/benj/notes/20220923T112406--foo__public.org",
                :tags ("public"),
                :date #inst
                "2022-09-23T11:24:06.000-00:00",
                :filetags ":public:clojure:",
                :file "foo.html",
                :title "foo",
                :identifier "20220923T112406",
                :EXPORT_FILE_NAME "foo"})

  (type (:date (->post {:out-dir "public"} a-post)))


  (def clojure-posts (->>
                      (keep
                       (->posts {:out-dir "public/"} [a-post])
                       (new-files "public/"))
                      (filter (fn [{:keys [tags]}]
                                (some tags ["clojure" "clojurescript"])))
                      sort-posts))

  (def opts (-> (read-string (slurp "bb.edn")) :blog))
  (atom-feed opts clojure-posts)
  (->>
   (new-files "public/")
   (keep (->posts {:out-dir "public/"} [a-post]))))

(when (= *file* (System/getProperty "babashka.file"))
  (let [opts (-> (read-string (slurp "bb.edn")) :blog)
        opts (assoc
              opts
              :posts (->posts opts (read))
              :new-files (new-files (:out-dir opts)))]
    (spit-feeds opts)))
