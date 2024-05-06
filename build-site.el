;; -*- lexical-binding: t; -*-

;; I just load my config for the visuals
(load-file "~/.emacs.d/init.el")
(use-package htmlize)

(require 'ox-publish)
(require 'denote)
(require 'parseedn)

(setf denote-directory "~/notes/")
(defvar ftlm/index-file "/home/benj/notes/20220923T161021--index__public.org")
(defvar ftlm/posts-file "/home/benj/notes/20221210T171258--ftlm-navbar__ftlm_public.org")

(defun ftlm/file->denote-links (file)
  (with-current-buffer (find-file-noselect file)
    (denote-link--expand-identifiers "\\[\\[denote:\\(?1:\\([0-9]\\{8\\}\\)\\(T[0-9]\\{6\\}\\)\\)]\\[.*?]]")))

(defun ftlm/denote-file-data (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((res)
	  (end (save-excursion (re-search-forward "^$"))))
      (while (re-search-forward "#\\+\\(.+?\\):\\s-+\\(.+?\\)$" end t)
	(push
	 (cons (match-string 1)
	       (match-string 2))
	 res))
      res)))

(defun dw/strip-file-name-metadata (file-name)
  (replace-regexp-in-string "^.*--\\(.*?\\)__.*$" "\\1" file-name))

(defun ftlm/post-data (file)
  (cons (cons :path file) (ftlm/denote-file-data file)))

(defun ftlm/post-files ()
  (mapcar
   (lambda (p) (expand-file-name p denote-directory))
   (cl-remove-if
    (lambda (s)
      (string-match-p "org_archive$" s))
    (ftlm/file->denote-links ftlm/posts-file))))

(defun ftlm/posts+index-files ()
  (append
   (list ftlm/index-file ftlm/posts-file)
   (ftlm/post-files)
   '("/home/benj/notes/20230301T123854--contact__public.org")))

(defun ftlm/posts (files)
  (mapcar #'ftlm/post-data files))

(defun note->path (lst) (cdr (assq :path lst)))


(dolist (path (ftlm/posts+index-files))
  (with-current-buffer
      (find-file-noselect path)
    (goto-char (point-min))
    (unless
        (re-search-forward "#\\+EXPORT_FILE_NAME:" nil t)
      (goto-char (point-min))
      (re-search-forward "^$")
      (let ((inhibit-read-only t))
        (insert
         (format "#+EXPORT_FILE_NAME: %s\n" (dw/strip-file-name-metadata path)))))
    (let ((before-save-hook nil))
      (save-buffer))))

(defun navbar-elm (link desc)
  (format "<li><a href=\"%s\">%s</a></li>\n" link desc))

(advice-add
 'denote-directory-files
 :around
 (defalias
   'denote-directory-files-around-advice
   (let ((m nil))
     (lambda (old-fn)
       (or m (setf m (funcall old-fn)))))))


(defun ftlm/denote-curr-keywords (file)
  (if-let* ((file-type (denote-filetype-heuristics file)))
      (with-current-buffer
          (find-file-noselect file)
        (denote-retrieve-keywords-value file file-type))
    (error "not a denote file: %s" file)))

(defun ftlm/navbar-posts-denote-links ()
  (with-current-buffer
      (find-file-noselect ftlm/posts-file)
    (goto-char (point-min))
    (cl-loop
     while (re-search-forward org-link-any-re nil t)
     collect
     (let* ((start (match-beginning 0))
            (link-object (save-excursion
                           (goto-char start)
                           (save-match-data
                             (org-element-link-parser))))
            (link (org-element-property
                   :path link-object))
            (path-id (denote-link--ol-resolve-link-to-target
                      link
                      :path-id))
            (path (file-name-nondirectory
                   (car path-id)))
            (p (file-name-sans-extension path))
            (p (dw/strip-file-name-metadata p))
            (description (buffer-substring-no-properties
                          (org-element-property
                           :contents-begin link-object)
                          (org-element-property
                           :contents-end link-object))))
       `((:path . ,(format "%s.html" p))
         (:description . ,description)
         (:tags . ,(ftlm/denote-curr-keywords path)))))))

(defun escape-fmt-str (str)
  (replace-regexp-in-string "%" "%%" str))

(defun get-preamble ()
  (escape-fmt-str
   (with-temp-buffer
     (insert
      (with-current-buffer
          (find-file-noselect
           "preamble.html")
        (buffer-substring-no-properties (point-min)
                                        (point-max))))
     (buffer-string))))

(defun build-postamle (info &optional more)
  (let* ((spec (org-html-format-spec info))
         (date (cdr (assq ?d spec)))
         (author (cdr (assq ?a spec)))
         (email (cdr (assq ?e spec)))
         (creator (cdr (assq ?c spec)))
         (validation-link (cdr (assq ?v spec))))

    (concat
     "<script src=\"https://cdn.jsdelivr.net/npm/scittle@0.6.15/dist/scittle.js\" type=\"application/javascript\"></script>\n\n    <script crossorigin src=\"https://unpkg.com/react@17/umd/react.production.min.js\"></script>\n    <script crossorigin src=\"https://unpkg.com/react-dom@17/umd/react-dom.production.min.js\"></script>\n    <script src=\"https://cdn.jsdelivr.net/npm/scittle@0.6.15/dist/scittle.reagent.js\"> </script>\n\n\n\n    <script src=\"./navbar_toggle.js\"></script>\n    <script type=\"application/x-scittle\" src=\"navbar.cljs\"></script>\n    <script src=\"./pixel.js\"></script>\n    \n"
     
     ;; (concat
;;       "<script>var SCITTLE_NREPL_WEBSOCKET_PORT = 1340;</script>
;; <script src=\"https://cdn.jsdelivr.net/npm/scittle@0.6.15/dist/scittle.nrepl.js\"></script>")
     
     more
     (and (plist-get info :with-date)
          (org-string-nw-p date)
          (format
           "<p class=\"date\">%s: %s</p>
"
           (org-html--translate
            "Date"
            info)
           date))
     (and (plist-get info :with-email)
          (org-string-nw-p email)
          (format
           "<p class=\"email\">%s: %s</p>
"
           (org-html--translate
            "Email"
            info)
           email))
     "
<div><a href=\"/about.html\">About</a></div>"
     "<div><a href=\"/contact.html\">Contact</a></div>")))

(setq org-html-head-include-default-style
      t
      org-html-style-default
      "<style>
  pre {

    border: 1px solid #e6e6e6;
    border-radius: 3px;
    background-color: black;
    padding: 8pt;
    font-family: monospace;
    overflow: auto;
    margin: 1.2em;
  }
  pre.src {
    position: relative;
    overflow: auto;
  }
  pre.src:before {
    display: none;
    position: absolute;
    top: -8px;
    right: 12px;
    padding: 3px;
    color: #faf7f7;
    background-color: black;
  }

</style>
")

(setq org-publish-project-alist
      (list
       (list
        "org-site:main"
        :org-html-preamble t
        :html-postamble #'build-postamle
        :with-broken-links t
        :html-preamble-format `(("en" ,(get-preamble)))
        :recursive t
        :exclude ".*"
        :include (append (ftlm/post-files) (list ftlm/posts-file))
        :base-directory "~/notes/"
        :publishing-function 'org-html-publish-to-html
        :htmlize-output-type 'css
        :publishing-directory "./public/"
        :with-author nil
        :with-email t
        :with-creator t
        :with-toc t
        :section-numbers nil
        :time-stamp-file nil)
       (list
        "org-site:index"
        :org-html-preamble t
        :with-broken-links t

        :html-postamble (lambda (info)
                          (build-postamle
                           info
                           "<script type=\"application/x-scittle\" src=\"index-main.cljs\"></script>"))
        :html-preamble-format `(("en" ,(get-preamble)))
        :html-head (with-current-buffer (find-file-noselect "src/ftlmemes/index-head.html") (buffer-string))
        :base-extension "org"
        :base-directory "~/notes/"
        :publishing-function 'org-html-publish-to-html
        :htmlize-output-type 'css
        :publishing-directory "./public/"
        :exclude ".*"
        :include (list ftlm/index-file)
        :recursive nil
        :with-author nil
        :with-email t
        :with-creator t
        :with-toc t
        :section-numbers nil
        :time-stamp-file nil)))

(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head (with-current-buffer (find-file-noselect "html-head.html") (buffer-string)))

(defun denote-link-ol-export (link description format)
  "Export a `denote:' link from Org files.
The LINK, DESCRIPTION, and FORMAT are handled by the export
backend."
  (let* ((path-id (denote-link--ol-resolve-link-to-target link :path-id))
         (path (file-name-nondirectory (car path-id)))
         (p (file-name-sans-extension path))
	 (p (dw/strip-file-name-metadata p))
         (id (cdr path-id))
         (desc (or description (concat "denote:" id))))
    (cond
     ;; I also do not want target=_blank. I want _self (the default)
     ((eq format 'html) (format "<a href=\"%s.html\">%s</a>" p desc))
     ((eq format 'latex) (format "\\href{%s}{%s}" (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" path) desc))
     ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
     ((eq format 'ascii) (format "[%s] <denote:%s>" desc path)) ; NOTE 2022-06-16: May be tweaked further
     ((eq format 'md) (format "[%s](%s.md)" desc p))
     (t path))))

(org-publish-all t)

(dolist (file
         (append (directory-files-recursively "src" "\\.js$")
                 (directory-files-recursively "src" "\\.css$")
                 (directory-files-recursively "src/ftlmemes/cljs/" "\\.cljs$")))
  (copy-file file "public/" t))

(copy-file "assets/favicon.ico" "public/" t)
(copy-directory "src/ftlmemes/clojure_function_quiz/" "public/" t t)
(copy-directory "src/ftlmemes/flipcoin//" "public/" t t)

(with-temp-buffer
  (parseedn-print
   (mapcar #'ftlm/post-data (ftlm/post-files)))
  (with-current-buffer
      (let ((b (get-buffer-create "bb")))
        (call-shell-region
	 (point-min)
	 (point-max)
	 "bb src/ftlmemes/feed.clj"
	 nil
	 b)
        b)
    (message (buffer-string))
    (kill-buffer)))

(defun print-posts-list ()
  (with-current-buffer
      (find-file-noselect "public/posts-list.edn")
    (erase-buffer)
    (parseedn-print
     (ftlm/navbar-posts-denote-links))
    (save-buffer)))

(defun build-search-index ()
  (let ((dir "./public/search-index/"))
    (make-directory dir t)
    (cl-loop for f in (ftlm/posts+index-files)
             for p = (with-current-buffer
                         (find-file-noselect f)
                       (goto-char (point-min))
                       (re-search-forward "#\\+EXPORT_FILE_NAME:\\s-+\\(.*\\)")
                       (match-string 1))
             do (copy-file f (expand-file-name p dir) t))))

(print-posts-list)

(build-search-index)



