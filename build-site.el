(require 'ox-publish)

;; Set the package installation directory so that packages aren't stored in the
;; ~/.emacs.d/elpa path.
(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install dependencies
(package-install 'htmlize)
(package-install 'denote)
(package-install 'parseedn)
(require 'denote)
(require 'parseedn)

(setf user-mail-address "Benjamin.Schwerdtner@gmail.com")

(setf make-backup-files nil)

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

;; (declare
;;  (ftlm/denote-file-data
;;   "/home/benj/notes/20220919T110439--binaural-beats-using-scittle__clojure.org"))

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
   (list ftlm/index-file)
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
  (format "<li><a href=\"%s.html\">%s</a></li>\n" link desc))

(defun get-preamble ()
  (concat
   (with-current-buffer
       (find-file-noselect
        "preamble.html")
     (buffer-string))
   (format
    "<style>
#navbar {
  display: none;
}

#navbar.is-open {
  display: block;
}

#navbar a {
  padding: 6px 8px 6px 16px;
  text-decoration: none;
  font-size: 20px;
  display: block;
  border-radius: 5px;
}

#navbar a:hover {
    color: #F689FF;
    background-color: black;
    font-weight: bold;
}

#navbar li {
    list-style-type: none;
}

#navbar ul {
    padding-inline-start: 0;
}
</style>
<div>
  <button id=\"navbar-toggle\">☰</button>
<ul id=\"navbar\">
    %s
</ul>
 </div>
"
    (with-temp-buffer
      (cl-loop
       for
       elm
       in
       (with-current-buffer
           (find-file-noselect
            ftlm/posts-file)
         (goto-char (point-min))
         (cl-loop
          while (re-search-forward
                 org-link-any-re
                 nil
                 t)
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
            (navbar-elm p description))))
       do
       (insert elm))
      (buffer-string)))))

;; </body>
;;  <script src="./darkmode-button.js"></script>
;; </html>

(defun build-postamle (info)
  (let* ((spec (org-html-format-spec info))
         (date (cdr (assq ?d spec)))
         (author (cdr (assq ?a spec)))
         (email (cdr (assq ?e spec)))
         (creator (cdr (assq ?c spec)))
         (validation-link (cdr (assq ?v spec))))
    (concat
     "<script src=\"navbar_toggle.js\"></script>\n"
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
           email)))))

(setq org-publish-project-alist
      (list
       (list
        "org-site:main"
        :org-html-preamble t
        :html-postamble #'build-postamle
        :html-preamble-format `(("en" ,(get-preamble)))
        :recursive t
        :exclude ".*"
        :include (ftlm/post-files)
        :base-directory "~/notes/"
        :publishing-function 'org-html-publish-to-html
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
        :html-postamble #'build-postamle
        :html-preamble-format `(("en" ,(get-preamble)))
        :html-head (with-current-buffer (find-file-noselect "src/ftlmemes/index-head.html") (buffer-string))
        :base-extension "org"
        :base-directory "~/notes/"
        :publishing-function 'org-html-publish-to-html
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

(setq org-html-validation-link
      nil
      org-html-head-include-scripts
      nil
      org-html-head-include-default-style
      nil
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
    (message (buffer-string))))
