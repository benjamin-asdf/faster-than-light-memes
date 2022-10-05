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

(setf denote-directory "~/notes/")

(defvar ftlm/index-file "/home/benj/notes/20220923T161021--index__public.org")

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

(declare
 (ftlm/denote-file-data
  "/home/benj/notes/20220919T110439--binaural-beats-using-scittle__clojure.org"))

(defun ftlm/post-data (file)
  (cons (cons :path file) (ftlm/denote-file-data file)))

(defun ftlm/post-files ()
  (cl-remove-if
   (lambda (s)
     (string-match-p "org_archive$" s))
   (ftlm/file->denote-links ftlm/index-file)))

(defun ftlm/posts+index-files ()
  (cons ftlm/index-file (ftlm/post-files)))

(defun ftlm/posts (files) (mapcar #'ftlm/post-data files))

(defun note->path (lst) (cdr (assq :path lst)))

(dolist (path (ftlm/posts+index-files))
  (with-current-buffer
      (find-file-noselect path)
    (goto-char (point-min))
    (unless
	(re-search-forward "#\\+EXPORT_FILE_NAME:" nil t)
      (goto-char (point-min))
      (re-search-forward "^$")
      (insert
       (format "#+EXPORT_FILE_NAME: %s\n" (dw/strip-file-name-metadata path))))))

(setq org-publish-project-alist
      (list
       (list "org-site:main"
             :recursive t
	     :exclude ".*"
	     :include (ftlm/posts+index-files)
             :base-directory "~/notes/"
             :publishing-function 'org-html-publish-to-html
             :publishing-directory "./gh-pages/"
             :with-author nil
             :with-creator t
             :auto-sitemap t
             :with-toc t
             :section-numbers nil
             :time-stamp-file nil)))

;; Customize the HTML output
(setq org-html-validation-link t)

(setq org-html-validation-link nil            ;; Don't show validation link
      org-html-head-include-scripts nil       ;; Use our own scripts
      org-html-head-include-default-style nil ;; Use our own styles
      org-html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />")

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
