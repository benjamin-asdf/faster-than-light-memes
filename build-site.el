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

(defvar ftlm/index-file "/home/benj/notes/20220923T161021--index__public.org")

(defun ftlm/file->denote-links (file)
  (with-current-buffer (find-file-noselect file)
    (when-let
	((regexp (denote-link--file-type-regexp (buffer-file-name))))
      (denote-link--expand-identifiers regexp))))

(defun ftlm/denote-file-data (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((res))
      (while (re-search-forward
	      "#\\+\\(.+?\\):\\s-+\\(.+?\\)$"
	      nil
	      t)
	(push
	 (cons (match-string 1)
	       (match-string 2))
	 res))
      res)))

(defun ftlm/posts ()
  (let ((files (ftlm/file->denote-links ftlm/index-file)))
    (mapcar
     (lambda (f)
       (cons (cons :path f) (ftlm/denote-file-data f)))
     files)))

(defvar posts (ftlm/posts))

(setq org-publish-project-alist
      (list
       (list "org-site:main"
             :recursive t
	     :exclude ".*"
	     :include (mapcar (lambda (lst) (cdr (assq :path lst))) posts)
             :base-directory "~/notes/"
             :publishing-function 'org-html-publish-to-html
             :publishing-directory "./public"
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

(defun dw/strip-file-name-metadata (file-name)
  (replace-regexp-in-string "^.*--\\(.*?\\)__.*$" "\\1" file-name))

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
     ((eq format 'html) (format "<a target=\"_blank\" href=\"%s.html\">%s</a>" p desc))
     ((eq format 'latex) (format "\\href{%s}{%s}" (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" path) desc))
     ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
     ((eq format 'ascii) (format "[%s] <denote:%s>" desc path)) ; NOTE 2022-06-16: May be tweaked further
     ((eq format 'md) (format "[%s](%s.md)" desc p))
     (t path))))

(with-temp-buffer
  (parseedn-print (ftlm/posts))
  (with-current-buffer
      (let ((b (get-buffer-create "bb")))
	(call-shell-region
	 (point-min)
	 (point-max)
	 "bb /home/benj/repos/faster-than-light-memes/src/ftlmemes/feed.clj"
	 nil
	 b)
	b)
    (message (buffer-string))))
