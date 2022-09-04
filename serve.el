(use-package simple-httpd
  :ensure t)

(httpd-serve-directory "./public")
(browse-url "http://localhost:8080")
