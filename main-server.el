;;; helper function

(require 'use-package)
(use-package f
  :ensure t)

(defun directory-non-hidden-files (directory)
  "Return absolute path of all non-hidden & non-backup files in DIRECTORY.
This does not opreate recursively. Only files / directories under the current
directory is shown."
  (directory-files directory t
                   ;; dircard ".", "..", hidden, and emacs-backup files
                   (rx string-start (not ?.) (* anything) (not ?~) string-end)))

(defun directory-tree (directory)
  "Return a tree of all files under DIRECTORY recursively."
  (cons directory
        (let ((res (directory-non-hidden-files directory)))
          (mapcar (lambda (path)
                    (if (not (file-directory-p path))
                        path
                      (directory-tree path)))
                  res))))

(defun directory-tree-to-org-link-list (tree dep)
  "Print TREE as org list.
TREE is created with `directory-tree'.  The printed result is a
list where .org files are displayed as link under
`eserver-root'. The result is intended to be captured with org
code block."
  (princ (format "%s- =%s/=\n"
                 (make-string (* 2 dep) ? )
                 (file-name-nondirectory (car tree))))
  (dolist (path (cdr tree))
    (if (consp path)
        (directory-tree-to-org-link-list path (1+ dep))
      (when (string-suffix-p ".org" path)
        (princ (format "%s- [[http:/%s][=%s=]]\n" ; one slash, relative path
                       (make-string (* 2 (1+ dep)) ? )
                       (f-relative path eserver-root)
                       (file-name-nondirectory path)))))))

;;; main server start

(require 'simple-httpd)

(defgroup eserver nil
  "Emacs server based on simple-httpd."
  :group 'comm)

(defcustom eserver-root (expand-file-name "~/eserver/")
  "Root directory of EServer."
  :group 'eserver
  :type 'directory)

(defvar eserver-site-descriptions '()
  "Alist of descriptions of sites under EServer.
This variable is added to by main-server.el at `eserver-root', or
server.el under subdirectories of `eserver-root'. For example:
  ((\"/\" . \"describe sites under EServer\")
   (\"/blog\" . \"this is a blog\"))")

(defvar eserver-icp-number nil
  "If non-nil, display ICP licensing number.")

(defvar eserver-police-number nil
  "Cons cell representing police licensing number.
E.g. (\"11010802020088\" . \"京公网安备 11010802020088 号\")")

(defun eserver-register-site (site description)
  "Register SITE with DESCRIPTION under EServer."
  (declare (indent defun))
  (setf (alist-get site eserver-site-descriptions nil nil 'string=)
        description))

(setq httpd-host "0.0.0.0")             ; listen for all IPV4 connections
(setq httpd-serve-files nil)            ; do not serve files

;;; load custom options before starting server, e.g.
;;;   change `httpd-port'
;;;   set ICP licensing number
(when (file-exists-p (expand-file-name "custom.el" eserver-root))
  (load (expand-file-name "custom.el" eserver-root)))

(httpd-start)

;;; / - describe cites under EServer

(eserver-register-site "/"
  "Describe sites under EServer.")

(defun eserver-request-get (key request)
  (alist-get key request "" nil 'string-equal))

(defun eserver-describe-sites ()
  (princ "Available sites:\n")
  (let* ((max-site-length             ; max length of site name
            (apply 'max (mapcar (lambda (site-cons)
                                  (length (car site-cons)))
                                eserver-site-descriptions)))
           ;; length of the first column (including spaces)
           (column-length (+ 6 max-site-length))
           (rows                        ; rows to be printed
            (sort ; sort list generated from `eserver-site-descriptions'
             (mapcar (lambda (site-cons)
                       ;; site-name spaces description
                       (format "<a href=\"%s\">%s</a>%s%s"
                               (car site-cons)
                               (car site-cons)
                               (make-string (- column-length
                                               (length (car site-cons)))
                                            ? ) ; space character
                               (cdr site-cons)))
                     eserver-site-descriptions)
             'string-lessp)))
      ;; print each row
      (mapc (lambda (row)
              (princ (format "  %s\n" row)))
            rows)
      nil))

(defun eserver-host-name (host)
  "Return proper hostname.
E.g. 127.0.0.1:8080 -> localhost
     what.blog.abc.xyz -> abc.xyz"
  (setq host (substring host 0 (string-match-p ":" host)))
  (if (or (string-equal host "localhost")
          (string-equal host "127.0.0.1"))
      "localhost"
    (string-join (last (split-string host "\\.") 2) ".")))

(defun eserver-insert-holiday-greeting ()
  "Insert holiday greeting"
  (let ((holidaies
         '(((4 . 1) . "Happy 愚人节"))))
    (let* ((dtime (decode-time (current-time)))
           (month (decoded-time-month dtime))
           (day (decoded-time-day dtime))
           (date (cons month day))
           (greeting (alist-get date holidaies nil nil 'equal)))
      (when greeting
        (insert (format "  %s\n" greeting))))))

(defun httpd/ (proc path query request)
  (with-httpd-buffer proc "text/html; charset=utf-8"
    (let ((host-name (eserver-host-name
                      (car (eserver-request-get "Host" request)))))
      (insert "<pre>")
      (insert "You are coming from host: "
              host-name ".\n")
      (eserver-describe-sites)
      (insert "\n")
      (insert (format-time-string "Server time: %Y-%m-%d %T\n"))
      (eserver-insert-holiday-greeting)
      (insert "</pre>\n<hr>\n")
      ;; add ICP licensing number at bottom
      (let ((icp-number (cdr (assoc-string host-name eserver-icp-number))))
        (when icp-number
          (insert "<a href=\"https://beian.miit.gov.cn/\" target=\"_blank\">"
                  icp-number
                  "</a><br>\n")))
      (when eserver-police-number
        (insert "<a target=\"_blank\" href=\"http://www.beian.gov.cn/portal/registerSystemInfo?recordcode="
                (car eserver-police-number)
                "\">\n  <img src=\"/beian.png\"/>"
                (cdr eserver-police-number)
                "\n</a><br>\n")))))

;;; beian.png
(defun httpd/beian.png (proc path &rest args)
  (httpd-send-file proc (expand-file-name "beian.png" eserver-root)))

;;; /favicon.ico

(eserver-register-site "/favicon.ico"
  "Favorite icon of this site.")

(defun httpd/favicon.ico (proc path &rest args)
  "Send favorite icon."
  (httpd-send-file proc (expand-file-name "favicon.ico" eserver-root)))

;;; /buffer

(eserver-register-site "/buffer"
  "Emacs buffer list.")

(defun httpd/buffer (proc path arguments &rest args)
  "Serve a list of Emacs buffers."
  (with-httpd-buffer proc "text/plain; charset=utf-8"
    (if (or (string-equal path "/buffer")
            (string-equal path "/buffer/"))
        (insert-buffer (list-buffers-noselect))
      (setq path (string-remove-prefix "/buffer/" path))
      (httpd-log `(buffer ,path))
      (when (get-buffer path)
        (insert-buffer path)))))

;;; /update

;; (eserver-register-site "/update"
;;   "Run the update script.")

;; (defun httpd/update (proc path parameters &rest args)
;;   (with-httpd-buffer proc "text/plain; charset=utf-8"
;;     (let ((shell-command-dont-erase-buffer t))
;;       (shell-command (expand-file-name "update.sh" eserver-root)
;;                      (current-buffer)))))

;;; load all server.el under eserver-root

(mapc (lambda (file)
        (load file)
        (httpd-log `(loaded submodule ,file)))
      (directory-files-recursively eserver-root
                                   (rx string-start "server.el" string-end)))
