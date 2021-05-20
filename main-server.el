;; helper function
(require 'f)

(defun directory-non-hidden-files (directory)
  "Return absolute path of all non-hidden & non-backup files in DIRECTORY.
This does not opreate recursively. Only files / directories under the current
directory is shown."
  (directory-files directory t
                   ;; dircard ".", "..", hidden, and emacs-backup files
                   (rx string-start (not ?.) (* anything) (not ?~) string-end)))

(defun get-directory-alist (directory)
  "Return an alist of all files under DIRECTORY recursively."
  (cons directory
        (let ((res (directory-non-hidden-files directory)))
          (mapcar (lambda (path)
                    (if (not (file-directory-p path))
                        path
                      (get-directory-alist path)))
                  res))))

(defun directory-alist-to-org-link-list (alist dep)
  "Print ALIST as org list.
ALIST is created with `get-directory-alist'.
The printed result is a list where .org files are displayed as link under
`eserver-root'. The result is intended to be captured with org code block."
  (dotimes (_ dep) (princ "  "))
  (princ (format "- =%s/=\n" (file-name-nondirectory (car alist))))
  (dolist (path (cdr alist))
    (if (consp path)
        (directory-alist-to-org-link-list path (1+ dep))
      (when (string-suffix-p ".org" path)
        (dotimes (_ (1+ dep)) (princ "  "))
        (princ (format "- [[http:/%s][=%s=]]\n" ; one slash, relative path
                       (f-relative path eserver-root)
                       (file-name-nondirectory path)))))))

;; main server start

(require 'simple-httpd)

(setq httpd-host "0.0.0.0")
(httpd-start)

(setq eserver-root (expand-file-name "~/eserver"))

(defun httpd/favicon.ico (proc path &rest args)
  (httpd-send-file proc (expand-file-name "favicon.ico" eserver-root)))

;; load all server.el under eserver-root
(mapc (lambda (file)
        (load file)
        (httpd-log `(loaded submodule ,file)))
      (directory-files-recursively eserver-root
                                   (rx string-start "server.el" string-end)))
