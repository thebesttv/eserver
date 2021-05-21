;; helper function
(require 'f)

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
  (dotimes (_ dep) (princ "  "))
  (princ (format "- =%s/=\n" (file-name-nondirectory (car tree))))
  (dolist (path (cdr tree))
    (if (consp path)
        (directory-tree-to-org-link-list path (1+ dep))
      (when (string-suffix-p ".org" path)
        (dotimes (_ (1+ dep)) (princ "  "))
        (princ (format "- [[http:/%s][=%s=]]\n" ; one slash, relative path
                       (f-relative path eserver-root)
                       (file-name-nondirectory path)))))))

;; main server start

(require 'simple-httpd)

(defgroup eserver nil
  "Emacs server based on simple-httpd."
  :group 'comm)

(defcustom eserver-root (expand-file-name "~/eserver/")
  "Root directory of EServer."
  :group 'eserver
  :type 'directory)

(setq httpd-host "0.0.0.0")             ; listen for all IPV4 connections
(setq httpd-serve-files nil)            ; do not serve files
(httpd-start)

(defun httpd/favicon.ico (proc path &rest args)
  "Serve file /favicon.ico."
  (httpd-send-file proc (expand-file-name "favicon.ico" eserver-root)))

(defun httpd/buffer (proc path arguments &rest args)
  "Serve a list of Emacs buffers."
  (with-httpd-buffer proc "text/plain"
    (if (or (string-equal path "/buffer")
            (string-equal path "/buffer/"))
        (insert-buffer (list-buffers-noselect))
      (setq path (string-remove-prefix "/buffer/" path))
      (httpd-log `(buffer ,path))
      (when (get-buffer path)
        (insert-buffer path)))))

;; load all server.el under eserver-root

(mapc (lambda (file)
        (load file)
        (httpd-log `(loaded submodule ,file)))
      (directory-files-recursively eserver-root
                                   (rx string-start "server.el" string-end)))
