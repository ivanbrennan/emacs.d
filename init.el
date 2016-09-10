;; (package-initialize)

(let ((gc-cons-threshold most-positive-fixnum)
      (file-name-handler-alist nil))

  (load-file (concat user-emacs-directory "core.el")))
