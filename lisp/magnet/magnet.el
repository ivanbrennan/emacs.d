;;; magnet.el --- Magnetic buffers -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar magnet-modes nil
  "List of major modes that qualify a buffer for magnetic treatment.")

(defvar magnet-names nil
  "List of name patterns that qualify a buffer for magnetic treatment.")

(defun magnet-toggle ()
  (interactive)
  (or (magnet--close)
      (magnet--open)))

(defun magnet--close ()
  (let ((win (cl-some #'magnet--detect-visible (buffer-list))))
    (when win
      (delete-window win) ;; try delete-side-window
      t)))

(defun magnet--open ()
  (let* ((buf (cl-some #'magnet--detect-hidden (buffer-list)))
         (win (if buf (display-buffer buf))))
    (if win (select-window win))))

(defun magnet--detect-visible (buf)
  (and (magnet--attract? buf)
       (get-buffer-window buf)))

(defun magnet--detect-hidden (buf)
  (and (magnet--attract? buf)
       (not (get-buffer-window buf 'visible))
       buf))

(defun magnet--attract? (buf)
  (or (magnet--attract-mode? buf)
      (magnet--attract-name? buf)))

(defun magnet--attract-mode? (buf)
  (memq (buffer-local-value 'major-mode buf) magnet-modes))

(defun magnet--attract-name? (buf)
  (member (buffer-name buf) magnet-names))

(provide 'magnet)
