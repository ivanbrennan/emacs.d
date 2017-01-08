(require 'cl-lib)

(defvar drawer/modes nil
  "List of major modes that qualify a buffer for drawer storage.")

(defun drawer/toggle ()
  (interactive)
  (or (drawer//try-hide)
      (drawer//try-show)))

(defun drawer//try-hide ()
  (let ((win (cl-some #'drawer//detect-visible (buffer-list))))
    (when win
      (delete-window win) ;; try delete-side-window
      t)))

(defun drawer//try-show ()
  (let* ((buf (cl-some #'drawer//detect-hidden (buffer-list)))
         (win (if buf (display-buffer buf))))
    (if win (select-window win))))

(defun drawer//detect-visible (buf)
  (and (drawer//hold? buf)
       (get-buffer-window buf)))

(defun drawer//detect-hidden (buf)
  (and (drawer//hold? buf)
       (not (get-buffer-window buf 'visible))
       buf))

(defun drawer//hold? (buf)
  (and (compilation-buffer-p buf)
       (memq (buffer-local-value 'major-mode buf) drawer/modes)))

(provide 'drawer)
