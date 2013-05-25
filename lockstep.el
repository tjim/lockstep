; lockstep.el
; Trevor Jim
; Keep frames in lock step, useful for pair programming with emacsclients
; Requires Emacs 24

(require 'cl)

(add-hook 'post-command-hook 'lockstep-point)
(add-hook 'window-configuration-change-hook 'lockstep-frame)

(defvar lockstep-frames nil)

(defun lockstep ()
  "Synchronize this frame's windows and points."
  (interactive)
  (turn-on-lockstep))

(defun turn-on-lockstep ()
  "Synchronize this frame's windows and points."
  (interactive)
  (when (not (memq (selected-frame) lockstep-frames))
    (push (selected-frame) lockstep-frames)
    (lockstep-frame)))

(defun turn-off-lockstep ()
  "Turn off synchronization for this frame."
  (interactive)
  (setq lockstep-frames (remq (selected-frame) lockstep-frames)))

(defun lockstep-needed ()
  (setq lockstep-frames (remove-if-not 'frame-live-p lockstep-frames))
  (and (> (length lockstep-frames) 1)
       (memq (selected-frame) lockstep-frames)))

(defun lockstep-frame ()
  "Synchronize window configurations of frames."
  (when (lockstep-needed)
    (remove-hook 'window-configuration-change-hook 'lockstep-frame)
    (unwind-protect
        (let* ((this-frame (selected-frame))
               (other-frames (remove-if (lambda (frame) (equal this-frame frame)) lockstep-frames)))
          (loop for other-frame in other-frames
                do (let ((this-frame-windows
                          (remove-if-not 'window-live-p
                                         (remove-if 'window-minibuffer-p (window-list this-frame))))
                         (other-frame-windows
                          (remove-if-not 'window-live-p
                                         (remove-if 'window-minibuffer-p (window-list other-frame)))))

                                        ; force this-frame and other-frame to have the same number of windows
                     (while (not (equal (length this-frame-windows) (length other-frame-windows)))
                       (when (< (length this-frame-windows) (length other-frame-windows))
                         (delete-window (pop other-frame-windows)))
                       (when (> (length this-frame-windows) (length other-frame-windows))
                         (split-window (car other-frame-windows))
                         (setq other-frame-windows (remove-if 'window-minibuffer-p (window-list other-frame)))))

                                        ; force this-frame-windows and other-frame-windows to have same configurations
                     (while this-frame-windows
                       (window-state-put (window-state-get (pop this-frame-windows)) (pop other-frame-windows))))))
      (add-hook 'window-configuration-change-hook 'lockstep-frame))))

(defun lockstep-point ()
  "Synchronize point in all windows in other lockstep frames visiting this buffer."
  (when (lockstep-needed)
    (let* ((this-frame (selected-frame))
           (other-frames (remove-if (lambda (frame) (equal this-frame frame)) lockstep-frames)))
      (loop for window in
            (remove-if-not (lambda (window)
                             (and (window-live-p window)
                                  (equal (window-buffer window) (current-buffer))))
                           (loop for frame in other-frames nconcing (window-list frame)))
            do (set-window-point window (point))))))

(provide 'lockstep)
