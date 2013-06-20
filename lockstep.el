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
    (let ((a-lockstep-frame (when lockstep-frames (car lockstep-frames))))
      (push (selected-frame) lockstep-frames)
      (lockstep-frame a-lockstep-frame))))

(defun turn-off-lockstep ()
  "Turn off synchronization for this frame."
  (interactive)
  (setq lockstep-frames (remq (selected-frame) lockstep-frames)))

(defun lockstep-needed ()
  (setq lockstep-frames (remove-if-not 'frame-live-p lockstep-frames))
  (and (> (length lockstep-frames) 1)
       (memq (selected-frame) lockstep-frames)))

(defun lockstep-frame (&optional master-frame)
  "Synchronize window configurations of frames."
  (when (lockstep-needed)
    (remove-hook 'window-configuration-change-hook 'lockstep-frame)
    (unwind-protect
        (let* ((this-frame (or master-frame (selected-frame)))
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
                       (let ((this-frame-window (pop this-frame-windows))
                             (other-frame-window (pop other-frame-windows)))
                         (window-state-put (window-state-get this-frame-window) other-frame-window)
                         (when master-frame
                           ; if called from turn-on-lockstep, synchronize point as well
                           (set-window-point other-frame-window (window-point this-frame-window))))))))
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
            do (progn
                 (set-window-point window (point))
                 (set-frame-selected-window (window-frame window) window))))))

(defun lockstep-popup ()
  "Modify the popup library so that popups in a buffer are shown in all windows showing the buffer.

Invoke this if you want this behavior; it is useful for auto-complete.

It would be better to only show the popup in a window synchronized in another frame, but that does
not seem possible in Emacs 24."
  (interactive)
  (defun* popup-create (point
                        width
                        height
                        &key
                        min-height
                        around
                        (face 'popup-face)
                        mouse-face
                        (selection-face face)
                        (summary-face 'popup-summary-face)
                        scroll-bar
                        margin-left
                        margin-right
                        symbol
                        parent
                        parent-offset
                        keymap)
    "Create a popup instance at POINT with WIDTH and HEIGHT.

MIN-HEIGHT is a minimal height of the popup. The default value is
0.

If AROUND is non-nil, the popup will be displayed around the
point but not at the point.

FACE is a background face of the popup. The default value is POPUP-FACE.

SELECTION-FACE is a foreground (selection) face of the popup The
default value is POPUP-FACE.

If SCROLL-BAR is non-nil, the popup will have a scroll bar at the
right.

If MARGIN-LEFT is non-nil, the popup will have a margin at the
left.

If MARGIN-RIGHT is non-nil, the popup will have a margin at the
right.

SYMBOL is a single character which indicates a kind of the item.

PARENT is a parent popup instance. If PARENT is omitted, the
popup will be a root instance.

PARENT-OFFSET is a row offset from the parent popup.

KEYMAP is a keymap that will be put on the popup contents."
    (or margin-left (setq margin-left 0))
    (or margin-right (setq margin-right 0))
    (unless point
      (setq point
            (if parent (popup-child-point parent parent-offset) (point))))

    (save-excursion
      (goto-char point)
      (let* ((row (line-number-at-pos))
             (column (popup-current-physical-column))
             (overlays (make-vector height nil))
             (popup-width (+ width
                             (if scroll-bar 1 0)
                             margin-left
                             margin-right
                             (if symbol 2 0)))
             margin-left-cancel
             (window (selected-window))
             (window-start (window-start))
             (window-hscroll (window-hscroll))
             (window-width (window-width))
             (right (+ column popup-width))
             (overflow (and (> right window-width)
                            (>= right popup-width)))
             (foldable (and (null parent)
                            (>= column popup-width)))
             (direction (or
                         ;; Currently the direction of cascade popup won't be changed
                         (and parent (popup-direction parent))

                         ;; Calculate direction
                         (popup-calculate-direction height row)))
             (depth (if parent (1+ (popup-depth parent)) 0))
             (newlines (max 0 (+ (- height (count-lines point (point-max))) (if around 1 0))))
             current-column)
        ;; Case: no newlines at the end of the buffer
        (when (> newlines 0)
          (popup-save-buffer-state
            (goto-char (point-max))
            (insert (make-string newlines ?\n))))
        
        ;; Case: the popup overflows
        (if overflow
            (if foldable
                (progn
                  (decf column (- popup-width margin-left margin-right))
                  (unless around (move-to-column column)))
              (when (not truncate-lines)
                ;; Truncate.
                (let ((d (1+ (- popup-width (- window-width column)))))
                  (decf popup-width d)
                  (decf width d)))
              (decf column margin-left))
          (decf column margin-left))

        ;; Case: no space at the left
        (when (and (null parent)
                   (< column 0))
          ;; Cancel margin left
          (setq column 0)
          (decf popup-width margin-left)
          (setq margin-left-cancel t))
        
        (dotimes (i height)
          (let (overlay begin w (dangle t) (prefix "") (postfix ""))
            (when around
              (popup-vertical-motion column direction))
            (setq around t
                  current-column (popup-current-physical-column))

            (when (> current-column column)
              (backward-char)
              (setq current-column (popup-current-physical-column)))
            (when (< current-column column)
              ;; Extend short buffer lines by popup prefix (line of spaces)
              (setq prefix (make-string
                            (+ (if (= current-column 0)
                                   (- window-hscroll (current-column))
                                 0)
                               (- column current-column))
                            ? )))

            (setq begin (point))
            (setq w (+ popup-width (length prefix)))
            (while (and (not (eolp)) (> w 0))
              (setq dangle nil)
              (decf w (char-width (char-after)))
              (forward-char))
            (if (< w 0)
                (setq postfix (make-string (- w) ? )))

            (setq overlay (make-overlay begin (point)))
;            (overlay-put overlay 'window window)
            (overlay-put overlay 'dangle dangle)
            (overlay-put overlay 'prefix prefix)
            (overlay-put overlay 'postfix postfix)
            (overlay-put overlay 'width width)
            (aset overlays
                  (if (> direction 0) i (- height i 1))
                  overlay)))
        (loop for p from (- 10000 (* depth 1000))
              for overlay in (nreverse (append overlays nil))
              do (overlay-put overlay 'priority p))
        (let ((it (make-popup :point point
                              :row row
                              :column column
                              :width width
                              :height height
                              :min-height min-height
                              :direction direction
                              :parent parent
                              :depth depth
                              :face face
                              :mouse-face mouse-face
                              :selection-face selection-face
                              :summary-face summary-face
                              :margin-left margin-left
                              :margin-right margin-right
                              :margin-left-cancel margin-left-cancel
                              :scroll-bar scroll-bar
                              :symbol symbol
                              :cursor 0
                              :offset 0
                              :scroll-top 0
                              :current-height 0
                              :list nil
                              :newlines newlines
                              :overlays overlays
                              :keymap keymap)))
          (push it popup-instances)
          it))))
)

(provide 'lockstep)
