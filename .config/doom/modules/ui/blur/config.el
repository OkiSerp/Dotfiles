;;; ui/blur/config.el -*- lexical-binding: t; -*-

(defun serp/add-blur-behind-x-frame (&optional frame &rest _)
  "Set blur behind `x' frame.\n
If FRAME in nil, use current frame."
  (interactive)
  (let* ((frame (cond (frame) (t (selected-frame))))
         (frame-id (frame-parameter frame 'outer-window-id))
         (command (format
                   "xprop %s %s %s"
                   "-f _KDE_NET_WM_BLUR_BEHIND_REGION 32c"
                   "-set _KDE_NET_WM_BLUR_BEHIND_REGION 0 -id"
                   frame-id)))
    (when (eql (window-system) 'x)
      (call-process-shell-command command)
      (set-frame-parameter frame 'blur 1))))

(defun serp/remove-blur-behind-x-frame (&optional frame &rest _)
  "Remove blur behind `x' frame.\n
If FRAME is nil, use current frame."
  (interactive)
  (let* ((frame (cond (frame) (t (selected-frame))))
         (frame-id (frame-parameter frame 'outer-window-id))
         (command (format
                   "xprop -remove _KDE_NET_WM_BLUR_BEHIND_REGION -id %s"
                   frame-id)))
    (when (eql (window-system) 'x)
      (call-process-shell-command command)
      (set-frame-parameter frame 'blur 0))))

(defun serp/toggle-blur-behind-x-frame (&optional frame &rest _)
  "Toggle blur behind `x' frame.\n
If FRAME is nil, use current frame."
  (interactive)
  (let* ((frame (cond (frame) (t (selected-frame))))
         (blur (frame-parameter frame 'blur)))
    (if (or (eql blur nil) (<= blur 0))
        (serp/add-blur-behind-x-frame frame)
      (serp/remove-blur-behind-x-frame frame))))

(defun serp/add-blur-behind-new-x-frame-on-switch (&rest _)
  "Set blur behind newly created `x' frame.\n
Note: the function works perfectly on frame switch."
  (let ((blur (frame-parameter (selected-frame) 'blur)))
    (when (eql blur nil)
      (serp/add-blur-behind-x-frame))))

(when (modulep! +x)
  (add-to-list 'default-frame-alist '(alpha-background . 90))
  (add-hook 'window-setup-hook 'serp/add-blur-behind-x-frame)
  (add-hook! 'window-selection-change-functions
    (serp/add-blur-behind-new-x-frame-on-switch)))

(add-hook! 'window-setup-hook
  (map! :when (modulep! :ui blur +x)
        :leader :desc "Blur behind frame"
        "tu" 'serp/toggle-blur-behind-x-frame))
