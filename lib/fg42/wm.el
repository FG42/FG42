;;; WM --- Enables FG42 as a window manager using EXWM
;;; Commentary:
;;; Code:
(require 'fpkg)


(defmacro when-wm (&rest body)
  "Run the BODY only if in wm mode."
  (if (string= (getenv "FG42_WM") "true")
      `(progn ,@body)
    nil))


(defmacro when-not-wm (&rest body)
  "Run the BODY only if in wm mode."
  (if (string= (getenv "FG42_WM") "true")
      nil
    `(progn ,@body)))


(defun disable-nlinum ()
  "Disable nlinum and fringe-mode."
  (fringe-mode -1)
  (nlinum-mode -1))


(defun run-program (path)
  "Execute the program at the given PATH."
  (start-process-shell-command path nil path))


(defun run-program-in-xterm (path &optional name)
  "Execute the program at the given PATH via xterm with the given NAME."
  (run-program (concat "xterm -e " path)))


(defun rename-x-window (&optional name)
  "Rename the current buffer to NAME."
  (interactive)
  (let ((new-name (or name (read-string "New name: "))))
    (exwm-workspace-rename-buffer new-name)))


(defability wm ()
  "Window manager ability for FG42."
  (depends-on 'exwm)

  (defun initialize-wm ()
    (when-wm
      (require 'exwm)
      (require 'exwm-config)
      (require 'exwm-systemtray)
      (exwm-config-ido)
      ;; Set the initial number of workspaces (they can also be created later).
      (setq exwm-workspace-number 10)

      ;; All buffers created in EXWM mode are named "*EXWM*". You may want to
      ;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
      ;; are run when a new X window class name or title is available.  Here's
      ;; some advice on this topic:
      ;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
      ;; + For applications with multiple windows (e.g. GIMP), the class names of
      ;    all windows are probably the same.  Using window titles for them makes
      ;;   more sense.
      ;; In the following example, we use class names for all windows except for
      ;; Java applications and GIMP.
      (add-hook 'exwm-update-class-hook
                (lambda ()
                  (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                              (string= "gimp" exwm-instance-name))
                    (exwm-workspace-rename-buffer exwm-class-name))))
      (add-hook 'exwm-update-title-hook
                (lambda ()
                  (when (or (not exwm-instance-name)
                            (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                            (string= "gimp" exwm-instance-name))
                    (exwm-workspace-rename-buffer exwm-title))))


      ;; Global keybindings can be defined with `exwm-input-global-keys'.
      ;; Here are a few examples:
      (setq exwm-input-global-keys
       `(
         ;; Bind "s-r" to exit char-mode and fullscreen mode.
         ([?\s-r] . exwm-reset)
         ([?\s-g] . keyboard-quit)
         ;; Bind "s-w" to switch workspace interactively.
         ([?\s-w] . exwm-workspace-switch)
         ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
         ,@(mapcar (lambda (i)
                     `(,(kbd (format "s-%d" i)) .
                       (lambda ()
                         (interactive)
                         (exwm-workspace-switch-create ,i))))
                   (number-sequence 0 9))
         ;; Bind "s-&" to launch applications ('M-&' also works if the output
         ;; buffer does not bother you).
         ([?\s-d] . (lambda (command)
                      (interactive (list (read-shell-command "$ ")))
                      (start-process-shell-command command nil command)))
         ;; Bind "s-<f2>" to "slock", a simple X display locker.
         ([s-f2] . (lambda ()
                     (interactive)
                     (start-process "" nil "/usr/bin/slock")))))
      ;; To add a key binding only available in line-mode, simply define it in
      ;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
      (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

      (push ?\C-c exwm-input-prefix-keys)

      ;; The following example demonstrates how to use simulation keys to mimic
      ;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
      ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
      ;; and DEST is what EXWM actually sends to application.  Note that both SRC
      ;; and DEST should be key sequences (vector or string).
      (setq exwm-input-simulation-keys
       `(
         ;; movement
         (,(kbd "C-b") . left)
         (,(kbd "M-b") . ,(kbd "C-<left>"))
         (,(kbd "C-f") . right)
         (,(kbd "M-f") . ,(kbd "C-<right>"))
         (,(kbd "C-p") . up)
         (,(kbd "C-n") . down)
         (,(kbd "C-a") . home)
         (,(kbd "C-e") . end)
         (,(kbd "M-v") . prior)
         (,(kbd "C-v") . next)
         (,(kbd "C-d") . delete)
         ;;(,(kbs "C-k") . [S-end delete])
         ;; navigation
         (,(kbd "C-c b") . ,(kbd "M-<left>"))
         (,(kbd "C-c f") . ,(kbd "M-<right>"))
         (,(kbd "C-c w") . ,(kbd "C-w"))
         (,(kbd "C-w") . ,(kbd "C-x"))
         (,(kbd "M-w") . ,(kbd "C-c"))
         (,(kbd "C-y") . ,(kbd "C-v"))
         ;; search
         (,(kbd "C-s") . ,(kbd "C-f"))))

      ;; You can hide the minibuffer and echo area when they're not used, by
      ;; uncommenting the following line.
      ;(setq exwm-workspace-minibuffer-position 'bottom)

      ;; Do not forget to enable EXWM. It will start by itself when things are
      ;; ready.  You can put it _anywhere_ in your configuration.
      (exwm-enable)
      (exwm-systemtray-enable)

      (with-ability nlinum
                    (add-hook 'exwm-mode-hook 'disable-nlinum)))))


(provide 'fg42/wm)
;;; wm.el ends here
