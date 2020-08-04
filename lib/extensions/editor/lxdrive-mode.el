;;; lxdrive-mode --- A minor mode for fast cursor movement
;;; Commentary:
;;; Code:

(require 'expand-region)

(setq original-global-map global-map)
(boundp 'lxdrive-minor-mode)

(defun turn-off-lxdrive ()
  "Toggle lxdrive mode."
  (interactive)
  (lxdrive-minor-mode nil)
  (setq lxdrive-minor-mode nil)
  (use-global-map original-global-map))

(defun turn-on-lxdrive ()
  "Toggle lxdrive mode."
  (interactive)
  (lxdrive-minor-mode t)
  (use-global-map lxdrive-mode-map))

(defun turn-off-and-command ()
  "Turn off the lxdrive mode and run the counsel command."
  (interactive)
  (turn-off-lxdrive)
  (counsel-M-x))

(defun switch-other ()
  "Switch to the most recent buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))


(defvar lxdrive-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Movement
    (define-key map (kbd "l") 'forward-char)
    (define-key map (kbd "j") 'backward-char)
    (define-key map (kbd "i") 'previous-line)
    (define-key map (kbd "k") 'next-line)
    (define-key map (kbd "u") 'backward-word)
    (define-key map (kbd "o") 'forward-word)
    (define-key map (kbd "n") 'backward-paragraph)
    (define-key map (kbd "m") 'forward-paragraph)
    (define-key map (kbd "TAB") 'indent-for-tab-command)
    (define-key map (kbd "`")      'cua-set-mark)
    (define-key map (kbd "=")      'er/expand-region)
    (define-key map (kbd "]")      'forward-page)
    (define-key map (kbd "[")      'backward-page)
    (define-key map (kbd "e")      'move-end-of-line)
    (define-key map (kbd "a")      'move-beginning-of-line)
    (define-key map (kbd "<f2>")   'go-to-line)
    (define-key map (kbd "C-TAB")  'other-window)
    (define-key map (kbd "M-TAB")  'switch-other)

    ;; Actions
    (define-key map (kbd "RET")   'newline)
    (define-key map (kbd "d")     'delete-char)
    (define-key map (kbd "<backspace>") 'delete-backward-char)
    (define-key map (kbd "y")   'cua-paste)
    (define-key map (kbd "C-w")   'kill-region)
    (define-key map (kbd "M-w")   'kill-ring-save)

    (define-key map (kbd "h")   'kill-and-join-forward)

    (define-key map (kbd "g") 'keyboard-quit)
    (define-key map (kbd "z")   'undo)
    (define-key map (kbd "/")   'undo)

    (define-key map (kbd "M-x")   'turn-off-and-command)
    (define-key map (kbd "C-x C-s") 'save-buffer)
    (define-key map (kbd "SPC") 'self-insert-command)

    ;;(define-key map (kbd "ESC ESC") 'turn-off-lxdrive)
    (define-key map (kbd "q") 'turn-off-lxdrive)
    (define-key map (kbd "M-SPC") 'turn-off-lxdrive)

    map)
  "Keymap for lxdrive-minor-mode.")

(define-minor-mode lxdrive-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :global t
  :lighter " lx")

;;(global-set-key (kbd "ESC ESC") 'turn-on-lxdrive)
(global-set-key (kbd "M-SPC") 'turn-on-lxdrive)
(global-set-key (kbd "M-TAB")  'switch-other)
(with-ability spaceline
  (spaceline-toggle-lxdrive-on))

(provide 'extensions/editor/lxdrive-mode)
;;; lxdrive-mode  ends here
