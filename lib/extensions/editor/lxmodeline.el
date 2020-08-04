;;; lxmode-line --- A small utility library to toggle the modeline.
;;; Commentary:
;;; Code:
(require 'doom-modeline)

(doom-modeline-def-segment lxdrive-info
  "Show the status of lxdrive mode"
  (if (and (boundp 'lxdrive-minor-mode) lxdrive-minor-mode)
      (list " " (all-the-icons-faicon  "arrows"  :height 0.8 :v-adjust 0.15 :face 'all-the-icons-lgreen))
    (list " " (all-the-icons-faicon "pencil" :height 0.8 :v-adjust 0.15))))


(doom-modeline-def-modeline 'fg42-mode-line
  '(bar lxdrive-info matches buffer-info buffer-position parrot selection-info)
  '(process vcs checker))


(defun setup-custom-doom-modeline ()
  "Setup fg42 modeline."
  (doom-modeline-set-modeline 'fg42-mode-line 'default))

(with-ability doom-modeline
              (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)
              (setq doom-modeline-height 15)
              (setq doom-modeline-buffer-encoding nil)
              (setq doom-modeline-lsp nil)
              (setq doom-modeline-mu4e nil)
              (setq doom-modeline-irc nil)
              (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
              (doom-modeline-mode t))

(provide 'extensions/editor/lxmodeline)
;;; lxmodeline ends here
