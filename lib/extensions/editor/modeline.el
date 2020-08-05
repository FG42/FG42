(ability spaceline (flycheck)
         "A really cool mode line alternative which borrowed from awesome spacemacs"
         (require 'spaceline-config)
         (require 'extensions/editor/spaceline-alt)

         ;; TODO: Move this to somewhere propriate
         ;; Modeline indicator for lxdrive
         (spaceline-define-segment lxdrive
           "lxdrive indicator on spaceline."
           (if (and (boundp 'lxdrive-minor-mode) lxdrive-minor-mode)
               (all-the-icons-faicon  "arrows"  :height 0.8 :v-adjust 0.15 :face 'all-the-icons-lgreen)
             (all-the-icons-faicon "pencil" :height 0.8 :v-adjust 0.15))
           :tight t)

         (spaceline-compile
           "ati"
           '(((lxdrive) :face highlight-face :skip-alternate t)
             ((ati-projectile ati-mode-icon ati-buffer-id) :face default-face)
             ((ati-process ati-region-info) :face highlight-face :separator " | ")
             ((ati-modified ati-window-numbering ati-buffer-size ati-position) :face highlight-face :skip-alternate t)
             ((ati-flycheck-status ati-(point)ackage-updates purpose) :separator " | " :face other-face))
           ;; ((minor-modes) :face default-face)


           '(((ati-vc-icon " ") :face default-face :skip-alternate t :tight t)))

         (setq-default mode-line-format '("%e" (:eval (spaceline-ml-ati)))))




(ability doom-modeline

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

     (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)
     (setq doom-modeline-height 15)
     (setq doom-modeline-buffer-encoding nil)
     (setq doom-modeline-lsp nil)
     (setq doom-modeline-mu4e nil)
     (setq doom-modeline-irc nil)
     (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
     (doom-modeline-mode t))


(provide 'extensions/editor/modeline)
