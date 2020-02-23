;;; go-init --- The entry point for golang extension
;;; Commentary:
;;; Code:

(defun fg42-go-hook ()
  """Setup emacs hooks and turn necessary modes on."""
  (lsp)
  (flymake-mode-on)
  (yas-minor-mode-on)
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (local-set-key (kbd "M-.") #'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark))



(defun go-path () (concat (getenv "HOME") "/go"))
(defun go-path-binary () (concat (go-path) "/bin"))
(defun extensions/go-initialize ()
  """Initialize Golang extension ."""
   (add-to-list 'exec-path (go-path-binary))
   (add-hook 'go-mode-hook 'fg42-go-hook))

(provide 'extensions/go/init)

;;; init ends here.
