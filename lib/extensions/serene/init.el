;;; serene-init --- The entry point for serene extension
;;; Commentary:
;;; Code:

;;;###autoload
(defun extensions/serene-initialize ()
  "Initialize the common Lisp extension."

  (require 'extensions/serene/serene-simple-mode)
  (add-hook 'serene-simple-mode-hook #'paredit-mode)
  (add-hook 'serene-simple-mode-hook #'rainbow-delimiters-mode)

  (with-ability parinfer ()
                (add-hook 'serene-simple-mode-hook #'parinfer-mode))

  (add-to-list 'auto-mode-alist
               '("\\.srns\\'" . serene-simple-mode))
  (add-to-list 'auto-mode-alist
               '("\\.srnl\\'" . serene-simple-mode)))



(provide 'extensions/serene/init)
;;; init.el ends here
