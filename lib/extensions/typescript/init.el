;;; TypescriptExtension --- Enables Typescript development on FG42
;;; Commentary:
;;; Code:

(defun setup-tide-mode ()
  (interactive)
  (require 'typescript-mode)
  (require 'web-mode)
  (setq tmp-directory (concat (getenv "HOME") "/.tmp"))
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  (flycheck-mode 1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  (eldoc-mode 1)
  (company-mode 1))


(defun lsp-typescript-config ()
  (interactive)
  (require 'lsp)
  (require 'lsp-clients))

;;;###autoload
(defun extensions/typescript-initialize ()
  "Initialize the typescript extension."
  (ability typescript-editor ('flycheck)
           (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

           (add-hook 'web-mode-hook
                     (lambda ()
                       (when (string-equal "tsx" (file-name-extension buffer-file-name))
                         (setup-tide-mode))))

           (add-hook 'typescript-mode-hook #'lsp-typescript-config)
           ;; enable typescript-tslint checker
           (flycheck-add-mode 'typescript-tslint 'web-mode)))


(provide 'extensions/typescript/init)
;;; init.el ends here
