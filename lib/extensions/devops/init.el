;;; devops-init --- The entry point for devops extension
;;; Commentary:
;;; Code:

(defun extensions/devops-initialize ()
  "Initialize devops extension."
  (exec-path-from-shell-initialize)
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  (add-hook 'yaml-mode-hook (lambda () (ansible))))

(provide 'extensions/devops/init)
;;; init ends here.
