;;; cpp-init --- The entry point for CPP extension
;;; Commentary:
;;; Code:


;;;###autoload
(defun extensions/cpp-initialize ()
  "Initialize Rust extension."
  (add-hook 'c++-mode-hook
            (lambda ()
              (ability lsp-c++ ('lsp)
                       (require 'lsp)
                       (require 'lsp-ui)
                       (lsp-deferred)))))



(provide 'extensions/cpp/init)
;;; init.el ends here
