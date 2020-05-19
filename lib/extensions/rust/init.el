;;; rust-init --- The entry point for Rust extension
;;; Commentary:
;;; Code:


;;;###autoload
(defun extensions/rust-initialize ()
  "Initialize Rust extension."
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


(provide 'extensions/rust/init)
;;; init.el ends here
