;;; rust-init --- The entry point for Rust extension
;;; Commentary:
;;; Code:


;;;###autoload
(defun extensions/rust-initialize ()
  "Initialize Rust extension."
  (setq rustic-format-trigger 'on-save))


(provide 'extensions/rust/init)
;;; init.el ends here
