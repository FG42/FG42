;;; RustExtention --- Enable Rust support in FG42
;;; Commentary:
;;; Code:
(require 'fpkg)
(require 'fg42/extension)
(require 'extensions/rust/init)

;; dependencies
(depends-on 'rust-mode)
(depends-on 'cargo)
(depends-on 'flycheck-rust)
(extension rust
           :version 0.0.1
           :on-initialize extensions/rust-initialize
           :docs "lib/extensions/rust/readme.org")
(provide 'extensions/rust)
;;; rust.el ends here
