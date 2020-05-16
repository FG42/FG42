;;; TypescriptExtension --- Enables Typescript development on FG42
;;; Commentary:
;; In order to user typescript extension `typescript' itself should
;; be available on the path provided by `exec-path'.
;;; Code:

(require 'fpkg)
(require 'fg42/extension)
(require 'extensions/typescript/init)

;; Dependencies ----------------------------------
(depends-on 'typescript-mode)


(defun typescript-doc ()
  "Something fun.")

;; Extension -------------------------------------
(extension typescript
           :version "2.32"
           :on-initialize extensions/typescript-initialize
           :docs "lib/extensions/typescript/readme.org")

(provide 'extensions/typescript)
;;; typescript.el ends here
