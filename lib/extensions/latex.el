(require 'fpkg)
(require 'fg42/extension)
(require 'extensions/latex/init)

;; Dependencies ----------------------------------
;(depends-on 'lsp-mode)

;TODO: add to MELPA and remove lsp-latex.el file beside init.el
;(with-ability lsp-latex (depends-on 'lsp-latex))

(defun latex-doc ()
  "something fun")

;; Extension -------------------------------------
(extension latex
     :version "2."
     :on-initialize extensions/latex-initialize
     :docs "lib/extensions/latex/readme.org")

(provide 'extensions/latex)
