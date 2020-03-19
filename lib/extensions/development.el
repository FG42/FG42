(require 'fpkg)
(require 'fg42/extension)
(require 'extensions/development/init)

;; Dependencies ----------------------------------
(depends-on 'company)
(depends-on 'company-statistics)
(depends-on 'projectile)
(depends-on 'yasnippet)
(depends-on 'yasnippet-snippets)
(depends-on 'smart-mode-line)

(depends-on 'quickrun)
(depends-on 'dash)
(depends-on 'websocket)

(with-ability parinfer
              (depends-on 'parinfer))

(with-ability terraform
              (depends-on 'terraform-mode))

(with-ability yaml
              (depends-on 'yaml-mode))
;; (with-ability hl
;;               (depends-on 'hl-sexp))

(with-ability dumb-jump
              (depends-on 'dumb-jump))

(with-ability bookmarks
              (depends-on 'bm))

(with-ability git
              (depends-on 'diff-hl)
              (depends-on 'magit)
              (depends-on 'gh)
              (depends-on 'evil-magit))

(with-ability github
              (depends-on 'magithub))

(with-ability focus
              (depends-on 'focus))

(with-ability code-browser
              (depends-on 'neotree))

(with-ability pt
              (depends-on 'pt))

(with-ability spell
              (depends-on 'flyspell))

(with-ability file-browser
              (depends-on 'ranger))

(with-ability shell
              (depends-on 'eshell-prompt-extras)
              (depends-on 'shell-pop))

(with-ability imenu
              (depends-on 'imenu)
              (depends-on 'imenu-list)
              (depends-on 'imenu-anywhere))

(with-ability lsp
              (depends-on 'lsp-mode)
              (depends-on 'lsp-ui)
              (depends-on 'company-lsp))

(with-ability dap
              (depends-on 'dap-mode))

;; TODO: Add flycheck-color-modebar
;; TODO  Add flycheck-tip

;; Extension -------------------------------------
(extension development
	   :version "2.31"
	   :on-initialize extension/development-initialize)

(provide 'extensions/development)
