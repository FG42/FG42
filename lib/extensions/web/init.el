;; Functions -------------------------------------------------
;;;###autoload
(defun web-mode-hook-func ()
  "Hooks for Web mode."
  (with-ability code-completion
                (require 'company-web-html)
                (set (make-local-variable
                      'company-backends) '(company-web-html))

                (define-key web-mode-map (kbd "C-'") 'company-web-html))
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  (setq web-mode-markup-indent-offset 2))

;;;###autoload
(defun extensions/web-activate-modes ()
  "Activate necessary modes"
  (setq css-indent-offset 2)

  (with-ability rainbow
                (rainbow-mode t))

  (with-ability spell
                (flyspell-prog-mode))

  (with-ability flycheck
                (flycheck-mode t)))

;;;###autoload
(defun jsx ()
  "Activate web-mode for editing jsx."
  (interactive)

  (flycheck-define-checker jsxhint-checker
    "A JSX syntax and style checker based on JSXHint."

    :command ("jsxhint" source)
    :error-patterns
    ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
    :modes (web-mode))

  (add-hook 'web-mode-hook
            (lambda ()
              (when (equal web-mode-content-type "jsx")
                ;; enable flycheck
                (flycheck-select-checker 'jsxhint-checker)
                (flycheck-mode))))

  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))

  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)


  (web-mode))

;;;###autoload
(defun extensions/web-initialize ()
  "Web development plugin initialization."

  (ability web-editor ('flycheck)
           "Gives FG42 the ability to edit web contents."
           (require 'web-mode)

           (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
           (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
           (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
           (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
           (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
           (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
           (add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode))
           (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
           (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
           (add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode))
           (add-to-list 'auto-mode-alist '("\\.esx$" . js2-jsx-mode))

           (add-hook 'web-mode-hook 'web-mode-hook-func)
           (add-hook 'web-mode-hook 'emmet-mode)
           (add-hook 'web-mode-hook 'extensions/web-activate-modes))

  (ability css-editor
           "Gives FG42 the ability to edit CSS."
           (add-hook 'css-mode-hook 'extensions/web-activate-modes))

  (ability sass-editor
           "Gives FG42 the ability to edit SASS/SCSS."

           (add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
           (add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

           (setq scss-compile-at-save nil)
           (setq sass-compile-at-save nil)

           (add-hook 'scss-mode-hook 'extensions/web-activate-modes)
           (add-hook 'sass-mode-hook 'extensions/web-activate-modes))

  (ability less-editor
           "Gives FG42 the ability to edit lesscss."
           (add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))
           (add-hook 'less-css-mode-hook 'extensions/web-activate-modes))

  (ability js-editor
           "Gives FG42 the ability to edit Javascript."
           (add-hook 'js2-mode-hook 'extensions/web-activate-modes)
           (add-to-list 'auto-mode-alist '("\\.js.erb$" . js2-mode))
           (add-to-list 'auto-mode-alist '("\\.es$" . js2-mode))
           (add-to-list 'auto-mode-alist '("\\.es6$" . js2-mode))

           (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))
  (ability jsx-editor
           "Gives FG42 the ability to edit JSX."
           (add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode))))
(debug)
(message "'web' extension has been initialized.")



(provide 'extensions/web/init)
