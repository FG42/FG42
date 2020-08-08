;;; development-extension --- A general extension for general development
;;; Commentary:
;;; Code:

;; Functions -------------------------------------------------
;;;###autoload
(defun disable-projectile ()
  (interactive)
  (projectile-global-mode nil))

;; Quick fix for company-mode and yasnippet clashing
(defun company-yasnippet-or-completion ()
  (interactive)
  (if (yas/expansion-at-point)
      (progn (company-abort)
             (yas/expand))
    (company-complete-common)))

(defun yas/expansion-at-point ()
  "Tested with v0.6.1. Extracted from `yas/expand-1'"
  (first (yas/current-key)))


(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;;;###autoload
(defun enable-pt-search ()
  (interactive)
  (define-key projectile-mode-map (kbd "C-c p s s") 'projectile-pt)
  (define-key projectile-mode-map (kbd "C-c p s r") 'pt-regexp))


(defun load-necessary-modes-and-keybindings ()
  "Load the modes and keybindings which at necessary and are not part ofcourse an ability."
  (which-function-mode))


;;;###autoload
(defun extension/development-initialize ()
  "Development plugin initialization."
  (load-necessary-modes-and-keybindings)

  (ability project-config ()
           "Makes projects configurable."
           (require 'projects/configuration))

  (ability pretty-symbols ()
           "Replace some symbols with icons"
           (global-prettify-symbols-mode 1))


  (ability lsp ()
           "LSP integration for FG42"
           (require 'lsp-mode)
           (require 'lsp-ui-imenu)

           ;; Disabling inline actions. Accessable via lsp-execute-code-action
           (setq lsp-ui-sideline-show-code-actions nil)
           (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
           (setq lsp-ui-sideline-ignore-duplicate t)
           (setq lsp-prefer-flymake nil)
           (add-hook 'lsp-mode-hook 'lsp-ui-mode))

  (ability dap ('lsp)
           (dap-mode 1)
           (dap-ui-mode 1))

  (ability bookmarks ()
           (setq bm-restore-repository-on-load t)

           (require 'bm)

           (cheatsheet-add :group '--Development--
                           :key   "M-p"
                           :description "Toggle bookmarks")

           (cheatsheet-add :group '--Development--
                           :key   "M-]"
                           :description "Jump to next bookmark")

           (cheatsheet-add :group '--Development--
                           :key   "M-["
                           :description "Jump to previous bookmark")

           (global-set-key (kbd "M-p") 'bm-toggle)
           (global-set-key (kbd "M-]") 'bm-next)
           (global-set-key (kbd "M-[") 'bm-previous)

           (setq bm-restore-repository-on-load t)
           (setq bm-in-lifo-order t)
           (setq bm-cycle-all-buffers t)
           (setq-default bm-buffer-persistence t)

           (setq bm-repository-file (locate-user-emacs-file "bm-repository"))

           ;; (add-hook' after-init-hook 'bm-repository-load)
           ;; (add-hook 'kill-buffer-hook #'bm-buffer-save)
           ;; ;; Restoring bookmarks
           ;; (add-hook 'find-file-hooks   #'bm-buffer-restore)
           ;; (add-hook 'after-revert-hook #'bm-buffer-restore)

           ;; The `after-revert-hook' is not necessary to use to achieve persistence,
           ;; but it makes the bookmark data in repository more in sync with the file
           ;; state. This hook might cause trouble when using packages
           ;; that automatically reverts the buffer (like vc after a check-in).
           ;; This can easily be avoided if the package provides a hook that is
           ;; called before the buffer is reverted (like `vc-before-checkin-hook').
           ;; Then new bookmarks can be saved before the buffer is reverted.
           ;; Make sure bookmarks is saved before check-in (and revert-buffer)
           ;; (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
           (add-hook' after-init-hook 'bm-repository-load)

           (add-hook 'kill-emacs-hook '(lambda nil
                                         (bm-buffer-save-all)
                                         (bm-repository-save))))

  (ability livemd ()
           "Live markdown preview."
           (require 'extensions/development/livemd))

  (ability imenu ()
           "IMenu integration for FG42"
           (require 'imenu)
           (require 'imenu-list)

           (cheatsheet-add :group '--Development--
                           :key   "C-'"
                           :description "Toggle IMenu list which shows the symbol definitions based on major mode.")

           (cheatsheet-add :group '--Development--
                           :key   "C-<2>"
                           :description "Search for the definition of the symbol you want anywhere.")

           ;; (imenu-list-minor-mode)
           (setq imenu-list-focus-after-activation t)
           (setq imenu-list-auto-resize t)
           (global-set-key (kbd "C-'") #'imenu-list-smart-toggle)
           (global-set-key (kbd "C-<f2>") #'imenu-anywhere)
           (global-set-key (kbd "<f13>") #'imenu-anywhere))


  (ability parinfer ()
           (add-hook 'emacs-lisp-mode-hook #'parinfer-mode))

  (ability dumb-jump ()
           "Easily jump to defination for most langs using ag"
           (cheatsheet-add :group '--Development--
                           :key   "M-g o"
                           :description "Jump to definition in another window")
           (cheatsheet-add :group '--Development--
                           :key   "C-u y or M-g j"
                           :description "Jump to definition in current buffer")
           (cheatsheet-add :group '--Development--
                           :key   "C-u i or M-g x"
                           :description "Jump to definition using an external tool")
           (cheatsheet-add :group '--Development--
                           :key   "M-g z"
                           :description "Jump to definition in another window using an external tool")

           (require 'dumb-jump)
           (dumb-jump-mode t)
           (define-key dumb-jump-mode-map (kbd "M-g o") 'dumb-jump-go-other-window)
           (define-key dumb-jump-mode-map (kbd "M-g j") 'dumb-jump-go)
           (define-key dumb-jump-mode-map (kbd "C-u y") 'dumb-jump-go)
           (define-key dumb-jump-mode-map (kbd "M-g x") 'dumb-jump-go-prefer-external)
           (define-key dumb-jump-mode-map (kbd "C-u i") 'dumb-jump-go-prefer-external)
           (define-key dumb-jump-mode-map (kbd "M-g z") 'dumb-jump-go-prefer-external-other-window))

  (ability git ()
           "A wonderful git interface for FG42"

           (cheatsheet-add :group '--Development--
                           :key   "C-x g"
                           :description "Rise up MAGIT. Git interface for FG42")
           (global-set-key (kbd "C-x g") 'magit-status)
           (when (is-evil?)
             (add-hook 'magit-mode-hook (lambda () (require 'evil-magit)))
             (defkey global-map 'magit-status :evil (:normal "SPC g s"))))


  (ability github ()
           "Github support"
           (require 'magithub)
           (magithub-feature-autoinject t))

  ;; (ability hl ()
  ;;          "Highligh the current block of code. This ability may slows you down."
  ;;          (require 'hl-sexp)
  ;;          (add-hook 'prog-mode-hook #'hl-sexp-mode))

  (ability code-completion ()
           "Use company mode to provides a complete auto completion framwork."
           (require 'company)
           (global-company-mode t)

           ;; Bigger popup window
           (setq company-tooltip-limit 20)

           ;; Align annotations to the right tooltip border
           (setq company-tooltip-align-annotations 't)

           ;; Decrease delay before autocompletion popup shows
           (setq company-idle-delay 0.1)
           (setq company-minimum-prefix-length 2)

           ;; Start autpocompletion only after typing
           (setq company-begin-commands '(self-insert-command))

           ;; Force complete file names on "C-c /" key
           (global-set-key (kbd "C-c /") 'company-files)

           (add-hook 'after-init-hook 'company-statistics-mode)
           (define-key company-active-map "\t" 'company-yasnippet-or-completion)

           (setq dabbrev-case-fold-search t)

           (add-to-list 'company-backends 'company-dabbrev)
           (add-to-list 'company-backends 'company-dabbrev-code))

  (ability yas ()
           "Snippet configuration."
           (let ((snippet_home (concat (file-name-directory
                                        (locate-library "yasnippet-snippets"))
                                       "snippets"))
                 (my_snippet (concat fg42-home "/lib/snippets")))
             (setq yas-snippet-dirs (list my_snippet snippet_home)))

           (yas-global-mode 1))

  (ability project-management ()
           "Ability to manage projects and project navigation."
           (projectile-global-mode)
           (setq projectile-enable-caching t))

  (ability spell ()
           "Check spell of any word using ispell. This ability may slows you down"
           (global-set-key (kbd "<f2>") 'ispell-word)
           (setq flyspell-issue-message-flg nil))

  (ability diff-highlight ()
           "Highlight the diffs based on VCS."
           (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
           (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

  (ability auto-pair ()
           "Auto pair stuffs like brackets begin/ends etc."
           ;; TODO: use autopair mode if electric pair was not as good as autopair
           (electric-pair-mode))

  (ability yaml ()
           "YAML editor."
           (require 'yaml-mode)
           (add-hook 'yaml-mode-hook (lambda () (interactive) (whitespace-mode 1)))
           (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
           (add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode)))

  (ability terraform ()
           "Terraform editor."
           (require 'terraform-mode)
           (add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode)))

  (ability code-browser ()
           "Adds the code browser to FG42."
           (require 'neotree)
           (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
           (cheatsheet-add :group '--Development--
                           :key   "f8"
                           :description "Toggle project browser sidebar. See ProjectBrowser")

           (cheatsheet-add :group '--ProjectBrowser--
                           :key   "n"
                           :description "Next line")
           (cheatsheet-add :group '--ProjectBrowser--
                           :key   "p"
                           :description "Previous line")
           (cheatsheet-add :group '--ProjectBrowser--
                           :key   "g"
                           :description "Refresh the tree")
           (cheatsheet-add :group '--ProjectBrowser--
                           :key   "A"
                           :description "Maximize/Minimize the project browser")
           (cheatsheet-add :group '--ProjectBrowser--
                           :key   "H"
                           :description "Toggle display hidden files")
           (cheatsheet-add :group '--ProjectBrowser--
                           :key   "C-c C-n"
                           :description "Create a file or create a directory if filename ends with a '/'")
           (cheatsheet-add :group '--ProjectBrowser--
                           :key   "C-c C-d"
                           :description "Delete a file or a directory.")
           (cheatsheet-add :group '--ProjectBrowser--
                           :key   "C-c C-r"
                           :description "Rename a file or a directory.")
           (cheatsheet-add :group '--ProjectBrowser--
                           :key   "C-c C-c"
                           :description "Chande root directory.")

           (global-set-key [f8] 'neotree-toggle))

  (ability shell ()
           "Eshell enhancements."

           (cheatsheet-add :group '--Development--
                           :key   "M-`'"
                           :description "Brings up the eshell")

           (custom-set-variables
            ;; custom-set-variables was added by Custom.
            ;; If you edit it by hand, you could mess it up, so be careful.
            ;; Your init file should contain only one such instance.
            ;; If there is more than one, they won't work right.
            '(shell-pop-default-directory "$HOME")
            ;;'(shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
            '(shell-pop-shell-type (quote ("eshell" "*shell*" (lambda nil (eshell shell-pop-term-shell)))))
            ;;'(shell-pop-term-shell "/bin/zsh")
            '(shell-pop-term-shell "eshell")

            '(shell-pop-window-size 30)
            '(shell-pop-full-span t)
            '(shell-pop-window-position "full")))

           ;; (require 'eshell-prompt-extras)
           ;; (with-eval-after-load "esh-opt"
           ;;   (autoload 'epe-theme-lambda "eshell-prompt-extras")
           ;;   (setq eshell-highlight-prompt nil
           ;;         eshell-prompt-function 'epe-theme-lambda))


  (ability focus ()
           "Provides means for focusing on code review."
           (cheatsheet-add :group '--Development--
                           :key   "M-x focus-mode"
                           :description "Highlights only the paragraph of code which you are reading for better focus."))
  (ability pt ()
           "Provides fast search ability via platinium search"
           (require 'pt)
           (cheatsheet-add :group '--Development--
                           :key   "C-c p s s"
                           :description "Search within a project using pt. It's fast.")

           (cheatsheet-add :group '--Development--
                           :key   "C-c p s r"
                           :description "Search for a regexp in a project.")
           (advice-add 'projectile-ag :around #'projectile-pt)
           (add-hook 'projectile-mode-hook 'enable-pt-search))

  (ability file-browser ()
           "A ranger like file browser for FG42"
           (cheatsheet-add :group '--Development--
                           :key   "f7"
                           :description "A ranger like file browser for FG42")
           (setq ranger-cleanup-eagerly t)
           (setq ranger-show-dotfiles nil)
           (global-set-key [f7] 'ranger))

  (ability smart-mode-line ()
           "Smarter modeline for FG42"
           (setq sml/no-confirm-load-theme t)
           (setq sml/theme 'respectful)
           (sml/setup))
  (message "'development' extension has been initialized."))

(provide 'extensions/development/init)
