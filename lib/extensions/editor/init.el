(require 'extensions/editor/buffers)

;; Customizations --------------------------------------------
(defcustom fg42-todo-file "~/.TODO.org"
  "Path to your TODO file. You can use a tramp address here as well."
  :type 'string
  :group 'fg42)

;; Hooks -----------------------------------------------------
(defvar fg42-before-open-todo-hook nil)
(defvar fg42-after-open-todo-hook nil)

;; Vars -----------------------------------------------------------------------
(defvar fg42-font "Fira Mono"
  "The default font to be used with FG42.")

(defvar fg42-font-size 12
  "The default font to be used with FG42.")


;; Functions -------------------------------------------------
(defun fg42-reload ()
  "Reload the entire FG42."
  (interactive)
  (load-file (concat (getenv "FG42_HOME") "/fg42-config.el")))


;;;###autoload
(defun fg42-open-todo ()
  (interactive)
  (run-hooks 'fg42-before-open-todo-hook)
  (find-file fg42-todo-file)
  (run-hooks 'fg42-after-open-todo-hook))

;;;###autoload
(defun extensions/editor-initialize ()
  "Base plugin initialization."

  (if (eq system-type 'darwin)
      (progn
        (message "Running on the stupid macOS X.")
        (exec-path-from-shell-initialize)))

  (require 'all-the-icons)
  (require 'cheatsheet)
  (require 'extensions/editor/utils)

  (add-to-list 'custom-theme-load-path
               (concat fg42-home "/lib/themes/custom_themes"))


  (ability silent-start ()
           ;; Remove splash screen
           (setq inhibit-splash-screen t))

  ;; scratch should be scratch
  (setq initial-scratch-message nil)


  ;; Font Configuration -----------------------------------
  (ability font ()
           "Sets the default font to fg42 font"
           (add-to-list 'default-frame-alist (cons 'font (format "%s-%d" fg42-font fg42-font-size)))
           (set-face-attribute 'default t :font fg42-font))
  ;; ------------------------------------------------------

  (ability race ()
      ;; Setting user preference based on the race.
      (if (is-evil?)
          (progn
            (require 'evil)
            (evil-mode 1)))

      (if (is-human?)
          (progn
            (cua-mode 'emacs)
            (cua-selection-mode t)
            (setq cua-auto-tabify-rectangles nil)
            (transient-mark-mode 1))))



  ;; Automatically removed excess backups of the file
  (ability delete-old-backups ()
           (setq delete-old-versions t))

  ;; FG42 motions
  (ability fg42-motions ()
          ;; Fast Move in the buffer
          (global-set-key (kbd "M-1") 'avy-goto-word-or-subword-1)

          ;; paragraph motions
          (global-set-key (kbd "C-s-n") 'forward-paragraph)
          (global-set-key (kbd "C-s-p") 'backward-paragraph)

          ;; replace strings
          (global-set-key (kbd "C-c M-s") 'replace-string)

          ;; Basic Key bindings
          (global-set-key (kbd "\C-c m") 'menu-bar-mode)


          (global-set-key (kbd "<f2>") 'goto-line)

          (global-set-key (kbd "M-TAB") 'switch-to-previous-buffer)
          (global-set-key (kbd "M-`") 'switch-to-favorite-buffer))

  (ability which-key ()
           (when (is-evil?)
             (which-key-mode t)))

  ;; enhance evil mode with space leader keybindings
  (ability space-keys (which-key)
           "evil mode with space leader keybindings"
           (when (is-evil?)
             (defkey global-map 'find-file :evil (:normal "SPC f f"))
             (defkey global-map 'kill-buffer :evil (:normal "SPC b k"))
             (defkey global-map 'save-buferr :evil (:normal "SPC b s"))
             (defkey global-map 'next-buffer :evil (:normal "SPC b n"))
             (defkey global-map 'previous-buffer :evil (:normal "SPC b p"))
             (defkey global-map 'switch-to-buffer :evil (:normal "SPC b l"))
             (defkey global-map 'other-window :evil (:normal "SPC w o"))
             (defkey global-map 'delete-window :evil (:normal "SPC w d"))
             (defkey global-map 'delete-other-windows :evil (:normal "SPC w m"))
             (defkey global-map 'split-window-vertically :evil (:normal "SPC w s v"))
             (defkey global-map 'eval-last-sexp :evil (:normal "SPC e e"))
             (defkey global-map 'eval-buffer :evil (:normal "SPC e b"))
             (defkey global-map 'comment-line :evil (:normal "SPC l c"))
             (defkey global-map 'describe-key :evil (:normal "SPC d k"))
             (defkey global-map 'describe-function :evil (:normal "SPC d f"))
             (defkey global-map 'describe-variable :evil (:normal "SPC d v"))))


  (ability highligh-current-line ()
           "Highlights the current line."
           (global-hl-line-mode t))


  ;; Flycheck syntax checker
  (ability flycheck ()
           "Check syntax on the fly using flycheck."
           (require 'flycheck)

           (add-hook 'prog-mode-hook 'global-flycheck-mode)
           (add-hook 'after-init-hook 'global-flycheck-mode))

  ;; ACE Window
  (ability ace-window ()
                (global-set-key (kbd "C-<tab>") 'ace-window)
                (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

  ;; Tramp configuration -------------------------------------
  (ability tramp ()
           (setq tramp-default-method "ssh")
           (cheatsheet-add :group '--EDITOR--
                           :key   "f9"
                           :description "Open up your todo file. checkout `fg42-todo-file` var and `fg42-open-todo` function.")
           (global-set-key [f9] 'fg42-open-todo))
  ;; TODO: why ?
  (global-unset-key (kbd "C-o"))
  (global-unset-key (kbd "C-v"))

  ;; Cheatsheets
  (ability cheatsheet ()
               (global-set-key (kbd "C-?") 'cheatsheet-show)
               (cheatsheet-add :group '--Navigation--
                   :key   "M-1"
                   :description "Jump to the a word or subword in the buffer")
               (cheatsheet-add :group '--EDITOR--
                   :key   "C-s-n"
                   :description "Move a paragraph forward")

               (cheatsheet-add :group '--EDITOR--
                   :key   "C-s-p"
                   :description "Move a paragraph backward")

               (cheatsheet-add :group '--HELP--
                   :key   "C-?"
                   :description "Show this cheatsheet")
               (cheatsheet-add :group '--Navigation--
                   :key   "M-f"
                   :description "Move a word to right")
               (cheatsheet-add :group '--Navigation--
                   :key   "M-b"
                   :description "Move a word to left")
               (cheatsheet-add :group '--Navigation--
                   :key   "M-{"
                   :description "Move back a paragraph")
               (cheatsheet-add :group '--Navigation--
                   :key   "M-}"
                   :description "Move forward by a paragraph"))


  ;; Don't allow tab as indent
  (setq-default indent-tabs-mode nil)

  ;; Default indent width
  (setq tab-width 2)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; Enhancements ---------------------------------------------

  (ability hide-toolbar ()
           (tool-bar-mode -1))

  (ability hide-scroll-bar ()
           (scroll-bar-mode -1))

  (ability column-number-in-modeline ()
           (column-number-mode t))

  (ability hide-menu ()
           "Hides the emacs menu completely."
           (menu-bar-mode -1))

  (ability highligh-matching-parens ()
           (show-paren-mode t))

  (ability cua-selection-mode ()
           (cua-selection-mode))

  (ability thin-cursor ()
           (setq-default cursor-type 'bar))

  (ability nonblinker-cursor ()
           (blink-cursor-mode -1))


  ;; expand-region -------------------------------------------
  (ability expand-region ()
           (global-set-key (kbd "C-=") 'er/expand-region))

  ;; Multiple cursor -----------------------------------------
  (ability multiple-cursors ()
      (global-set-key (kbd "C->") 'mc/mark-next-like-this)
      (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
      (global-set-key (kbd "C-c C-SPC ") 'mc/mark-all-like-this))

  ;; Reload FG42
  (define-key global-map (kbd "C-<f5>") 'fg42-reload)

  ;; HideShow mode -------------------------------------------------------
  (ability hide-show ()
           (global-set-key (kbd "C-\-") 'hs-toggle-hiding)
           (hs-minor-mode))

  ;; Guru Configuration
  (ability guru ()
           (require 'guru-mode)
           (guru-global-mode +1))

  ;; Tabbar Configuration
  (ability tabbar ()
           (tabbar-mode 1))

  ;; Session Management ---------------------------------------
  (ability desktop-mode ()
           "Save your current working buffers and restore later"
           (desktop-save-mode 1))

  (ability emoji ()
           "Adds support for emoji support in FG42. (github style)"
           (require 'emojify)
           (add-hook 'after-init-hook #'global-emojify-mode))


  (ability persian ()
           (set-fontset-font "fontset-default"
                             (cons (decode-char 'ucs #x0627)
                                   (decode-char 'ucs #x0649))
                             "Vazir")

           (set-fontset-font "fontset-default"
                             (cons (decode-char 'ucs #xFE8D)
                                   (decode-char 'ucs #xFEF0))
                             "Vazir")

           (set-fontset-font "fontset-default"
                             (cons (decode-char 'ucs #x064e)
                                   (decode-char 'ucs #x06a9))
                             "Vazir")

           (set-fontset-font "fontset-default"
                             (cons (decode-char 'ucs #x06F0)
                                   (decode-char 'ucs #x00A0))
                             "Vazir"))

  ;; Backup files ---------------------------------------------
  (ability backup-files ()
           ;; Put them in one nice place if possible
           (if (file-directory-p "~/.backup")
               (setq backup-directory-alist '(("~/.backup")))
             (make-directory "~/.backup"))
           (setq backup-by-copying t))

  (ability versioned-backup ('backup-files)
           (setq delete-old-versions t)
           (setq version-control t)
           (setq kept-new-versions 3)
           (setq kept-old-versions 2))

  ;; get rid of yes-or-no questions - y or n is enough
  (defalias 'yes-or-no-p 'y-or-n-p)

  (setup-utils)

  (setq my-path (file-name-directory load-file-name))

  (require 'extensions/editor/modeline)
  (require 'extensions/editor/version)
  (require 'extensions/editor/about)
  (require 'extensions/editor/custom)
  (require 'extensions/editor/session-management)
  (require 'extensions/editor/lxdrive-mode)
  (require 'extensions/editor/selection-candidates)

  (message "'editor' extension has been initialized."))

(provide 'extensions/editor/init)
