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
        (transient-mark-mode 1)))

  ;; Automatically removed excess backups of the file
  (setq delete-old-versions t)

  ;; Font Configuration -----------------------------------
  (ability font ()
           "Sets the default font to fg42 font"
           (add-to-list 'default-frame-alist (cons 'font (format "%s-%d" fg42-font fg42-font-size)))
           (set-face-attribute 'default t :font fg42-font))
  ;; ------------------------------------------------------

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
                  :description "Move forward by a paragraph")

  (global-set-key (kbd "C-?") 'cheatsheet-show)

  ;; Fast Move in the buffer
  (global-set-key (kbd "M-1") 'avy-goto-word-or-subword-1)

  (cheatsheet-add :group '--Navigation--
                  :key   "M-1"
                  :description "Jump to the a word or subword in the buffer")


  ;; Remove splash screen
  (setq inhibit-splash-screen t)

  ;; scratch should be scratch
  (setq initial-scratch-message nil)

  (ability highligh-current-line ()
           "Highlights the current line."
           (global-hl-line-mode t))
  (ability flycheck ()
           "Check syntax on the fly using flycheck."
           (require 'flycheck)

           (add-hook 'prog-mode-hook 'global-flycheck-mode)
           (add-hook 'after-init-hook 'global-flycheck-mode))

  (ability spaceline (flycheck)
           "A really cool mode line alternative which borrowed from awesome spacemacs"
           (require 'spaceline-config)
           (require 'extensions/editor/spaceline-alt)

           ;; TODO: Move this to somewhere propriate
           ;; Modeline indicator for lxdrive
           (spaceline-define-segment lxdrive
             "lxdrive indicator on spaceline."
             (if (and (boundp 'lxdrive-minor-mode) lxdrive-minor-mode)
                 (all-the-icons-faicon  "arrows"  :height 0.8 :v-adjust 0.15 :face 'all-the-icons-lgreen)
               (all-the-icons-faicon "pencil" :height 0.8 :v-adjust 0.15))
             :tight t)

           (spaceline-compile
             "ati"
             '(((lxdrive) :face highlight-face :skip-alternate t)
               ((ati-projectile ati-mode-icon ati-buffer-id) :face default-face)
               ((ati-process ati-region-info) :face highlight-face :separator " | ")
               ((ati-modified ati-window-numbering ati-buffer-size ati-position) :face highlight-face :skip-alternate t)
               ((ati-flycheck-status ati-(point)ackage-updates purpose) :separator " | " :face other-face))
             ;; ((minor-modes) :face default-face)


             '(((ati-vc-icon " ") :face default-face :skip-alternate t :tight t)))

           (setq-default mode-line-format '("%e" (:eval (spaceline-ml-ati)))))

  ;;(spaceline-emacs-theme))

  ;; ACE Window

  (global-set-key (kbd "C-<tab>") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  ;; Tramp configuration -------------------------------------
  (ability tramp ()
           (setq tramp-default-method "ssh")
           (cheatsheet-add :group '--EDITOR--
                           :key   "f9"
                           :description "Open up your todo file. checkout `fg42-todo-file` var and `fg42-open-todo` function.")
           (global-set-key [f9] 'fg42-open-todo))

  (global-unset-key (kbd "C-o"))
  (global-unset-key (kbd "C-v"))

  (cheatsheet-add :group '--EDITOR--
                  :key   "C-s-n"
                  :description "Move a paragraph forward")

  (cheatsheet-add :group '--EDITOR--
                  :key   "C-s-p"
                  :description "Move a paragraph backward")

  (global-set-key (kbd "C-s-n") 'forward-paragraph)
  (global-set-key (kbd "C-s-p") 'backward-paragraph)

  ;; replace strings
  (global-set-key (kbd "C-c M-s") 'replace-string)

  ;; Basic Key bindings
  (global-set-key (kbd "\C-c m") 'menu-bar-mode)

  (global-set-key (kbd "<f2>") 'goto-line)

  (global-set-key (kbd "M-TAB") 'switch-to-previous-buffer)
  (global-set-key (kbd "M-`") 'switch-to-favorite-buffer)

  ;; Don't allow tab as indent
  (setq-default indent-tabs-mode nil)

  ;; Default indent width
  (setq tab-width 2)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; Enhancements ---------------------------------------------

  ;; Global configurations
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq x-select-enable-clipboard t)
  (column-number-mode t)

  (ability hide-menu ()
           "Hides the emacs menu completely."
           (menu-bar-mode -1))

  (show-paren-mode t)
  (cua-selection-mode t)

  (ability thin-cursor ()
           (setq-default cursor-type 'bar))

  (ability nonblinker-cursor ()
           (blink-cursor-mode -1))


  ;; expand-region -------------------------------------------
  (global-set-key (kbd "C-=") 'er/expand-region)

  ;; Multiple cursor -----------------------------------------
  ;; multiple cursor configurations
  (ability multiple-cursors ()
      (global-set-key (kbd "C->") 'mc/mark-next-like-this)
      (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
      (global-set-key (kbd "C-c C-SPC ") 'mc/mark-all-like-this))

  ;; Reload FG42
  (define-key global-map (kbd "C-<f5>") 'fg42-reload)

  ;; Key Chord ------------------------------------------------
  ;; (require 'key-chord)
  ;; (key-chord-mode 1)

  ;; (key-chord-define-global "hj"     'undo)
  ;; (key-chord-define-global "kl"     'right-word)
  ;; (key-chord-define-global "sd"     'left-word)
  ;; (key-chord-define-global "m,"     'forward-paragraph)
  ;; (key-chord-define-global "p["     'backward-paragraph)

  ;; HideShow -------------------------------------------------------
  (global-set-key (kbd "C-\-") 'hs-toggle-hiding)
  (hs-minor-mode)

  ;; Guru Configuration
  (ability guru ()
           (require 'guru-mode)
           (guru-global-mode +1))

  ;; IDO configurations ---------------------------------------------
  (ability ido ()
           (require 'ido)
           (require 'flx-ido)
           (require 'ido-vertical-mode)

           (ido-everywhere t)

           (require 'ido-completing-read+)
           (ido-ubiquitous-mode 1)

           (ido-mode t)

           (ability smex ()
             (smex-initialize)
             (global-set-key (kbd "M-x") 'smex))

           (flx-ido-mode 1)
           (setq ido-use-faces nil)
           (setq ido-use-filename-at-point nil)
           (setq ido-enable-flex-matching t)
           (ido-vertical-mode 1))


  (ability ivy ()
           "Completion using ivy."
           (require 'ivy)
           (require 'counsel)

           (ivy-mode 1)

           (setq ivy-use-virtual-buffers t)
           (setq enable-recursive-minibuffers t)
           (global-set-key (kbd "M-x") 'counsel-M-x)

           (global-set-key (kbd "<f1> f") 'counsel-describe-function)
           (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
           (global-set-key (kbd "<f1> l") 'counsel-find-library)
           (global-set-key (kbd "C-c k") 'counsel-ag)
           (global-set-key (kbd "C-c C-r") 'ivy-resume))

  ;; Swiper ---------------------------------------------------
  (ability swiper (ivy)
           "Replace default isearch with swiper"
           (global-set-key "\C-s" 'swiper)
           (global-set-key "\C-r" 'swiper))
  ;; (with-ability ido
  ;;               (global-set-key (kbd "C-x b") 'ido-switch-buffer)))

  (ability tabbar ()
           (tabbar-mode 1))
  ;; Helm -----------------------------------------------------
  (ability helm ()
           "Helm is an emacs incremental completion and selection narrowing framework"
           (require 'helm)
           (require 'helm-flx)
           (global-set-key (kbd "C-c h") 'helm-command-prefix)
           (global-set-key (kbd "M-x") 'helm-M-x)
           (global-set-key (kbd "C-x C-f") 'helm-find-files)
           (global-unset-key (kbd "C-x c"))

           (define-key helm-map (kbd "<tab>")
             'helm-execute-persistent-action)

           (define-key helm-map (kbd "C-i")
             'helm-execute-persistent-action)

           (define-key helm-map (kbd "C-z")
             'helm-select-action)



           (when (executable-find "curl")
             (setq helm-google-suggest-use-curl-p t))

           (setq helm-split-window-in-side-p t
                 helm-move-to-line-cycle-in-source t
                 helm-ff-search-library-in-sexp t
                 helm-scroll-amount 8
                 helm-ff-file-name-history-use-recentf t)

           (setq helm-flx-for-helm-find-files t
                 helm-flx-for-helm-locate     t)

           (helm-flx-mode +1)
           (helm-mode 1))

  ;; Session Management ---------------------------------------
  (ability desktop-mode ()
           "Save your current working buffers and restore later"
           (desktop-save-mode 1))

  (ability emoji ()
           "Adds support for emoji support in FG42. (github style)"
           (require 'emojify)
           (add-hook 'after-init-hook #'global-emojify-mode))

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
                    "Vazir")
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
  ;; Load about submenu
  (require 'extensions/editor/version)
  (require 'extensions/editor/about)
  (require 'extensions/editor/custom)
  (require 'extensions/editor/session-management)
  (require 'extensions/editor/lxdrive-mode)
  (require 'extensions/editor/lxmodeline)
  (message "'editor' extension has been initialized."))

(provide 'extensions/editor/init)
