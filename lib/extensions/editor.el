(require 'fpkg)
(require 'fg42/extension)
(require 'extensions/editor/init)

;; Dependencies ----------------------------------
(depends-on 'seq)
(depends-on 'ov)

;; Expand region
(with-ability expand-region
              (depends-on 'expand-region))


;; JSON support
(with-ability json
              (depends-on 'json-mode))

;; Markdown support.
(with-ability markdown
              (depends-on 'markdown-mode))

;; Cheatsheets
(with-ability cheats
              (depends-on 'cheatsheet))

;; all-the-icons ability.
(with-ability icons
              (depends-on 'all-the-icons))

;; Fast motions based on charachters
(with-ability char-jump
              (depends-on 'avy))

;; Don't worry unless you're evil this mode won't start
(with-ability evil
              (depends-on 'evil))

;; Resize splitted windows
;; (depends-on 'windresize)

;; i3 like workspaces for FG42
(with-ability workspaces
              (depends-on 'eyebrowse))

;; Brighter background for active windows
(with-ability highlight-active-window
              (depends-on 'solaire-mode))

;; Enable line numbers if ability line-numbers is active
(with-ability line-numbers
              global-display-line-numbers-mode)

;; Multiple cursors
(with-ability multiple-cursors
              (depends-on 'multiple-cursors))

;; Spaceline, modeline from spacemacs project.
(with-ability spaceline
              (depends-on 'spaceline))

;; Cool modeline from doom project.
(with-ability doom-modeline
              (depends-on 'doom-modeline))

;; Learn Emacs keys the hard way.
(with-ability guru
              (depends-on 'guru-mode))

;; Tramp, edit files on remote.
(with-ability tramp
              (depends-on 'tramp))

;; ivy selection candidate and narrowing framework.
(with-ability ivy
              (depends-on 'ivy)
              (depends-on 'counsel))

;; another selection framework
(with-ability ido
              (depends-on 'ido)
              (depends-on 'ido-completing-read+)
              (depends-on 'smex)
              (depends-on 'ido-vertical-mode)
              (depends-on 'flx-ido))

(with-ability helm
              (depends-on 'helm)
	      (depends-on 'helm-ag)
              (depends-on 'helm-themes)
              (depends-on 'helm-flx)
              (depends-on 'helm-make)
              (depends-on 'helm-mode-manager)
              (depends-on 'helm-projectile)
              (depends-on 'helm-swoop)
              (depends-on 'helm-themes))
;; In buffer search
(with-ability swiper
	      (depends-on 'swiper))

;; Syntax highlighter
(with-ability flycheck
              (depends-on 'flycheck))

;; Emoji support
(with-ability emoji
              (depends-on 'emojify))

;; Tabbar
(with-ability tabbar
              (depends-on 'tabbar))

;; Which-key keychord helper
(with-ability which-key
              (depends-on 'which-key))

;; Mange FG42 windows easily
(with-ability ace-window
              (depends-on 'ace-window))

;; macOS compatibility
(with-ability macos
              (if (eq system-type 'darwin)
                  (depends-on 'exec-path-from-shell)))

;; Extension -------------------------------------
(extension editor
	   :version "2.31"
	   :on-initialize extensions/editor-initialize)

(provide 'extensions/editor)
