;; Uncomment this for debugging purposes
;;(setq debug-on-error t)
;; THEME
;; =====
;; Load the default theme
;; Other options are:
;; (theme themes/color-theme-spacemacs-light)
;; (theme themes/color-theme-spacemacs-dark)
;; (theme themes/color-theme-doom-dracula)
(theme themes/color-theme-doom-one)
;; (theme themes/color-theme-doom-molokai)

;; ABILITIES
;; =========
;; Disable abilities which you don't want.
(disable 'rbenv 'helm 'spell 'linum 'tabbar
         'smart-mode-line 'desktop-mode 'jedi
         'dired+ 'guru 'emoji 'elpy 'github
         'versioned-backup)


;; EXTENSIONS
;; ==========
;; Uncomment extensions that you may need.
(activate-extensions 'editor
                     'development
                     'web
                     'editor-theme
                     'javascript
                     'ruby
                     'clojure
                     'haskell
                     'php
                     'go
                     'common-lisp
                     'python
                     'arduino
                     'java
                     'racket
                     'irc
                     'latex)


;; What are you ?
(i-am-god)

;; i-am-god   => Emacs user.
;; i-am-human => A user of other boring editors.
;; i-am-evil  => An evil user. A vim user.


;; Example of things you can do in your ~/.fg42.el config file:
;;
;; Setting your TODO file path:
;;   (setq fg42-todo-file "~/.TODO.org")
;; or you can open a remote TODO file
;;   (add-hook 'fg42-before-open-todo-hook 'disable-projectile)
;;   (setq fg42-todo-file "/ssh:user@host:/home/USER/.TODO.org")
;;
;; Set some environment variables for your fg42 to use
;;  (setenv "http_proxy" "localhost:8118")
;;  (setenv "https_proxy" "localhost:8118")
;;
;; Alwasy open a your TODO file as your first buffer
;;  (add-hook 'fg42-after-initialize-hook 'fg42-open-todo)

;; If you're using tools like rbenv or nodenv or other similar tools
;; to manage versions of your favorite language, then you need to
;; add their shims to your path. Follow the example below:
;;
;; (setenv "PATH" (concat "/home/USER/.rbenv/shims:"
;;                        "/home/USER/.nodenv/shims:"
;;                        "/home/USER/bin:" (getenv "PATH")))
;;
;; Ofcourse you need to change the USER to your username

;; If you have a separate bin directory as well you can add it
;; to the exec-path as follows:
;;
;; (add-to-list 'exec-path "/home/USER/bin")
;;
;; abilities like clojure which uses the lien tools need to find
;; specific tools (lein in this case) in your exec-path.


;; IRC Extension configuration ------------------------------------------------
;; Setup all the servers and channels you need
;; (setq irc-servers
;;   '(("irc.freenode.net"
;;      :port 6697
;;      :encryption tls
;;      :channels ("#5hit" "#emacs"))))

;; Set you nickname
;; (setq irc-nickname "awesome-fg42-user")

;; You can use password manager for storing you IRC credentials.
;; NOTE: you need to have a pair of GPG keys for this.
;;
;; Add the following to your ~/.authinfo.gpg :
;;
;; machine freenode login <username> port nickserv password <password>
;;
;; Or if you don't want to use the password manager your can directly
;; set it up like this:
;; (setq irc-auth '(("freenode" nickserv "some_user" "p455w0rd")
;;                  ("freenode" chanserv "some_user" "#channel" "passwd99"))
