(add-to-list 'load-path (concat (getenv "FG42_HOME") "/lib"))
(require 'fg42)
(require 'fg42/wm)
(initialize-wm)

;; USER CONFIGS
;; ============
;; Load user config file in ~/.fg42.el
(load-user-config "~/.fg42.el")
;; NOTE: It's important to use ~/.fg42.el instead of this file
;;       because updating fg42 will discard your changes in
;;       this file.
(fg42-initialize)
