;;; GoExtention --- Enable Golang support in FG42
;;; Commentary:
;;; Code:
(require 'fpkg)
(require 'fg42/extension)
(require 'extensions/go/init)

;; dependencies

(depends-on 'go-mode)
(depends-on 'go-add-tags)
(depends-on 'go-stacktracer)
(depends-on 'gotest)
(depends-on 'exec-path-from-shell)


(extension go
           :version 0.0.1
           :on-initialize extensions/go-initialize
           :docs "lib/extensions/go/readme.org")
(provide 'extensions/go)
;;; go.el ends here
