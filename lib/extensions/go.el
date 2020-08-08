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
(depends-on 'go-eldoc)
(depends-on 'gotest)
(depends-on 'exec-path-from-shell)

(defability go-2 () "GO2"
            (message "GO 2 ability enabled"))

(extension go
           :version 0.0.1
           :abilities (go-2)
           :on-initialize extensions/go-initialize
           :docs "lib/extensions/go/readme.org")
(provide 'extensions/go)
;;; go.el ends here
