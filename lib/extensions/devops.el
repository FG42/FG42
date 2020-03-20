;;; DevopsExtension --- Enable Devops support in FG42
;;; Commentary:
;;; Code:
(require 'fpkg)
(require 'fg42/extension)
(require 'extensions/devops/init)

;; dependencies
(depends-on 'kubel)
(depends-on 'ansible)
(depends-on 'docker)
(depends-on 'dockerfile-mode)

(extension devops
           :version 0.0.1
           :on-initialize extensions/devops-initialize
           :docs "lib/extensions/devops/readme.org")
(provide 'extensions/devops)
;; devops ends here
