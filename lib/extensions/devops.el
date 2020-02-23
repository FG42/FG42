;;; DevopsExtension --- Enables Devops related things on FG42
;;; Commentary:
;;; Code:

(require 'fpkg)
(require 'fg42/extension)
(require 'extensions/devops/init)

;; Dependencies ----------------------------------

(depends-on 'dockerfile-mode)
(depends-on docker)
(depends-on ansible)
(depends-on kubernetes)

;; Extension -------------------------------------
(extension devops
           :version "0.0.1"
           :on-initialize extensions/devops-initialize
           :docs "lib/extensions/devops/readme.org")



(provide 'extensions/devops)
;; devops.el ends here
