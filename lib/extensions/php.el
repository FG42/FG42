(require 'fpkg)
(require 'fg42/extension)
(require 'extensions/php/init)

;; Dependencies ----------------------------------
(depends-on 'php-mode)

;; Extension -------------------------------------
(extension php
	   :version "2.32"
	   :on-initialize extensions/php-initialize)

(provide 'extensions/php)
