;;; LuaExtention --- Enable Lua support in FG42
;;; Commentary:
;;; Code:
(require 'fpkg)
(require 'fg42/extension)
(require 'extensions/lua/init)

;; dependencies
(depends-on 'lua-mode)


(extension lua
           :version 0.0.1
           :on-initialize extensions/lua-initialize
           :docs "lib/extensions/lua/readme.org")
(provide 'extensions/lua)
;;; lua.el ends here
