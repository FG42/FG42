;;; FG42 --- The mighty editor for the emacsians -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2010-2020 Sameer Rahmani <lxsameer@gnu.org>
;;
;; Author: Sameer Rahmani <lxsameer@gnu.org>
;; URL: https://gitlab.com/FG42/FG42
;; Version: 3.0.0
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;; Code:

(defvar fg42-v3 (or (getenv "FG42_V3") '()))

(if fg42-v3
    (add-to-list 'load-path (concat (getenv "FG42_HOME") "/core"))
  (add-to-list 'load-path (concat (getenv "FG42_HOME") "/lib")))

;; DEBUG
;; =====
;; Uncomment the below code to trigger stacktraces in case of any errors
;; (toggle-debug-on-error)

(require 'fg42)

;; THEME
;; =====
;; Load the default theme
;; Other options are:
;; (theme themes/color-theme-spacemacs-monokai)
;; (theme themes/color-theme-spacemacs-light)
;; (theme themes/color-theme-doom-one)
;; (theme themes/color-theme-doom-molokai)
;;(theme themes/color-theme-spacemacs-dark)

;; ABILITIES
;; =========
;; Disable abilities which you don't want.
;; (disable 'rbenv 'helm 'spell 'linum 'smart-mode-line)

;; EXTENSIONS
;; ==========
;; Uncomment extensions that you may need.
;; (activate-extensions 'editor
;;                      'development
;;                      'web
;;                      'editor-theme
;;                      ;'arduino
;;                      'javascript
;;                      ;'php
;;                      'clojure
;;                      ;'python
;;                      'ruby
;; 		     )

;; USER CONFIGS
;; ============
;; Load user config file in ~/.fg42.el

(load-user-config
 (if fg42-v3
     "~/.fg42.v3.el"
   "~/.fg42.el"))

;; NOTE: It's important to use ~/.fg42.el instead of this file
;;       because updating fg42 will discard your changes in
;;       this file.
(when (not fg42-v3)
  (fg42-initialize))


(provide 'fg42-config)
;;; fg42-config.el ends here
