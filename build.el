;;; build --- build script for FG42
;;
;; Copyright (C) 2010-2020  Sameer Rahmani <lxsameer@gnu.org>
;;
;; Author: Sameer Rahmani <lxsameer@gnu.org>
;; Keywords: lisp fg42 IDE package manager
;; Version: 1.0.0
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
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'load-path (concat (getenv "HOME") "/.fg42/lib"))

(require 'fg42)
(require 'fpkg)

(activate-extensions 'editor
                     'development
                     'web
                     'editor-theme
                     'javascript
                     'ruby
                     'clojure
                     'haskell
                     'php
                     'common-lisp
                     'python
                     'serene
                     'typescript
                     'arduino
                     'java
                     'racket
                     'irc
                     'latex)

(fg42-initialize)

(provide 'build)
;;; build.el ends here
