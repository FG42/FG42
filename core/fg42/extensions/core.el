;;; extensions --- Extension library of FG42  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2010-2020  Sameer Rahmani <lxsameer@gnu.org>
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

(require 'cl-lib)
(require 'fg42/system/core)


(cl-defstruct fg42-extension
  "Each FG42 extension should implement a copy of this structure."
  name

  ;; Let's keep this field for backward compatiblity for a while
  docs

  ;; Each extension should expose a info page.
  doc-index
  ;; To be used with `describe-extension'
  (docstring nil)
  ;; Projectile provides a project type that we can use to
  ;; activate/load the extensions based on their registered
  ;; type.
  project-types

  (version nil)

  ;; An instance of fg42-actions structure that describe the
  ;; different actions of the given extension
  (actions nil)
  (path nil)
  ;; Callbacks
  (on-initialize nil)
  (on-load)
  (on-unload))


(defun fg42-extensions/build-path (system ext)
  "Build a path for the given EXT name (symbol) via SYSTEM info."
  ;; TODO: should we extract variables such as `fg42-home' to their
  ;;       dedicated ns in order to avoid the warning ?
  (let ((ext-name (symbol-name ext)))
    (concat (fg42-system-root system)
            "/extensions/" ext-name  "/" ext-name ".el")))


(defun fg42-extensions/path (system ext)
  "Return the path to the given extension EXT in the given SYSTEM."
  (cond
   ((symbolp ext) (fg42-extensions/build-path system ext))
   ((fg42-extension-p ext)
    (or (fg42-extension-path ext)
        (fg42-extensions/build-path system
                                    (intern (fg42-extension-name ext)))))))


(defmacro defextension (name docstring &rest args)
  "A simple DSL to define new fg42 extension by given NAME, DOCSTRING and ARGS."
  (declare (doc-string 2) (indent 1))
  ;; TODO: Inject the docstring to the current `system' in order
  ;;       to collect it later for `describe-extension' function.
  (when (not (stringp docstring))
    (throw 'extention-error
           "`docstring' is mandatory and it should be a string."))
  `(setq ,name (apply 'make-fg42-extension
                      :name ,(symbol-name name)
                      :docstring ,docstring
                      (quote ,args))))


(provide 'fg42/extensions/core)
;;; core.el ends here
