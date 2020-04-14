;;; system --- System library of FG42 -*- lexical-binding: t; -*-
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
;; `System' is just a state monad which holds the state of the editor.
;; Each system has to have a `start' function to start the setup process.
;;
;;; Code:

(require 'cl-lib)
(require 'fg42/utils)


(cl-defstruct fg42-system
  "A `system' describes a FG42 instance. Everything that is needed
to load FG42."
  name

  ;; We will use this value for `describe-system' as a short
  ;; documentation.
  docstring

  ;; TODO: guess the system root based on the `name' field
  ;;       as the default value
  (root (concat (getenv "HOME") "/.fg42"))

  ;; The directory to store all sort of temporary files including
  ;; backups, flycheck temps and stuff like that.
  (tmp-path "~/.tmp")

  (packages '())
  ;; ;; A list of preloads to setup extensions which are not loaded yet.
  ;; ;; For more information on preloads checkout `fg42/extension'
  ;; (preloads '())

  (extensions '())
  (abilities '())
  ;; A function which takes the `system' and starts it.
  (start (lambda (system) system))
  (stop nil))


(defmacro defsystem (name &optional docstring &rest body)
  "Define a system with the given NAME, DOCSTRING and BODY."
  (declare (doc-string 2) (indent 1))
  (let ((form (if (boundp (intern (format "%s" name))) 'setq 'defvar)))
    `(,form ,name (make-fg42-system
                   :name ,(symbol-name name)
                   :docstring ,docstring
                   ,@body))))


(provide 'fg42/system/core)
;;; core.el ends here
