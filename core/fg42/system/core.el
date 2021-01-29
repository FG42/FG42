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
(require 'fg42/state)

(defun fg42/system-cons (system k v)
  "Set the given key K to the given value V in the SYSTEM."
  (cons (cons k v) system))


(defun fg42/system-cons-to (system k v)
  "Add the given value V to the value of key K in SYSTEM."
  (let* ((value (fg42/system-get system k))
         (m (fg42/system-cons system k (cons v value))))
    m))


(defun fg42/system-get (system k)
  "Return the value of the given key K in the SYSTEM."
  (cdr (assoc k system)))

(comment
  (fg42/system-get (fg42/system-cons '((:1 . 4)) :1 2) :1)
  (fg42/system-get '((:a . ((1 . 2)))) :a)
  (fg42/system-get
   (fg42/system-cons-to '() :a '((x . 5)))
   :a))


(defun fg42/system-register-cube (system name cube)
  "Add the given CUBE with the given NAME to the SYSTEM."
  (fg42/system-cons-to system :cubes (cons name cube)))


(defmacro defsystem (name props &rest body)
  "Define a system with the given NAME, DOCSTRING and BODY."
  (declare (indent 1))
  `(defun ,name ()
     (fg42/cube-compose ,@body)))


(provide 'fg42/system/core)
;;; core.el ends here
