;;; Cube --- Cube library of FG42 -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2010-2020 Sameer Rahmani & Contributors
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
;; Cubes are the building blocks of any `FG42' editor.  Each `cube' is a
;; unit which defines different abilities in a deterministic and idempotent
;; way.  Cubes are composable and a composition of cubes creates an editor.
;;
;;; Code:

(require 'seq)
(require 'fg42/utils)
(require 'fg42/system/dependencies)
(require 'fg42/system/keys)


(defmacro defcube (name params &rest body)
  "Define a cube with the given NAME, PARAMS and BODY."
  (declare (indent 1))
  `(defun ,name ,params ,@body))


(defun fg42/cube-run (cube system)
  "Run the given CUBE with the given SYSTEM.

Returns a pair of new system and the cube vlaue."
  (funcall cube system))


(defun fg42/cube-compose (&rest cubes)
  "Compose the given CUBES."
  (lambda (system)
    (cond
     ((null cubes) (cons system '()))
     (t
      (seq-reduce
       (lambda (s cube)
         (fg42/cube-apply (fg42/cube-run cube s)))
       cubes
       system)))))



(defun fg42/cube--apply (system cube)
  "Apply thie given CUBE to given SYSTEM."
  (if cube
      (let ((name (plist-get cube :name)))
        (-> system
          ;; insert the cube into the state
            (fg42/system-register-cube name cube)
            ;; Add the dependencies of the cube to the state
            (fg42/system-merge-dependencies name (plist-get cube :dependencies))
            ;; Add the keybindings of the cube to the state
            (fg42/system-merge-keys name (plist-get cube :keys))))
    system))


(defun fg42/cube-apply (system-cube-pair)
  "Apply the cube in the given SYSTEM-CUBE-PAIR to the system inside of it."
  (fg42/cube--apply (car system-cube-pair) (cdr system-cube-pair)))


(provide 'fg42/cube)
;;; cube.el ends here
