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
(require 'fg42/utils)
(require 'fg42/state)


(defun fg42/cube-apply (state cube)
  "Apply the given CUBE to the given STATE.
It returns a cons of (new-state . cube)."
  (cons state cube))


(defun fg42/cube-bind (m f)
  "Bind the state monad M with the given function F."
  (let ((binder (fg42/state-bind-maker #'fg42/cube-apply)))
    (funcall binder m f)))


(provide 'fg42/cube)
;;; cube.el ends here
