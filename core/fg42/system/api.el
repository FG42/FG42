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
;; This namespace contains several selector function to work with the
;; `fg42-system' data structure.
;;
;;; Code:
(require 'fg42/utils)
(require 'fg42/system/core)


(defun fg42-system/root (system)
  "Return the root path of the given SYSTEM."
  (fg42-system-root system))


(defun fg42-system/fpkg-path (system)
  "Return the absolute path to the fpkg backend from SYSTEM root."
  (path-join
   (fg42-system/root system)
   (fg42-system-fpkg-backend-path system)))


(defun fg42-system/fpkg-backend-version (system)
  "Return the FPKG backend version of the given SYSTEM."
  (fg42-system-fpkg-backend-version system))


(defun fg42-system/fpkg-initilized-p (system)
  "Return a boolean value indicating whether the SYSTEM is initialized or not."
  (fg42-system-fpkg-initilized system))


(defun fg42-system/fpkg-initilized! (system)
  "Mark fpkg as initialized for the given SYSTEM."
  (setf (fg42-system-fpkg-initilized system) t))


(defun fg42-system/core-dependencies (system)
  "Return a list of core dependencies for the given SYSTEM.
Core dependencies are those packages which are essential to the system itself
and not the extensions."
  (fg42-system-core-dependencies system))


(provide 'fg42/system/api)
;;; api.el ends here
