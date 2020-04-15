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

(defvar fg42-system/active--system nil
  "A private variable to store the active system.
Use `fg42-get-current-system' instead")


(defun fg42-system/get-active-system ()
  "Return the current active system of FG42."
  fg42-system/active--system)


(defun fg42-system/set-system! (system)
  "Set the current system to the given SYSTEM."
  ;; TODO: In the future when we moved to parallel boot
  ;;       we need to make sure that this funciton
  ;;       sets the state safely.
  (setq fg42-system/active--system system))


(provide 'fg42/system/utils)
;;; utils.el ends here
