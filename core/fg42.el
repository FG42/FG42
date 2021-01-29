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
(require 'fg42/cube)
(require 'fg42/utils)
(require 'fg42/system/core)


(defvar fg42-home (getenv "FG42_HOME")
  "The pass to fg42-home.")

(defvar fg42-tmp (concat fg42-home "/tmp"))


(autoload 'fg42/system-start "fg42/system"
  "Starts the given SYSTEM.")


(defun fg42/start! (system)
  "Start the given SYSTEM description."
  (add-hook 'window-setup-hook
            (lambda ()
              (require 'fg42/system)
              (fg42/system-start system))))


(provide 'fg42)
;;; fg42.el ends here
