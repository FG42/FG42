;;; dependencies --- System library of FG42 -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2010-2021 Sameer Rahmani & Contributors
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
(require 'fg42/system/core)


(defun fg42/system-merge-keys (system cube-name keys)
  "Retun an updated SYSTEM with the given keys KEYS for CUBE-NAME."
  (if keys
      ;; TODO: Validate the deps here
      (fg42/system-cons-to system :keys (cons cube-name keys))
    system))


(provide 'fg42/system/keys)
;;; keys.el ends here
