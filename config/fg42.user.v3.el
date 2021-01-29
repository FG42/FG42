;;; FG42 --- The mighty editor for the emacsians -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2010-2021 Sameer Rahmani <lxsameer@gnu.org>
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
(setq debug-on-error t)
(require 'fg42/cube)
(require 'cubes/elisp)
(require 'cubes/org)

(defsystem default-system ()
  (fg42/elisp-cube)
  (fg42/org-cube))


(fg42/start! (default-system))


(provide '.fg42)
;;; .fg42.v3.el ends here
