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
;;; Acknoledgement:
;; Thanks to all the people who contributed to FG42.
;;
;;; Commentary:
;;; Code:

(require 'fg42)
(require 'fg42/system/core)


(defsystem FG42
  "FG42 implemented in term of systems and this is the default system."
  :fpkg-backend-path ".fpkg-v3"
  :start (lambda (system)
           (fg42/start! system))


  :extensions '(fg42-elisp))


(provide 'system)
;;; system.el ends here
