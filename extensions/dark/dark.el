;;; dark.el --- Dark side of Emacs                   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  amirreza

;; Author: amirreza <amirreza@localhost.localdomain>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(depends-on 'evil)
(depends-on 'evil-collection)
(depends-on 'evil-magit)

(defun extensions/dark-initailize ()
  (setq evil-want-keybinding nil)
  (require 'evil)
  (require 'evil-collection)
  (evil-mode +1))

(extension editor
     :version "2.31"
     :on-initialize extensions/dark-initailize)



(provide 'dark)
;;; dark.el ends here
