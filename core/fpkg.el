;;; fpkg --- a simple package manager for FG42                     -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2010-2021  Sameer Rahmani <lxsameer@gnu.org>
;;
;; Author: Sameer Rahmani <lxsameer@gnu.org>
;; Keywords: lisp fg42 IDE package manager
;; Version: 1.0.0
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
;;
;; Simple package manager for FG42
;;
;;; Code:
(require 'seq)
(require 'cl-lib)
(require 'package)


(defun fpkg/->package-el-version (version)
  "Convert the given FG42 package VERSION to package.el compatible version."
  (cond
   ((null version) '(0))
   ((eq version :latest) '(0))
   (t version)))


(defun fpkg/package-installed-via-package-el (pkg)
  "Return non-nil if the given package PKG is installed."
  (package-installed-p (plist-get pkg :name)
                       (fpkg/->package-el-version (plist-get pkg :version))))



(defun fpkg/package-installed? (pkg)
  "Return non-nil if the given package PKG is installed."
  (let* ((source (or (plist-get pkg :source) "package-el"))
         (func-name (concat "fpkg/package-installed-via-" source))
         (f (symbol-function (intern func-name))))
    (funcall f pkg)))


(defun fpkg/install--package-via-package-el (pkg)
  "Install the given package PKG via package.el."
  ;; TODO: Install packages using the package-desc to fetch
  ;;       packages with version
  ;; (let ((package (package-desc-create :name (plist-get pkg :name)
  ;;                                     :version (fpkg/->package-el-version (plist-get pkg :version))
  ;;                                     :kind 'tar)))
  ;;   (package-install package)))
  (package-install (plist-get pkg :name)))


(defun fpkg/install-package (pkg)
  "Intall the package PKG via its propreate source."
  (let* ((source (or (plist-get pkg :source) "package-el"))
         (func-name (concat "fpkg/install--package-via-" source))
         (install-func (symbol-function (intern func-name))))
    (funcall install-func pkg)))


(defun fpkg/initialize (system)
  "Initilize the `package.el' for the given SYSTEM."
  (message "KKKKKKKKKKKKKKKKKKKKKKKKKKK")
  ;; TODO: Grap any extra repo from the system (sympol-plist)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/") t)

    ;; Initialize package.el
  (package-initialize))



(provide 'fpkg)
;;; fpkg.el ends here
