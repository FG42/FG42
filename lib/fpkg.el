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
(require 'cl-lib)
(require 'subr-x)
(require 'fg42/extension)
(require 'fg42/utils)
(require 'fpkg/installers)

;; Variables ---------------------------------
(cl-defstruct fpkg-dependency
  "Package structure for FG42."
  name
  (version "0")
  (path nil)
  (source 'elpa))


(defvar fpkg-initilized-p nil
  "A boolean flag that indicates whether FPKG is initialized or not.")


(defvar required-packages (make-hash-table)
  "A hash of `fg42-package structure representing required packages.")


;; Functions ----------------------------------
(defun all-dependencies-installed? ()
  "Return t if all the dependencies installed."
  (let ((result t))
    (dolist (pkg (hash-table-keys required-packages))
      (when (not (package-installed-p pkg))
        (message "'%s' package is not installed" pkg)
        (setq result nil)))
    result))


(defun install--package (pkg)
  "Intall the package PKG via its propreate source."
  (let* ((source (fpkg-dependency-source pkg))
         (func-name (concat "install-package-via-" (symbol-name source)))
         (install-func (symbol-function (intern func-name))))
    (funcall install-func pkg)))


(defun fpkg-initialize ()
  "Initilize the `package.el' and related stuff to be used in FG42."
  (let ((packages (hash-table-values required-packages)))

    (require 'package)

    (add-to-list 'package-archives
     '("melpa" . "http://melpa.org/packages/") t)
    (when (< emacs-major-version 24)
      ;; For important compatibility libraries like cl-lib
      (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

    ;; Initialize package.el
    (package-initialize)

    (setq url-http-attempt-keepalives nil)

    (unless (all-dependencies-installed?)
      ;; check for new packages (package versions)
      (message "%s" "Refreshing package database...")
      (package-refresh-contents)

      ;; install the missing packages
      (dolist (pkg packages)
        (when (not (package-installed-p (fpkg-dependency-name pkg)))
          (install--package pkg))))))


(defun fpkg-initialize-once ()
  "Initilize FPKG only once."
  (when (not fpkg-initilized-p)
    (fpkg-initialize)))



(defun official-extension-p (args)
  "Predicate to say if ARGS is an official FG42 extension."
  (member args fg42/extensions))


(defun get-receipe (name)
  "Get the receipe for given NAME if that is an official extension."
  (list name :host 'gitlab :repo (format "FG42/%s" name)))


(defmacro fg42-install-extension (args)
  "Install if given ARGS is an official extension."
  ;; TODO: Straight supports reciepe registery it might worth
  ;;       using it.
  (let ((reciepe (get-receipe args)))
    `(use-package ,args :straight ,reciepe)))


(defmacro depends-on (pkgname &rest details)
  "Install the given PKGNAME with the optional DETAILS."
  (if (official-extension-p pkgname)
      `(fg42-install-extension ,(eval pkgname))
    `(use-package ,(eval pkgname) ,@details)))

(defun depends-on (pkgname &rest args)
  "Install the package PKGNAME with respect to the ARGS."
  (let ((pkg (apply 'make-fpkg-dependency :name pkgname args)))
    (puthash pkgname pkg  required-packages)))


(provide 'fpkg)
;;; fpkg.el ends here
