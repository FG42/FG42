;;; fpkg --- a simple package manager for FG42                     -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2010-2012  Sameer Rahmani <lxsameer@gnu.org>
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
(require 'fpkg/installers)

;; Variables ---------------------------------
(cl-defstruct fpkg-dependency
  "Package structure for FG42."
  name
  (version "0")
  (path nil)
  (source 'elpa))


(defvar bootstrap-version nil
  "Bootstrap version of straight.  This var is used in straight's installer.")


(defvar fpkg-packages-path
  (expand-file-name ".fpkg/" fg42-home)
  "The path to the directory which FPKG will use to store that packages.")

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
  "Install a package via its propreate source."
  (let* ((source (fpkg-dependency-source pkg))
	 (func-name (concat "install-package-via-" (symbol-name source)))
	 (install-func
	  (symbol-function
	   (intern func-name))))
    (funcall install-func pkg)))


(defun fpkg-initialize-old ()
  "Initilize the package.el and related stuff to be used in FG42"
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


(defun fpkg-initialize ()
  "Initilize the straight.e package manager and setup necessary hooks."
  (let ((bootstrap-file (concat fpkg-packages-path
                                "straight/repos/straight.el/bootstrap.el"))
        (bootstrap-version 5))

    (make-directory fpkg-packages-path t)
    (setq straight-base-dir fpkg-packages-path)
    (if (not (file-exists-p bootstrap-file))
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp))
      (load bootstrap-file nil 'nomessage))))


(defun fpkg-initialize-once ()
  "Initilize FPKG only once."
  (when (not fpkg-initilized-p)
    (fpkg-initialize)))


(defun depends-on-old (pkgname &rest args)
  "Install the given PKGNAME if it isn't installed.  Ignore ARGS for now."
  (let ((pkg (apply 'make-fpkg-dependency :name pkgname args)))
    (puthash pkgname pkg  required-packages)))

(defun depends-on (pkgname)
  "Install the given PKGNAME if it isn't installed."
  (straight-use-package pkgname))


(provide 'fpkg)
;;; fpkg.el ends here
