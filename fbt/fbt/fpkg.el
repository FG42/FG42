;;; FGBuildTool --- The build tool for FG42
;;
;; Copyright (c) 2010-2020  Sameer Rahmani <lxsameer@gnu.org>
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
;;
;;; Commentary:
;;; Code:
(require 'fg42/system/core)
(require 'fg42/system/api)

(defvar bootstrap-version nil
  "`straight.el' bootstrap version.  Set it in the `system' level, not here.")

(defvar straight-base-dir)
(defvar straight-repository-branch)
(defvar straight-cache-autoloads)
(defvar straight-check-for-modifications)
(defvar straight-enable-package-integration)
(defvar straight-vc-git-default-clone-depth)
(defvar autoload-compute-prefixes)
(defvar straight-fix-org)
(defvar straight-recipe-repositories)
(defvar straight-recipe-overrides)


(defun fbt-fpkg/-set-straight-values (system)
  "Set the default values for some of `straight.el' vars for the SYSTEM."
  (setq straight-base-dir (fg42-system/fpkg-path system)
        straight-repository-branch "develop"
        ;; Our autoload process is different.
        straight-cache-autoloads nil
        ;; `straight.el' suppose to be a functional pkg manager but it actually
        ;; allows user to edit libraries in place which we don't like it.
        ;; We have our own mechanism to allow users extend FG42 so we don't
        ;; want `straight.el' to handle it. Frankly it reduces the boot time
        ;; by a lot.
        straight-check-for-modifications nil
        straight-enable-package-integration nil
        ;; We don't want `straight.el' to deep clone the dependencies. Some packages
        ;; might break this way according to `doom-emacs'
        straight-vc-git-default-clone-depth 1
        ;; We have our own autoload system.
        autoload-compute-prefixes nil
        straight-fix-org nil))


(defun fbt-fpkg/-install-core-dependencies (system)
  "Install the core dependencies of the given SYSTEM.
Core dependencies are thoses packages that the system itself is depends on
and not the extensions."
  (mapc #'straight-use-recipes (fg42-system/core-dependencies system)))


(defun fbt-fpkg/initialize (system)
  "Initilize the package manager for the given SYSTEM.
Basically fpkg will bootstrap and `straight.el' repositoryu for the given
SYSTEM by fetching the required values from it.  Including the path to the
target directory."
  (unless (fboundp 'straight--reset-caches)
      (let ((bootstrap-file (concat (fg42-system/fpkg-path system)
                                    "straight/repos/straight.el/bootstrap.el"))
            (bootstrap-version (fg42-system/fpkg-backend-version system)))

        (make-directory (fg42-system/fpkg-path system) t)
        (fbt-fpkg/-set-straight-values system)

        (or (require 'straight nil t)
            (file-readable-p bootstrap-file)
            (with-current-buffer
                (url-retrieve-synchronously
                 (format "https://raw.githubusercontent.com/raxod502/straight.el/%s/install.el"
                         straight-repository-branch)
                 'silent 'inhibit-cookies)
              (goto-char (point-max))
              (eval-print-last-sexp))
            (load bootstrap-file nil t)))
      (require 'straight))

  (straight--reset-caches)
  (fbt-fpkg/-install-core-dependencies system)

  (setq straight-recipe-repositories nil
        straight-recipe-overrides nil)

  ;; Someday we might have to use our own fork of `straight.el'
  (straight-register-package
   `(straight :type git :host github
              :repo "raxod502/straight.el"
              :files ("straight*.el")
              :branch ,straight-repository-branch
              :no-byte-compile t))

  (fg42-system/fpkg-initilized! system))


(defun fbt-fpkg/initialize-once (system)
  "Initilize FPKG only once for the given SYSTEM."
  (when (not (fg42-system/fpkg-initilized-p system))
    (fbt-fpkg/initialize system)
    ;; TODO: Install `use-package'
    ;; (straight-use-package 'use-package)
    ))


(defun fbt-fpkg/init (system params)
  "Setup fpkg repository with the given PARAMS for the given SYSTEM."
  (fbt-fpkg/initialize-once system))


(defun fbt-fpkg/print-help (command)
  "Print the usage for fpkg and the possible wrong COMMAND."
  (message "Usage:\n")
  (message "init - Setup the fpkg repository"))


(defun fbt-fpkg (system params)
  "The main interface to `fpkg' subcommand and PARAMS for the loaded SYSTEM."
  (let ((subcommand (car args))
        (args (cdr params)))
    (cond
     ((string= subcommand "init") (funcall #'fbt-fpkg/init system args))
     (t (fbt-fpkg/print-help subcommand)))))


(defun fpt-fpkg/initialize-load-path (system)
  "Setup the required load paths from the given SYSTEM."
  (add-to-list 'load-path
               (path-join (fg42-system/fpkg-path system)
                          "straight/repos/straight.el/")))


(provide 'fbt/fpkg)
;;; fpkg.el ends here
