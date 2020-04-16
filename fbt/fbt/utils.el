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
(defun ->path (dir)
  "Return the absolute path to the given DIR with respect to FG42_HOME."
  (concat (getenv "FG42_HOME") (format "/%s" dir)))


(defun find-files (dir suffix)
  "Find all the files with the given SUFFIX in the given DIR."
  (split-string (shell-command-to-string
                 (format "find %s -iname \"*.%s\"" (->path dir) suffix)) "\n" t))


(defun el-files-in (dir)
  "Return a list of elisp files in the given DIR."
  (find-files dir "el"))


(defun elc-files-in (dir)
  "Return a list of elisp files in the given DIR."
  (find-files dir "elc"))


;; TODO: Merge this into a unified utils module with the one
;; from `fg42/utils' package
(defun path-join (&rest paths)
  "Join the given PATHS."
  (apply #'concat
         (append
          (mapcar #'file-name-as-directory (butlast paths))
          (last paths))))


(provide 'fbt/utils)
;;; utils.el ends here
