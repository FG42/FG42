;;; FGBuildTool --- The build tool for FG42
;;
;; Copyright (c) 2010-2021  Sameer Rahmani <lxsameer@gnu.org>
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

(require 'fbt/utils)


(defun lint (dir)
  "Run linter on all the elisp files in the given DIR."
  (let ((files (el-files-in dir)))
    (if files
        (dolist (file files)
          ;; TODO: Setup flycheck here and use it to lint the elisp file.
          ;; tried to use flymake but it doesn't let you do it manually
          (with-temp-buffer
            (insert-file-contents file)))
      (error "Couldn't find any elisp files"))))


(provide 'fbt/lint)
;;; lint.el ends here
