;;; brain-switch --- Brain switch library to keep the state of mind
;;
;; Copyright (c) 2020  Sameer Rahmani <lxsameer@gnu.org>
;;
;; Author: Sameer Rahmani <lxsameer@gnu.org>
;; URL: https://gitlab.com/FG42/FG42
;; Keywords: webkit
;; Version: 0.1.0
;; Package-Requires: ()
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
;; This library is heavily inspired by Kite mini library. Kudos Tung Dao
;; for his great work.
;;
;;; Commentary:
;; Personally I switch between many many tasks constantly during a week.
;; It's really hard to deal with the context switch and some times I forget
;; about what I was doing on a project or why the other task was blocked.
;; This library helps me which keeping my state of mind when I'm leaving a
;; Task for another one.
;;
;; This library simply creates a database of the WORK you do and keeps track
;; of some NOTES related to each WORK.  You can review the notes on each WORK
;; when ever you like via `fg42/brain-notes-for' function.  You can switch to
;; other state of mind with `fg42/brain-switch' which takes a note for your
;; current WORK and switches to a new state by showing you what ever note
;; you took on the same WORK previously.
;;; Code:
(require 'seq)
(require 'fg42/utils)

(defvar fg42/brain-state-file "~/.brain.state"
  "The path to the brain state db.")

(defvar fg42/brain-state-date-format "%Y-%m-%d %T")


(defun fg42/brain-state-create (current-work entries logs)
  "Create a new state out of the given ENTRIES and LOGS.
CURRENT-WORK is the key that is considered to be the current work."
  (list 'entry-map entries 'logs logs 'current current-work))


(defun fg42/-brain-entry-map (state)
  "Return the map of entries of the given STATE."
  (plist-get state 'entry-map))


(defun fg42/-brain-logs (state)
  "Return the list of entries of the given STATE."
  (plist-get state 'logs))


(defun fg42/-brain-current (state)
  "Return the current work of the given STATE."
  (plist-get state 'current))


(defun fg42/load-brain-state ()
  "Load the brain state from the state file."
  (if (file-exists-p fg42/brain-state-file)
      (file->lisp fg42/brain-state-file)
    (list)))


(defun fg42/save-brain-state (state)
  "Override the state on the brain state file by the given STATE."
  (lisp->file fg42/brain-state-file state))


(defun fg42/add-brain-entry (state work entry)
  "Add the given ENTRY under the key WORK in the given STATE."
  (let ((entries (assoc work (fg42/-brain-entry-map state)))
        (log (list :add (format-time-string fg42/brain-state-date-format) work)))
    (fg42/brain-state-create
     (fg42/-brain-current state)
     (cons (list work (cons entry (cadr entries)))
           (fg42/-brain-entry-map state))
     (cons log (fg42/-brain-logs state)))))


(defun fg42/brain-keys (state)
  "Return all the entry-map keys of the given STATE."
  (seq-reduce (lambda (acc x)
                (let ((v (car x)))

                  (if (member v acc)
                      acc
                    (cons v acc))))
              (fg42/-brain-entry-map state)
              '()))


(defun fg42/brain-delete-work (state work)
  "Remove the give WORK from the given STATE."
  (interactive
   (let ((state (fg42/load-brain-state)))
     (list state
           (completing-read "Work: " (fg42/brain-keys state)))))
  (fg42/save-brain-state
   (fg42/brain-state-create
    nil
    (seq-filter (lambda (x) (equal (car x) work))
                (fg42/-brain-entry-map state))
    (cons (list :delete
                (format-time-string fg42/brain-state-date-format)
                work)
          (fg42/-brain-logs state)))))



(defun fg42/brain-notes-for (state work)
  "Create a buffer with all the nosts of the given WORK in STATE."
  (interactive
   (let* ((state (fg42/load-brain-state)))
     (list state
           (completing-read "Work: " (fg42/brain-keys state) nil 'confirm))))
  (let ((buf (get-buffer-create (format "*%s-notes*" work)))
        (entries (cadr (assoc (intern work) (fg42/-brain-entry-map state)))))
    (set-buffer buf)
    (erase-buffer)
    (mapcar (lambda (entry)
              (insert (format "[%s]: %s\n"
                              (propertize (car entry) 'face 'font-lock-builtin-face)
                              (propertize (cadr entry) 'face 'bold))))
            entries)
    (switch-to-buffer buf)))


(defun fg42/brain-notes-for-current-work (state)
  "Create a buffer with all the nosts of the current work in STATE."
  (interactive
   (let* ((state (fg42/load-brain-state)))
     (list state)))
  (fg42/brain-notes-for state (fg42/-brain-current state)))


(defun fg42/brain-add-notes-for (state work note)
  "Add a note for the given WORK in STATE."
  (interactive
   (let* ((state (fg42/load-brain-state)))
     (list state
           (completing-read "Work: " (fg42/brain-keys state))
           (read-string "Note: "))))
  (fg42/save-brain-state
   (fg42/add-brain-entry state
                         work
                         (list (format-time-string fg42/brain-state-date-format)
                               note))))


(defun fg42/brain-switch (state src-work note dst-work)
  "Switch the brain focus from SRC-WORK to DST-WORK on the given STATE.
NOTE is the message that has to be saved for the SRC-WORK."
  (interactive
   (let* ((state (fg42/load-brain-state))
          (keys  (fg42/brain-keys state))
          (current (fg42/-brain-current state)))
     (list
      state
      (completing-read "What were you working on: " keys nil 'confirm)
      (read-string
       "Note for the current work: ")

      (completing-read "Switch brain to: "
                       keys))))

  (fg42/save-brain-state
   (plist-put
    (fg42/add-brain-entry state
                          src-work
                          (list (format-time-string fg42/brain-state-date-format)
                                note))
    'current
    (intern dst-work)))
  ;; We don't use the latest state for the target work
  (fg42/brain-notes-for state dst-work))


(provide 'fg42/brain-switch)
;;; brain-switch.el ends here
