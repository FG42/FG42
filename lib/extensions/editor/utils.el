;; Functions -----------------------------
;;;###autoload
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is
not blank and we are not at the end of the line,
then comment current line.
Replaces default behaviour of comment-dwim, when
it inserts comment at the end of the line."

  ;; Original idea from
  ;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html

  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p))
           (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position)
                                   (line-end-position))
    (comment-dwim arg)))


;;;###autoload
(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
   Deletes whitespace at join.
   http://www.emacswiki.org/emacs/AutoIndentation"
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
             (just-one-space 0)
             (backward-char 1)
             (kill-line arg))
    (kill-line arg)))

;;;###autoload
(defun setup-utils ()
  "Setup several utitlies for FG42"
  (global-set-key (kbd "C-k") 'kill-and-join-forward)
  (global-set-key (kbd "M-;") 'comment-dwim-line)
  (global-set-key (kbd "M-j") (lambda ()
                                (interactive) (join-line -1))))
(defun callable-command (command)
  '(lambda '() (,command)))


(provide 'extensions/editor/utils)
