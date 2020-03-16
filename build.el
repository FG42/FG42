(add-to-list 'load-path (concat (getenv "HOME") ".fg42/lib"))

(defvar bootstrap-version nil
  "Bootstrap version of straight.  This var is used in straight's installer.")

(defun fpkg-initialize ()
  "Initilize the straight.e package manager and setup necessary hooks."
  (let ((bootstrap-file "~/.fg42/.fpkg/straight/repos/straight.el/bootstrap.el")
        (bootstrap-version 5))

    (setq straight-base-dir "~/.fg42/.fpkg/")
    (if (not (file-exists-p bootstrap-file))
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp))
      (load bootstrap-file nil 'nomessage))))

(fpkg-initialize)
