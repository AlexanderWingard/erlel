(dolist (dep '(erlang
               projectile
               helm))
  (when (not (package-installed-p dep))
    (package-install dep)
    (require dep)))

(defun erlel-grep-function (name module)
  (erlel-grep "git grep -n ^" name "\\( -- :/*" module ".erl"))

(defun erlel-grep-macro (name)
  (erlel-grep "git grep -n '\\-define(" name "[ ,(]' -- :/*.[eh]rl"))

(defun erlel-grep-record (name)
  (erlel-grep "git grep -n '\\-record(" name "[ ,]' -- :/*.[eh]rl"))

(defun erlel-grep (&rest args)
  (split-string
   (shell-command-to-string
    (apply 'concat args))
   "\n"
   t))

(defun erlel-dwim ()
  (interactive)
  (let* ((id (erlang-get-identifier-at-point))
         (kind (erlang-id-kind id))
         (module (erlang-id-module id))
         (name (erlang-id-name id))
         (arity (erlang-id-arity id))
         (matches (cond ((and module name arity)
                         (erlel-grep-function name module))
                        ((and (eq kind 'macro))
                         (erlel-grep-macro name))
                        ((and (eq kind 'record))
                         (erlel-grep-record name)))))
    (if matches
        (helm :sources
              (helm-build-sync-source "Matches"
                :candidates matches
                :action (lambda (s)
                          (let* ((tokens (split-string s ":"))
                                 (file (nth 0 tokens))
                                 (line (nth 1 tokens)))
                            (find-file file)
                            (goto-char (point-min))
                            (forward-line (1- (string-to-number line)))))))
      (print id))))

(global-set-key (kbd "M-ä") 'erlel-dwim)
