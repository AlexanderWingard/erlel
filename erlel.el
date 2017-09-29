(require 'erlang)
(require 'projectile)
(require 'helm)

(defun erlel-grep-function (name module)
  (split-string
   (shell-command-to-string
    (concat "git grep -n ^" name " -- *" module ".erl"))
   "\n"))

(defun erlel-grep-macro (name)
  (split-string
   (shell-command-to-string
    (concat "git grep -n define\\(" name " -- *.[eh]rl"))
   "\n"))

(defun erlel-grep-record (name)
  (split-string
   (shell-command-to-string
    (concat "git grep -n record\\(" name " -- *.[eh]rl"))
   "\n"))

(defun erlel-dwim ()
  (interactive)
  (let* ((id (erlang-get-identifier-at-point))
         (kind (erlang-id-kind id))
         (module (erlang-id-module id))
         (name (erlang-id-name id))
         (arity (erlang-id-arity id))
         (grepper (cond ((and module name arity)
                         (lambda () (erlel-grep-function name module)))
                        ((and (eq kind 'macro))
                         (lambda () (erlel-grep-macro name)))
                        ((and (eq kind 'record))
                         (lambda () (erlel-grep-record name))))))
    (if grepper
        (projectile-with-default-dir (projectile-project-root)
          (helm :sources
                (helm-build-sync-source "Matches"
                  :candidates grepper
                  :action (lambda (s)
                            (let* ((tokens (split-string s ":"))
                                   (file (nth 0 tokens))
                                   (line (nth 1 tokens)))
                              (find-file file)
                              (goto-char (point-min))
                              (forward-line (1- (string-to-number line))))))))
      (print id))))

(global-set-key (kbd "M-ä") 'erlel-dwim)
