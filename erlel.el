(require 'erlang)
(require 'projectile)
(require 'helm)

(defun erlel-grep-function (name module)
  (split-string
   (shell-command-to-string
    (concat "git grep -n ^" name " -- *" module ".erl"))
   "\n"))

(defun erlel-dwim ()
  (interactive)
  (let* ((id (erlang-get-identifier-at-point))
         (kind (erlang-id-kind id))
         (module (erlang-id-module id))
         (name (erlang-id-name id))
         (arity (erlang-id-arity id)))
    (cond ((and module name arity)
           (projectile-with-default-dir (projectile-project-root)
             (helm :sources
                   (helm-build-sync-source "Matches"
                     :candidates (lambda () (erlel-grep-function name module))
                     :action (lambda (s)
                               (seq-let (file line) (split-string s ":")
                                 (find-file file)
                                 (goto-char (point-min))
                                 (forward-line (1- (string-to-number line))))))))))))

(global-set-key (kbd "M-ä") 'erlel-dwim)
