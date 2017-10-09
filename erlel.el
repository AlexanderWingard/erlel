(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(dolist (dep '(erlang
               helm))
  (when (not (package-installed-p dep))
    (package-refresh-contents)
    (package-install dep)
    (require dep)))

(defun erlel-find-uses ()
  (interactive)
  (erlel-find-thing 'uses))

(defun erlel-find-definitions ()
  (interactive)
  (erlel-find-thing 'definition))

(defun erlel-grep (&rest args)
  (split-string (shell-command-to-string (apply 'concat args)) "\n" t))

(defun erlel-find-thing (what)
  (let* ((id (erlang-get-identifier-at-point))
         (kind (erlang-id-kind id))
         (module (erlang-id-module id))
         (name (erlang-id-name id))
         (matches
          (cond
           ((eq what 'definition)
            (cond ((and module name)
                   (erlel-grep "git grep -n ^" name "\\( -- :/*" module ".erl"))
                  ((and (eq kind 'macro))
                   (erlel-grep "git grep -n '\\-define(" name "[ ,(]' -- :/*.[eh]rl"))
                  ((and (eq kind 'record))
                   (erlel-grep "git grep -n '\\-record(" name "[ ,]' -- :/*.[eh]rl"))))

           ((eq what 'uses)
            (cond ((and module name)
                   (append
                    (erlel-grep "git grep -n " name " -- :/*" module ".erl")
                    (erlel-grep "git grep -n " module ":" name " -- :/*")))
                  ((and (eq kind 'macro))
                   (erlel-grep "git grep -n \\?" name " -- :/*"))
                  ((and (eq kind 'record))
                   (erlel-grep "git grep -n '#" name "' -- :/*")))))))
    (if matches
        (helm :sources
              (helm-build-sync-source "Matches"
                :candidates matches
                :action (lambda (s)
                          (let* ((tokens (split-string s ":"))
                                 (file (nth 0 tokens))
                                 (line (nth 1 tokens)))
                            (ring-insert find-tag-marker-ring (point-marker))
                            (find-file file)
                            (goto-char (point-min))
                            (forward-line (1- (string-to-number line)))))))
      (print id))))

(global-set-key (kbd "M-ä") 'erlel-find-definitions)
(global-set-key (kbd "M-å") 'erlel-find-uses)
