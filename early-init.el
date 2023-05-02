;; speed up startup
(setq my/old-gc-cons-treshold gc-cons-threshold)
(setq gc-cons-threshold 500000000)
(message "gc-cons-threshold raised to %s" gc-cons-threshold)
(run-with-idle-timer
 3 nil
 (lambda ()
   (setq gc-cons-threshold my/old-gc-cons-treshold)
   (message "gc-cons-threshold restored to %s" gc-cons-threshold)))

(setq package-enable-at-startup nil)
(org-babel-load-file (expand-file-name "init.org" user-emacs-directory))
(delete-file (expand-file-name "init.el" user-emacs-directory))
