;;; init.el --- Emacs initialization file
;;; Commentary:
;;; Code:

;;; PACKAGE MANAGEMENT ---------------------------------------------------------

;; setup straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; setup use-package
(straight-use-package 'use-package)
(use-package straight
  :custom (straight-use-package-by-default t))

;;; CUSTOM DEFINITIONS ---------------------------------------------------------

(defun my/split-switch-below ()
  "Split and switch to window below."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun my/split-switch-right ()
  "Split and switch to window on the right."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun my/smart-home ()
  "Jump to beginning of line or first non-whitespace."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point)) (beginning-of-line))))

(defun my/terminal ()
  "Open the terminal."
  (interactive)
  (eat "bash"))

(defun my/switch-to-terminal ()
  "Create or switch to the terminal buffer."
  (interactive)
  (let ((term-win (get-buffer-window "*eat*")))
    (if
        (eq term-win nil)
        (progn
          (my/split-switch-right)
          (my/terminal))
      (select-window term-win))))

(defun my/dashboard ()
  "Switch to a custom dashboard buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*my-dashboard*"))
  (read-only-mode 0)
  (centaur-tabs-local-mode 1)
  (setq-local mode-line-format nil
              cursor-type nil)
  (erase-buffer)
  (dashboard-insert-banner)
  (call-interactively #'beginning-of-buffer)
  (newline
   (/
    (-
     (window-height)
     (count-lines (point-min) (point-max))
     5)
    2))
  (cd "~")
  (read-only-mode 1)
  (message nil))

(defun my/eat-reset ()
  "Reset eat and input newline."
  (interactive)
  (eat-reset)
  (eat-self-input 1 ?\n))

;;; APPEARANCE -----------------------------------------------------------------

;; vanilla stuff
(blink-cursor-mode 0)
(menu-bar-mode     0)
(scroll-bar-mode   0)
(tool-bar-mode     0)
(global-prettify-symbols-mode 1)

(defvar my/default-font "IosevkaNerdFontMono")
(set-frame-font my/default-font)
(setq-default tab-width 4)
(setq use-dialog-box nil
      inhibit-startup-screen t
      initial-scratch-message "")

;; yes or no in a single keystroke
(defalias 'yes-or-no-p 'y-or-n-p)

;; no "when done with this frame" message in emacsclient
(use-package server :custom (server-client-instructions nil))

;; line numbers
(use-package display-line-numbers
  :custom (display-line-numbers-type 'relative)
  :config
  (set-face-background 'line-number nil)
  (global-display-line-numbers-mode 1))

;; theme
(use-package gruvbox-theme
  :custom (custom-safe-themes '("72ed8b6bffe0bfa8d097810649fd57d2b598deef47c992920aef8b5d9599eefe" default))
  :config (load-theme 'gruvbox-dark-medium))

;; dashboard
(use-package dashboard
  :custom
  (dashboard-banner-logo-title (concat "Welcome back, " user-full-name "!"))
  (dashboard-startup-banner (expand-file-name "splash.png" user-emacs-directory))
  :config
  (set-face-attribute 'dashboard-banner-logo-title nil :height 200))
(add-hook 'after-init-hook #'my/dashboard)

;; modeline
(use-package telephone-line
  :custom
  (telephone-line-lhs
   '((accent . (telephone-line-vc-segment
                telephone-line-process-segment))
     (nil    . (telephone-line-project-segment
                telephone-line-buffer-segment)))))
:config (telephone-line-mode 1)

;; tabs
(use-package all-the-icons)
(use-package centaur-tabs
  :custom
  (centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-modified-marker "●")
  (centaur-tabs-set-bar 'under)
  (centaur-tabs-set-close-button nil)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-style "bar")
  (x-underline-at-descent-line 1)
  :config
  (centaur-tabs-mode 1)
  (centaur-tabs-change-fonts my/default-font 100)
  (centaur-tabs-headline-match))

;; completion UI
(use-package vertico
  :custom
  (vertico-count 30)
  (vertico-cycle t)
  :config (vertico-mode 1))

;; better syntax highlighting
(use-package tree-sitter
  :config (global-tree-sitter-mode 1)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))
(use-package tree-sitter-langs)

;; indent guides
(use-package highlight-indent-guides
  :custom (highlight-indent-guides-responsive 'stack)
  :hook (prog-mode . highlight-indent-guides-mode))

;; show whitespace
(use-package whitespace
  :config (global-whitespace-mode 1)
  :custom (whitespace-style '(face tab-mark trailing missig-newline-at-eof)))

;;; POPUP CONTROL --------------------------------------------------------------

(use-package popwin
  :config
  ;;(push "*undo-tree*" popwin:special-display-config)
  ;;(push "*Help*"      popwin:special-display-config)
  (push "*Backtrace*" popwin:special-display-config)
  (push '("^[*]" :regex t) popwin:special-display-config)
  (popwin-mode 1))

;;; TEMPORARY FILES ------------------------------------------------------------

(defvar my/temp-dir (concat user-emacs-directory "temp/"))
(setq backup-directory-alist         `(("." . ,my/temp-dir))
      auto-save-file-name-transforms `((".*"  ,my/temp-dir t))
      auto-save-list-file-prefix               my/temp-dir)

;;; INFORMATION TOOLS ----------------------------------------------------------

;; fill column
(add-hook 'display-fill-column-indicator-mode-hook
          (lambda () (set-fill-column 80)))
(global-display-fill-column-indicator-mode)

;; show keybindings on input
(use-package which-key
  :custom
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0)
  :config
  (which-key-mode 1)
  (which-key-setup-side-window-bottom))

;; frecency-based sorting
(use-package prescient
  :config (prescient-persist-mode 1)
  :custom (prescient-save-file (concat my/temp-dir "prescient-save.el")))
(use-package vertico-prescient :config (vertico-prescient-mode 1))

;; better selection functions
(use-package consult :init (recentf-mode 1))

;; extra info in some completions
(use-package marginalia :config (marginalia-mode 1))

;; better parenthesis
(use-package rainbow-delimiters
  :hook prog-mode
  :custom (rainbow-delimiters-max-face-count 6)
  :config
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#cc241d")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#98971a")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#d79921")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#458588")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#b16286")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#689d6a"))

;; git gutter
(use-package git-gutter
  :custom
  (git-gutter:added-sign    "+")
  (git-gutter:modified-sign "~")
  (git-gutter:deleted-sign  "-")
  (git-gutter:update-timer 2)
  :config
  (set-face-background 'git-gutter:added    nil)
  (set-face-background 'git-gutter:modified nil)
  (set-face-background 'git-gutter:deleted  nil)
  (global-git-gutter-mode 1))

;;; EDITING --------------------------------------------------------------------

(cua-mode 1)

(use-package multiple-cursors)

;; jump to things
(use-package avy
  :custom
  (avy-keys
   (nconc
    (number-sequence ?a ?z)
    (number-sequence ?A ?Z)
    (number-sequence ?1 ?9)
    '(?0))))

;; better undo
(use-package undo-tree
  :custom (undo-tree-history-directory-alist `(("." . ,my/temp-dir)))
  :config (global-undo-tree-mode 1))

;;; TERMINAL -------------------------------------------------------------------

(use-package eat
  :bind (:map eat-semi-char-mode-map ("C-l" . #'my/eat-reset)))

;;; PROGRAMMING BASICS ---------------------------------------------------------

(use-package project)

;; autocompletion
(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-show-numbers t))

;; language server support
(use-package lsp-mode
  :custom (lsp-headerline-breadcrumb-enable nil)
  :hook
  (go-mode      . lsp-deferred)
  (haskell-mode . lsp-deferred))

(use-package lsp-ui
  :custom
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil))

;; formatting
(use-package format-all
  :hook prog-mode
  (format-all-mode . format-all-ensure-formatter)
  :config
  (push '("Haskell" stylish-haskell) format-all-default-formatters))

;; error checking
(use-package flycheck :config (global-flycheck-mode 1))

;; snippets
(use-package yasnippet :config (yas-global-mode 1))

;; TODO highlighting
(use-package hl-todo :config (global-hl-todo-mode 1))

;; electricity
(electric-indent-mode 1)
(electric-pair-mode 1)

;;; LANGUAGES ------------------------------------------------------------------

;; lisp
(use-package lisp-extra-font-lock :config (lisp-extra-font-lock-global-mode 1))
(use-package parinfer-rust-mode
  :hook emacs-lisp-mode
  :custom
  (parinfer-rust-library-directory my/temp-dir)
  (parinfer-rust-auto-download t))

(add-hook
 'emacs-lisp-mode-hook
 #'(lambda ()
     (electric-indent-local-mode 0)
     (electric-pair-local-mode 0)))

;; haskell
(use-package haskell-mode
  :bind (:map haskell-mode-map ("C-x C-p" . #'haskell-interactive-switch))
  :hook
  (haskell-interactive-mode
   . (lambda ()
       (bind-key "C-p" #'haskell-interactive-mode-history-previous 'haskell-interactive-mode-map)
       (bind-key "C-n" #'haskell-interactive-mode-history-next     'haskell-interactive-mode-map))))

(use-package lsp-haskell)

;; golang
(use-package go-mode
  :bind (:map go-mode-map ("C-x C-p" . #'my/switch-to-terminal)))

;; rust
(use-package rustic)

;;; KEYBINDINGS ----------------------------------------------------------------

;; remove all existing keybinds
;;(setq my-global-map (make-keymap))
;;(substitute-key-definition
;; 'self-insert-command 'self-insert-command
;; my-global-map global-map)
;;(use-global-map my-global-map)

(defmacro my/bind-keys* (&rest body)
  "Globally bind all keys.
BODY: a list of alternating key-function arguments."
  `(progn
     ,@(cl-loop
        while body collecting
        `(bind-key* ,(pop body) ,(pop body)))))

(my/bind-keys*
 ;; menus
 "C-x C-f" #'find-file
 "C-x C-g" #'consult-ripgrep
 "C-x C-k" (lambda () (interactive) (kill-buffer (current-buffer)))
 "C-x C-s" #'consult-buffer
 "C-x C-t" #'my/switch-to-terminal
 "C-x C-u" #'undo-tree-visualize

 ;; window control
 "C-<next>"  #'centaur-tabs-forward
 "C-<prior>" #'centaur-tabs-backward
 "C-x C-0"   #'delete-window
 "C-x C-1"   #'delete-other-windows
 "C-x C-2"   #'my/split-switch-below
 "C-x C-3"   #'my/split-switch-right
 "C-x C-4"   #'kill-buffer-and-window

 ;; movement
 "C-#"   #'next-window-any-frame
 "C-M-#" #'previous-window-any-frame
 "C-a"   #'my/smart-home
 "M-c"   #'avy-goto-word-1
 "M-l"   #'consult-goto-line
 "M-n"   #'scroll-up-command
 "M-p"   #'scroll-down-command
 "M-s"   #'consult-line

 ;; editing
 "C-,"     #'mc/mark-previous-like-this
 "C-."     #'mc/mark-next-like-this
 "C-<tab>" #'format-all-buffer
 "C-s"     #'save-buffer
 "C-y"     #'undo-tree-redo
 "C-z"     #'undo-tree-undo

 ;; LSP stuff
 "C-c C-a"     #'lsp-execute-code-action
 "C-c C-f C-d" #'lsp-ui-peek-find-definitions
 "C-c C-f C-i" #'lsp-ui-peek-find-implementation
 "C-c C-f C-r" #'lsp-ui-peek-find-references
 "C-c C-o"     #'lsp-organize-imports
 "C-c C-r"     #'lsp-rename

 ;; text scale
 "C-+" #'text-scale-increase
 "C--" #'text-scale-decrease
 "C-=" #'text-scale-mode

 ;; utils
 "C-x C-a" #'mark-whole-buffer
 "C-x C-r" (lambda () (interactive) (load-file user-init-file))

 ;; help
 "C-h C-b" #'describe-personal-keybindings
 "C-h C-f" #'describe-function
 "C-h C-k" #'describe-key
 "C-h C-v" #'describe-variable)

(start-process
 "startup-notify" nil
 "notify-send" "emacs"
 (format "Startup took %s!" (emacs-init-time)))

(provide 'init)
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:
;;; init.el ends here
