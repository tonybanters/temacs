;;; keybinds.el --- All keybindings -*- lexical-binding: t -*-

;; C-c to escape insert mode
(define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)

;;; Global keys
(global-set-key (kbd "M-x") 'execute-extended-command)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m l") 'magit-log)

;;; Multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;;; Move text
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;;; Dired evil bindings
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map
    "." 'dired-create-empty-file
    "h" 'dired-up-directory
    "l" 'dired-find-file
    "n" 'evil-search-next
    "N" 'evil-search-previous))

;;; LSP keybindings
(with-eval-after-load 'eglot
  (evil-define-key 'normal eglot-mode-map
    "K" 'eldoc-box-help-at-point
    "gd" 'xref-find-definitions
    "gr" 'xref-find-references))

;;; Leader keybindings (SPC)
(evil-leader/set-key
  ;; files
  "ff" 'my/telescope-find-files
  "fF" 'my/fzf-find-file
  "fg" 'my/consult-ripgrep
  "fs" 'my/consult-ripgrep-symbol
  "fo" 'consult-recent-file
  "fl" 'consult-line
  "fi" 'my/find-emacs-config
  "fp" 'my/switch-project
  ;; buffers
  "bb" 'consult-buffer
  "fb" 'consult-buffer
  "bp" 'previous-buffer
  "bn" 'next-buffer
  "bd" 'kill-current-buffer
  "bm" 'ibuffer
  ;; custom
  "cr" 'my/set-base-dir
  "cd" (lambda () (interactive) (dired (file-name-directory (or buffer-file-name default-directory))))
  "cl" 'my/close-popup-window
  "cn" 'next-error
  "cp" 'previous-error
  "cc" 'compile
  "cm" 'recompile
  "cb" (lambda () (interactive) (compile "bear -- make"))
  ;; magit
  "ms" 'magit-status
  "ml" 'magit-log
  ;; ssh/servers
  "st" 'my/connect-tonydev
  ;; terminal
  "to" 'my/vterm-here
  ;; window
  "wv" 'split-window-right
  "ws" 'split-window-below
  "wd" 'delete-window
  "wh" 'evil-window-left
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  "wl" 'evil-window-right
  ;; quit
  "qq" 'save-buffers-kill-terminal)
