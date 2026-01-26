;;; config.el --- Configuration -*- lexical-binding: t -*-

(when (file-directory-p "/run/current-system/sw/lib")
  (add-to-list 'treesit-extra-load-path "/run/current-system/sw/lib"))

(setq epg-pinentry-mode 'loopback)

(add-hook 'org-mode-hook 'visual-line-mode)

(defun rc/get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Consolas-13")
   ((eq system-type 'gnu/linux) "Iosevka Nerd Font-24")))

(add-to-list 'default-frame-alist `(font . ,(rc/get-default-font)))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)
(global-display-line-numbers-mode 1)

(setq-default c-basic-offset 4
              c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "bsd")))

(add-hook 'c-mode-hook (lambda ()
                         (interactive)
                         (c-toggle-comment-style -1)))

(add-hook 'rust-mode-hook
          (lambda ()
            (setq-local eglot-workspace-configuration
                        '(:rust-analyzer
                          (:cargo (:allFeatures t)
                           :rustfmt (:extraArgs ["--edition" "2021"]))))
            (add-hook 'before-save-hook 'eglot-format-buffer nil t)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'dired-x)
(require 'dired-aux)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq make-backup-files nil)
(setq auto-save-default nil)

(defvar my/base-dir "/home/tony")

(defun my/set-base-dir ()
  "Set the root directory for searches."
  (interactive)
  (setq my/base-dir (read-directory-name "Set base directory: " my/base-dir))
  (message "Search root set to: %s" my/base-dir))

(defun my/consult-fd ()
  "Find files from base dir."
  (interactive)
  (let ((default-directory my/base-dir))
    (consult-fd)))

(defun my/fzf-find-file ()
  "Find files from base dir using fzf."
  (interactive)
  (let ((default-directory my/base-dir))
    (fzf-find-file)))

(defun my/affe-find ()
  "Find files from base dir using affe (async fuzzy)."
  (interactive)
  (affe-find my/base-dir))

(load (expand-file-name "telescope.el" user-emacs-directory))

(defun my/telescope-find-files ()
  "Find files from base dir using telescope."
  (interactive)
  (telescope-find-files my/base-dir))

(defun my/consult-ripgrep ()
  "Ripgrep from base dir."
  (interactive)
  (consult-ripgrep my/base-dir))

(defun my/consult-ripgrep-symbol ()
  "Ripgrep symbol at point from base dir."
  (interactive)
  (consult-ripgrep my/base-dir (thing-at-point 'symbol t)))

(defun my/find-emacs-config ()
  "Find files in emacs config dir."
  (interactive)
  (let ((default-directory "/home/tony/.emacs.d/"))
    (consult-fd)))

(defun my/switch-project ()
  "Pick a project from ~/repos, set base-dir, open dired."
  (interactive)
  (let* ((repos-dir "~/repos/")
         (dirs (seq-filter
                (lambda (f) (file-directory-p (expand-file-name f repos-dir)))
                (directory-files repos-dir nil "^[^.]")))
         (chosen (completing-read "Project: " dirs nil t)))
    (when chosen
      (let ((project-dir (expand-file-name chosen repos-dir)))
        (setq my/base-dir project-dir)
        (dired project-dir)
        (message "Base dir: %s" project-dir)))))

(defun my/vterm-here ()
  "Open vterm in current window."
  (interactive)
  (let ((default-directory (or (and buffer-file-name (file-name-directory buffer-file-name))
                               default-directory))
        (display-buffer-alist nil))  ; bypass all display rules
    (pop-to-buffer-same-window (vterm "*vterm*"))))

(setq display-buffer-alist
      '(("\\*xref\\*\\|\\*compilation\\*\\|\\*grep\\*"
         (display-buffer-reuse-window display-buffer-below-selected)
         (window-height . 0.35))))

(defun my/close-popup-window ()
  "Close windows showing xref, compilation, grep, or help buffers."
  (interactive)
  (dolist (win (window-list))
    (when (string-match-p "\\*xref\\*\\|\\*compilation\\*\\|\\*grep\\*\\|\\*Help\\*"
                          (buffer-name (window-buffer win)))
      (delete-window win))))

(defun my/reformat-parenthesized-content ()
  "Reformat comma-separated content inside parentheses to multiple lines."
  (interactive)
  (let* ((line (thing-at-point 'line t))
         (inside (and (string-match "(\\([^)]+\\))" line)
                      (match-string 1 line))))
    (if (not inside)
        (message "No content found inside parentheses")
      (let* ((prefix (and (string-match "^\\(.*?\\)(" line)
                          (match-string 1 line)))
             (suffix (and (string-match ")\\(.*\\)$" line)
                          (match-string 1 line)))
             (parts (split-string inside "," t "[ \t]*"))
             (new-lines (list (concat prefix "("))))
        (dotimes (i (length parts))
          (let ((part (nth i parts)))
            (if (< i (1- (length parts)))
                (push (concat "        " part ",") new-lines)
              (push (concat "        " part) new-lines))))
        (push (concat "    )" (string-trim-right suffix)) new-lines)
        (setq new-lines (nreverse new-lines))
        (beginning-of-line)
        (kill-line 1)
        (insert (mapconcat 'identity new-lines "\n") "\n")))))

(add-hook 'prog-mode-hook
          (lambda ()
            (modify-syntax-entry ?- "w")
            (modify-syntax-entry ?_ "w")))

(require 'erc)
(require 'erc-services)
(erc-services-mode 1)

(setq erc-nick "tonybtw"
      erc-user-full-name "tony"
      erc-prompt-for-nickserv-password nil
      erc-nickserv-identify-mode 'autodetect
      erc-use-auth-source-for-nickserv-password t)

(setq erc-autojoin-channels-alist
      '(("libera.chat" "#technicalrenaissance")))

(defun my/erc-connect-libera ()
  "Connect to Libera.Chat via ERC."
  (interactive)
  (erc-tls :server "irc.libera.chat"
           :port 6697
           :nick "tonybtw"
           :user "tony"))
