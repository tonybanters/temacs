;;; packages.el --- Package configuration -*- lexical-binding: t -*-

;;; evil mode (vim bindings)
(rc/require 'evil 'evil-leader 'evil-collection 'evil-commentary)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-mode 1)
(evil-collection-init)
(evil-commentary-mode 1)

;;; Vertico + Consult + Orderless (telescope-like fuzzy finding)
(rc/require 'vertico 'consult 'orderless 'marginalia 'vertico-posframe)
(vertico-mode 1)
(vertico-posframe-mode 1)
(marginalia-mode 1)
(recentf-mode 1)

(setq vertico-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))
(setq vertico-posframe-poshandler #'posframe-poshandler-frame-center)

(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion)))))

(setq consult-fd-args '("fd" "--color=never" "--type" "f" "--hidden" "--follow" "--exclude" ".git"))

;; Live preview as you navigate
(setq consult-preview-key 'any)

;;; magit
(rc/require 'magit)
(setq magit-auto-revert-mode nil)

;;; multiple cursors
(rc/require 'multiple-cursors)

;;; Move Text
(rc/require 'move-text)

;;; Company (autocompletion)
(rc/require 'company)
(global-company-mode)

;;; Language modes
(rc/require 'zig-mode 'rust-mode)

;;; LSP (eglot is built-in to Emacs 29+)
(require 'eglot)
(rc/require 'eldoc-box)

;; Auto-start LSP for these modes
(add-hook 'zig-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)


;;; Direnv integration (loads devshell environment)
(rc/require 'envrc)
(envrc-global-mode)

;;; vterm (terminal emulator)
(require 'vterm)
(defun rc/find-shell ()
  "Find a suitable shell, checking common locations."
  (or (getenv "SHELL")
      (seq-find #'file-executable-p
                '("/bin/bash"                      ; FHS standard
                  "/usr/bin/bash"                  ; Some distros
                  "/run/current-system/sw/bin/bash" ; NixOS
                  "/bin/sh"))                      ; Ultimate fallback
      "/bin/sh"))
(setq vterm-shell (rc/find-shell))
(setq vterm-kill-buffer-on-exit t)

;;; Theme
(rc/require-theme 'gruber-darker)

;;; Clean up modeline (hide minor modes)
(setq eldoc-minor-mode-string nil)
(setq company-lighter nil)
(setq-default abbrev-mode nil)
(with-eval-after-load 'flymake (setq flymake-mode-line-format nil))
(with-eval-after-load 'envrc (setq envrc-lighter nil))
(with-eval-after-load 'evil-commentary (setq evil-commentary-mode-lighter nil))
