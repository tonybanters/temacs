;;; packages.el --- Package configuration -*- lexical-binding: t -*-

;;; evil mode (vim bindings)
(setq evil-want-keybinding nil)  ; required before loading evil-collection
(setq evil-search-module 'evil-search)  ; required for cgn
(setq evil-undo-system 'undo-redo)  ; use emacs 28+ native undo-redo
(rc/require 'evil 'evil-leader 'evil-collection 'evil-commentary)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-mode 1)
(evil-collection-init)
(evil-commentary-mode 1)

;;; Vertico + Consult + Orderless (telescope-like fuzzy finding)
(rc/require 'vertico 'consult 'orderless 'marginalia 'vertico-posframe 'fzf 'affe)
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

;; Flex matching (fzf-style: characters in sequence)
(setq orderless-matching-styles '(orderless-literal orderless-flex))

;; Affe (async fuzzy finder using orderless)
(setq affe-find-command "fd --color=never -t f")

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
(rc/require 'nix-mode 'zig-mode 'rust-mode 'php-mode 'web-mode 'go-mode 'typescript-mode)


;;; Tree-sitter text objects (vif, vaf, vic, vac, etc.)
(rc/require 'tree-sitter 'tree-sitter-langs 'evil-textobj-tree-sitter)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
(define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
(define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
(define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))

;;; CSS color preview
(rc/require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'php-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)
(add-hook 'js-mode-hook 'rainbow-mode)
(add-hook 'web-mode-hook 'rainbow-mode)
(add-hook 'scss-mode-hook 'rainbow-mode)
(add-hook 'conf-mode-hook 'rainbow-mode)
(add-hook 'toml-mode-hook 'rainbow-mode)
(add-hook 'yaml-mode-hook 'rainbow-mode)
(add-hook 'conf-toml-mode-hook 'rainbow-mode)

;;; Treesitter context (sticky function header)
(rc/require 'topsy)
(add-hook 'prog-mode-hook 'topsy-mode)

;;; Org mode
(rc/require 'org-superstar 'org-fancy-priorities)

(setq org-directory "~/org/")
(setq org-agenda-files '("~/repos/agendas/private.org"))

;; Pretty bullets
(add-hook 'org-mode-hook #'org-superstar-mode)
(setq org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆"))

;; Priority icons
(add-hook 'org-mode-hook #'org-fancy-priorities-mode)
(setq org-fancy-priorities-list '("⚑" "▲" "»"))

;; Syntax highlighting in code blocks
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-hide-block-startup nil
      org-src-preserve-indentation nil
      org-edit-src-content-indentation 0)

;; Hide emphasis markers (*bold*, /italic/, etc.)
(setq org-hide-emphasis-markers t)

;; Visual tweaks
(setq org-ellipsis " ▾")           ; nicer fold indicator
(setq org-startup-folded 'content) ; show headings on open
(add-hook 'org-mode-hook #'org-indent-mode) ; clean indentation

;; Make RET follow links and toggle checkboxes
(setq org-return-follows-link t)

;;; Org Present (presentation mode)
(rc/require 'org-present 'visual-fill-column)

(defun my/org-present-start ()
  ;; Smaller, more readable font scaling
  (setq-local face-remapping-alist
              '((default (:height 1.3) default)
                (header-line (:height 2.0) variable-pitch)
                (org-document-title (:height 1.5) org-document-title)
                (org-level-1 (:height 1.3) org-level-1)
                (org-level-2 (:height 1.2) org-level-2)
                (org-level-3 (:height 1.1) org-level-3)
                (org-code (:height 1.0) org-code)
                (org-block (:height 1.0) org-block)))
  ;; Center content
  (setq visual-fill-column-width 80)
  (setq visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  ;; Word wrap
  (visual-line-mode 1)
  ;; Hide UI
  (setq header-line-format " ")
  (display-line-numbers-mode 0)
  (org-display-inline-images))

(defun my/org-present-end ()
  (setq-local face-remapping-alist nil)
  (setq header-line-format nil)
  (visual-fill-column-mode 0)
  (visual-line-mode 0)
  (display-line-numbers-mode 1)
  (org-remove-inline-images))

(add-hook 'org-present-mode-hook #'my/org-present-start)
(add-hook 'org-present-mode-quit-hook #'my/org-present-end)

;;; LSP (eglot is built-in to Emacs 29+)
(require 'eglot)
(rc/require 'eldoc-box)

;; Auto-start LSP for these modes
(add-hook 'zig-mode-hook 'eglot-ensure)
(add-hook 'nix-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'php-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)

;; LSP server configurations
(with-eval-after-load 'eglot
  ;; PHP
  (add-to-list 'eglot-server-programs
               '(php-mode . ("intelephense" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(web-mode . ("intelephense" "--stdio")))
  ;; TypeScript/TSX (typescript-language-server)
  (add-to-list 'eglot-server-programs
               '(typescript-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(tsx-ts-mode . ("typescript-language-server" "--stdio"))))

;; File associations for TypeScript React
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

;; Go format on save
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'eglot-format-buffer nil t)))

(add-hook 'web-mode-hook 'eglot-ensure)


;;; Direnv integration (loads devshell environment)
(rc/require 'envrc)
(envrc-global-mode)

;;; vterm (terminal emulator)
(rc/require 'vterm)
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
;; (rc/require-theme 'gruber-darker)
(rc/require 'doom-themes)
(load-theme 'doom-palenight t)

;;; Clean up modeline (hide minor modes)
(setq eldoc-minor-mode-string nil)
(setq company-lighter nil)
(setq-default abbrev-mode nil)
(with-eval-after-load 'flymake (setq flymake-mode-line-format nil))
(with-eval-after-load 'envrc (setq envrc-lighter nil))
(with-eval-after-load 'evil-commentary (setq evil-commentary-mode-lighter nil))
