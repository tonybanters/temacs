;;; init.el --- Main entry point -*- lexical-binding: t -*-

(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(defvar rc/package-contents-refreshed nil)

(defun rc/package-refresh-contents-once ()
  (when (not rc/package-contents-refreshed)
    (setq rc/package-contents-refreshed t)
    (package-refresh-contents)))

(defun rc/require-one-package (package)
  (when (not (package-installed-p package))
    (rc/package-refresh-contents-once)
    (package-install package)))

(defun rc/require (&rest packages)
  (dolist (package packages)
    (rc/require-one-package package)))

(defun rc/require-theme (theme)
  (let ((theme-package (intern (concat (symbol-name theme) "-theme"))))
    (rc/require theme-package)
    (load-theme theme t)))

; Load config files
(load (expand-file-name "packages.el" user-emacs-directory))
(load (expand-file-name "config.el" user-emacs-directory))
(load (expand-file-name "keybinds.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load-file custom-file))
