;;; telescope.el --- Telescope-like fuzzy finder -*- lexical-binding: t -*-

(require 'posframe)

(defgroup telescope nil
  "Telescope-like fuzzy finder."
  :group 'convenience)

(defcustom telescope-fd-command "fd -t f --color=never --hidden --exclude .git"
  "Command to list files."
  :type 'string
  :group 'telescope)

(defcustom telescope-results-width-pct 0.35
  "Width of results window as percentage of frame."
  :type 'float
  :group 'telescope)

(defcustom telescope-preview-width-pct 0.50
  "Width of preview window as percentage of frame."
  :type 'float
  :group 'telescope)

(defcustom telescope-height-pct 0.70
  "Height of telescope as percentage of frame."
  :type 'float
  :group 'telescope)

(defcustom telescope-gap 2
  "Gap between results and preview panes in characters."
  :type 'integer
  :group 'telescope)

(defcustom telescope-padding 2
  "Internal padding within panes in characters."
  :type 'integer
  :group 'telescope)

;; Internal state
(defvar telescope--input "")
(defvar telescope--files nil)
(defvar telescope--filtered nil)
(defvar telescope--selected 0)
(defvar telescope--base-dir nil)
(defvar telescope--results-buffer " *telescope-results*")
(defvar telescope--preview-buffer " *telescope-preview*")

(defun telescope--get-files (dir)
  "Get all files in DIR using fd."
  (let ((default-directory dir))
    (split-string
     (shell-command-to-string (concat telescope-fd-command " ."))
     "\n" t)))

(defun telescope--fzf-filter (files query)
  "Filter FILES with QUERY using fzf --filter."
  (if (or (null files) (string-empty-p query))
      (seq-take files 100)
    (with-temp-buffer
      (insert (string-join files "\n"))
      (call-process-region (point-min) (point-max) "fzf" t t nil "--filter" query)
      (split-string (buffer-string) "\n" t))))

(defun telescope--render-results ()
  "Render the results buffer."
  (with-current-buffer (get-buffer-create telescope--results-buffer)
    (let* ((inhibit-read-only t)
           (pad (make-string telescope-padding ?\s))
           (max-results (- telescope--height 5))  ; leave room for prompt/footer/padding
           (content-width (- telescope--results-width (* telescope-padding 2) 2))
           (separator (make-string (min content-width 80) ?─)))
      (erase-buffer)
      ;; Top padding
      (insert "\n")
      ;; Prompt
      (insert pad)
      (insert (propertize "> " 'face '(:foreground "#87afff" :weight bold)))
      (insert telescope--input)
      (insert (propertize "█" 'face 'cursor))
      (insert "\n")
      (insert pad)
      (insert (propertize separator 'face '(:foreground "#444444")))
      (insert "\n")
      ;; Results
      (if (null telescope--filtered)
          (progn
            (insert pad)
            (insert (propertize "  No matches\n" 'face '(:foreground "#666666" :slant italic))))
        (let ((idx 0))
          (dolist (file (seq-take telescope--filtered max-results))
            (let* ((max-len (- content-width 4))
                   (display (if (> (length file) max-len)
                                (concat "..." (substring file (- 3 max-len)))
                              file))
                   (selected (= idx telescope--selected))
                   (face (if selected
                             '(:background "#3a3a3a" :weight bold)
                           nil))
                   (prefix (if selected "→ " "  ")))
              (insert pad)
              (insert (propertize (concat prefix display "\n") 'face face)))
            (cl-incf idx))))
      ;; Footer
      (insert pad)
      (insert (propertize separator 'face '(:foreground "#444444")))
      (insert "\n")
      (insert pad)
      (insert (propertize (format "%d/%d"
                                  (min (1+ telescope--selected) (length telescope--filtered))
                                  (length telescope--filtered))
                          'face '(:foreground "#666666"))))))

(defun telescope--update-preview ()
  "Update preview buffer with selected file content."
  (with-current-buffer (get-buffer-create telescope--preview-buffer)
    (let ((inhibit-read-only t)
          (pad (make-string telescope-padding ?\s)))
      (erase-buffer)
      (if-let* ((selected (nth telescope--selected telescope--filtered))
                (path (expand-file-name selected telescope--base-dir))
                (_ (and (file-exists-p path)
                        (file-regular-p path)
                        (not (file-directory-p path)))))
          (condition-case nil
              (progn
                ;; Top padding
                (insert "\n")
                ;; File name header
                (insert pad)
                (insert (propertize (file-name-nondirectory path) 'face '(:foreground "#87afff" :weight bold)))
                (insert "\n")
                (insert pad)
                (insert (propertize (make-string (min 60 (- telescope--preview-width 4)) ?─) 'face '(:foreground "#444444")))
                (insert "\n")
                ;; Content with left padding
                (let ((start (point)))
                  (insert-file-contents path nil 0 10000)
                  (goto-char start)
                  ;; Add padding to each line
                  (while (not (eobp))
                    (insert pad)
                    (forward-line 1)))
                (goto-char (point-min))
                ;; Apply syntax highlighting
                (when-let ((mode (assoc-default path auto-mode-alist 'string-match)))
                  (delay-mode-hooks (funcall mode))
                  (font-lock-ensure)))
            (error
             (erase-buffer)
             (insert "\n" pad)
             (insert (propertize "Cannot preview file" 'face '(:foreground "#666666")))))
        (insert "\n" pad)
        (insert (propertize "No preview available" 'face '(:foreground "#666666")))))))

(defvar telescope--results-x 0)
(defvar telescope--results-y 0)
(defvar telescope--preview-x 0)
(defvar telescope--results-width 55)
(defvar telescope--preview-width 75)
(defvar telescope--height 25)

(defun telescope--calc-dimensions ()
  "Calculate frame dimensions based on percentages."
  (let ((frame-cols (frame-width))
        (frame-rows (frame-height)))
    (setq telescope--results-width (max 30 (floor (* frame-cols telescope-results-width-pct))))
    (setq telescope--preview-width (max 40 (floor (* frame-cols telescope-preview-width-pct))))
    (setq telescope--height (max 15 (floor (* frame-rows telescope-height-pct))))))

(defun telescope--calc-positions ()
  "Calculate frame positions."
  (telescope--calc-dimensions)
  (let* ((char-width (frame-char-width))
         (char-height (frame-char-height))
         (border-px 4)
         (gap-px (* telescope-gap char-width))
         (total-pixel-width (+ (* telescope--results-width char-width)
                               (* telescope--preview-width char-width)
                               (* 2 border-px)
                               gap-px))
         (total-pixel-height (* telescope--height char-height))
         (start-x (max 0 (/ (- (frame-pixel-width) total-pixel-width) 2)))
         (start-y (max 0 (/ (- (frame-pixel-height) total-pixel-height) 2))))
    (setq telescope--results-x start-x)
    (setq telescope--results-y start-y)
    (setq telescope--preview-x (+ start-x (* telescope--results-width char-width) border-px gap-px))))

(defun telescope--results-poshandler (_info)
  "Position handler for results frame."
  (cons telescope--results-x telescope--results-y))

(defun telescope--preview-poshandler (_info)
  "Position handler for preview frame."
  (cons telescope--preview-x telescope--results-y))

(defun telescope--show-frames ()
  "Display the telescope posframes."
  (telescope--calc-positions)
  ;; Results frame (left)
  (posframe-show telescope--results-buffer
                 :position (point-min)
                 :poshandler #'telescope--results-poshandler
                 :width telescope--results-width
                 :height telescope--height
                 :border-width 2
                 :border-color "#5f5fff"
                 :background-color "#1c1c1c"
                 :foreground-color "#d0d0d0"
                 :override-parameters '((cursor-type . nil)))
  ;; Preview frame (right)
  (posframe-show telescope--preview-buffer
                 :position (point-min)
                 :poshandler #'telescope--preview-poshandler
                 :width telescope--preview-width
                 :height telescope--height
                 :border-width 2
                 :border-color "#5f87af"
                 :background-color "#1c1c1c"
                 :foreground-color "#d0d0d0"
                 :override-parameters '((cursor-type . nil))))

(defun telescope--hide-frames ()
  "Hide telescope posframes."
  (posframe-hide telescope--results-buffer)
  (posframe-hide telescope--preview-buffer))

(defun telescope--refresh ()
  "Refresh filtered results and display."
  (setq telescope--filtered (telescope--fzf-filter telescope--files telescope--input))
  (setq telescope--selected (min telescope--selected
                                  (max 0 (1- (length telescope--filtered)))))
  (telescope--render-results)
  (telescope--update-preview)
  (telescope--show-frames))

(defun telescope--select-next ()
  "Select next item."
  (when telescope--filtered
    (let ((max-visible (- telescope--height 5)))
      (setq telescope--selected
            (min (1+ telescope--selected)
                 (1- (min (length telescope--filtered) max-visible)))))
    (telescope--render-results)
    (telescope--update-preview)
    (telescope--show-frames)))

(defun telescope--select-prev ()
  "Select previous item."
  (setq telescope--selected (max 0 (1- telescope--selected)))
  (telescope--render-results)
  (telescope--update-preview)
  (telescope--show-frames))

(defun telescope--backspace ()
  "Delete last character from input."
  (when (> (length telescope--input) 0)
    (setq telescope--input (substring telescope--input 0 -1))
    (telescope--refresh)))

(defun telescope--insert-char (char)
  "Insert CHAR into input."
  (setq telescope--input (concat telescope--input (char-to-string char)))
  (telescope--refresh))

;;;###autoload
(defun telescope-find-files (&optional dir)
  "Find files in DIR using telescope interface."
  (interactive)
  (let ((dir (expand-file-name (or dir default-directory))))
    (setq telescope--base-dir dir)
    (setq telescope--input "")
    (setq telescope--selected 0)
    (setq telescope--files (telescope--get-files dir))
    (setq telescope--filtered (seq-take telescope--files 100))
    (telescope--render-results)
    (telescope--update-preview)
    (telescope--show-frames)
    (let ((result nil))
      (unwind-protect
          (while (null result)
            (let ((key (read-key (propertize " " 'face '(:height 0.1)))))
              (cond
               ;; Quit
               ((memq key '(?\e ?\C-g ?\C-c))
                (setq result 'cancel))
               ;; Confirm selection
               ((memq key '(?\r ?\C-m))
                (if telescope--filtered
                    (setq result (expand-file-name
                                   (nth telescope--selected telescope--filtered)
                                   telescope--base-dir))
                  (setq result 'cancel)))
               ;; Navigation
               ((or (memq key '(?\C-n ?\C-j)) (equal key 'down))
                (telescope--select-next))
               ((or (memq key '(?\C-p ?\C-k)) (equal key 'up))
                (telescope--select-prev))
               ;; Scroll
               ((memq key '(?\C-d))
                (dotimes (_ 5) (telescope--select-next)))
               ((memq key '(?\C-u))
                (dotimes (_ 5) (telescope--select-prev)))
               ;; Delete
               ((memq key '(?\C-h ?\C-? 127 backspace))
                (telescope--backspace))
               ;; Clear input
               ((memq key '(?\C-w))
                (setq telescope--input "")
                (telescope--refresh))
               ;; Regular character input
               ((and (characterp key) (>= key 32) (<= key 126))
                (telescope--insert-char key)))))
        ;; Cleanup
        (telescope--hide-frames))
      ;; Handle result
      (unless (eq result 'cancel)
        (find-file result)))))

(provide 'telescope)
;;; telescope.el ends here
