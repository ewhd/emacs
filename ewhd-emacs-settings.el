;;; ewhd-emacs-settings.el --- built-in Emacs settings
;; -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Custom Interactive Functions:
(defun ewhd-switch-to-last-buffer ()
  "Switch to the previously used buffer.

This behaves like repeatedly invoking `switch-to-buffer` with no argument,
toggling between the current buffer and the last one."
  (interactive)
  (switch-to-buffer nil))


(defun xah-toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
always cycle in this order: Init Caps, ALL CAPS, all lower.

URL `http://xahlee.info/emacs/emacs/modernization_upcase-word.html'
Version 2020-06-26"
  (interactive)
  (let (
        (deactivate-mark nil)
        $p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alpha:]")
        (setq $p1 (point))
        (skip-chars-forward "[:alpha:]")
        (setq $p2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region $p1 $p2)
      (put this-command 'state 1))
     ((equal 1 (get this-command 'state))
      (upcase-region $p1 $p2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region $p1 $p2)
      (put this-command 'state 0)))))


(defun ewhd-increment-number-decimal (&optional arg)
  "Increment the decimal number at point by 'ARG', preserving its width.

Moves backward to the start of the current number, increments it, and
replaces it with a zero-padded result of the same length. If the result
would be negative, it wraps within the field’s width. With no prefix
argument, increments by 1.

URL `https://www.emacswiki.org/emacs/IncrementNumber'"
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))


(defun ewhd-decrement-number-decimal (&optional arg)
  "Decrement the number forward from point by 'ARG'.

URL `https://www.emacswiki.org/emacs/IncrementNumber'"
  (interactive "p*")
  (ewhd-increment-number-decimal (if arg (- arg) -1)))


(defun ewhd-increment-string (string)
  "Return a copy of STRING with its first decimal number incremented by one.

If STRING contains a sequence of digits, only the first such sequence is
located and increased. All surrounding text is preserved. If no number is
found, the function returns STRING unchanged."
  (interactive "*")
  (setq start (string-match "\\([0-9]+\\)" string))
  (setq end (match-end 0))
  (setq number (string-to-number (substring string start end)))
  (setq new-num-string (number-to-string (+ 1 number)))
  (concat (substring string 0 start) new-num-string (substring string end)))


(defun ewhd-yank-increment ()
  "Yank the latest kill, incrementing the first number found before inserting it.

The updated text is inserted at point and also pushed back onto the kill
ring, replacing the top entry."
  (interactive "*")
  (setq new-text (ewhd-increment-string (current-kill 0)))
  (insert-for-yank new-text)
  (kill-new new-text t))


(defun ewhd-server-shutdown ()
  "Save modified buffers and cleanly terminate the current Emacs session.

  Prompts to save any unsaved buffers according to `save-some-buffers`,
  then exits Emacs.

URL `https://emacs.stackexchange.com/a/55799'"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; Change C-w behavior when no
(defadvice kill-region (before unix-werase activate compile)
  "When called interactively with no active region, delete a single word backwards instead.

URL `https://www.reddit.com/r/emacs/comments/16rbsnw/comment/k23kmxg/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button'"
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (save-excursion (backward-word 1) (point)) (point)))))

(defun ewhd-split-window-sensibly (&optional window)
  "Split 'WINDOW' sensibly, preferring a vertical (side-by-side) split.

If 'WINDOW' can be split vertically, `split-window-right` is used; otherwise,
falls back to a horizontal (top-bottom) split with `split-window-below`.
Defaults to the selected window if none is provided."
  (interactive)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             (with-selected-window window
               (split-window-below))))))


;;; General Behavior and Settings:
(setq-default fill-column 80)           ; Wrap at 80 chars
(setq-default truncate-lines nil)       ; Allow wrapping by default
(setq-default visual-line-mode t)       ; Wrap at word boundaries
(setq-default comment-column 40)        ; Comments start at column 40
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default scroll-margin 2)          ; Add a margin when scrolling vertically
(setq-default show-trailing-whitespace t) ; highlight trailing whitespace


(setq project-vc-extra-root-markers '(".project")) ; mark dir as project root
(setq make-backup-files nil)            ; Disable Emacs backups
(setq custom-file (make-temp-file "emacs-custom-"))
(setq calendar-week-start-day 6)
(setq undo-limit (* 1024 1024 10))      ; Set undo limit to 10MiB
(setq inhibit-startup-screen t)
(setq visible-bell t)
(setq help-window-select t)             ; Focus new help windows when opened
(setq scroll-conservatively 101)        ; Avoid recentering when scrolling far
(setq mouse-wheel-scroll-amount '(1))
(setq sentence-end-double-space nil)
(setq column-number-mode t)
(setq pop-up-frames nil)
(setq mouse-drag-and-drop-region-cross-program t)
(setq mouse-1-click-follows-link 'double)
(setq set-mark-command-repeat-pop t)
(setq next-error-message-highlight 'keep)
(setq mouse-autoselect-window t)
(setq focus-follows-mouse t)
(setq show-paren-when-point-inside-paren nil)
(setq show-paren-style 'mixed)
(setq split-height-threshold 80)
(setq split-width-threshold 80)
(setq split-window-preferred-function 'ewhd-split-window-sensibly)
(setq enable-recursive-minibuffers t)
(setq minibuffer-depth-indicate-mode t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(show-paren-mode 1)
(delete-selection-mode 1)               ; Replace region when inserting text
(recentf-mode 1)
(global-hl-line-mode 1)                 ; highlight current line
(savehist-mode 1)                       ; Persist history over Emacs restarts.
                                        ; Required by Vertico

;; Do not treat < or > as delimiters in text modes
(defun ewhd-text-mode-syntax-setup ()
  "Remove < and > as paired delimiters in text modes."
  (modify-syntax-entry ?< "." (syntax-table))
  (modify-syntax-entry ?> "." (syntax-table)))

(add-hook 'text-mode-hook 'ewhd-text-mode-syntax-setup)


;;; Revert Buffers:
;; Don't revert Buffer Menu (makes it unusable)
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)


;;; Indentation and EOL behavior:
;; Only display line numbers on wide windows
(defun ewhd-programming-text-behavior ()
  "Configure buffer for programming text editing."
  (setq truncate-lines t)               ; Don't wrap
  ;; (display-line-numbers-mode 1)      ; Show line numbers
  (setq display-line-numbers-type 'relative)
  (display-fill-column-indicator-mode 1)  ; Show 80-char guide
  (setq show-trailing-whitespace t)       ; Highlight trailing spaces
  (setq indent-tabs-mode nil)             ; Use spaces
  (display-fill-column-indicator-mode 1))  ; Visually indicate column width

(defvar ewhd-line-numbers-min-width 90
  "Minimum window width to show line numbers.")

(defun ewhd-smart-display-line-numbers ()
  "Toggle line numbers based on window width."
  (display-line-numbers-mode
   (if (>= (window-width) ewhd-line-numbers-min-width) 1 -1)))

;; Soft wrap prose
(add-hook 'text-mode-hook 'visual-line-mode)

;; Apply custom settings for programming modes
(add-hook 'prog-mode-hook 'ewhd-programming-text-behavior)

;; Apply ewhd-smart-display-line-numbers to programming modes
(add-hook 'prog-mode-hook 'ewhd-smart-display-line-numbers)

;; Apply ewhd-smart-display-line-numbers to programming modes when window size
;; changes
(add-hook 'window-configuration-change-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode)
              (ewhd-smart-display-line-numbers))))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		            term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
                help-mode-hook
                org-agenda-mode-hook
		            chart-mode
                ibuffer-mode-hook
                dired-mode-hook
		            ))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Hard wrap (auto-fill) only code comments
(add-hook 'prog-mode-hook
          (lambda ()
            (setq comment-auto-fill-only-comments t)
            (auto-fill-mode 1)))

;; Lisp modes: 2-space indentation, no tabs
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 2)))

;; Python: 4-space indentation, no tabs (PEP 8)
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq python-indent-offset 4)))



;;; Set Font
(defun font-available-p (font-name)
  "Check if a font is available.
Argument FONT-NAME is the name of a font."
  (find-font (font-spec :name font-name)))

(when (window-system)
  (cond
   ;; ((font-available-p "Iosevka Nerd Font Mono")
   ;;  (set-frame-font "Iosevka Nerd Font Mono-12.5"))
   ((font-available-p "JetBrainsMono Nerd Font")
    (set-frame-font "JetBrainsMono Nerd Font-11"))
   ((font-available-p "Noto Sans Mono")
    (set-frame-font "Noto Sans Mono-11"))
   ))


;;; Keybindings
(use-package emacs
  :ensure nil
  :bind (("C-v"         . yank)
         ("C-:"         . comment-region)
         ("C-h n"       . nil)
         ("C-h C-n"     . nil)
         ("M-z"         . zap-up-to-char)
         ("s-z"         . zap-up-to-char)
         ("<mouse-3>"   . mouse-major-mode-menu)
         ("<C-mouse-3>" . mouse-popup-menubar)
         ("C-_"         . text-scale-decrease)
         ("C-+"         . text-scale-increase)
         ("C-x C-d"     . dired)        ; replace list-directory with dired
         ("C-x K"       . kill-current-buffer)
         ("C-x C-b"     . consult-buffer) ; replaces list-buffers
         ("M-V"         . scroll-other-window-down)
         ("C-x M-b"     . view-buffer-other-window)
         ("C-x M-f"     . find-file-other-window)
         ("C-S-o"       . other-window)
         ("C-x C-r"     . recentf-open-files)
         ("M-SPC"       . execute-extended-command)
         ("C--"         . window-swap-states)
         ;; Custom Functions:
         ("C-0"         . ewhd-switch-to-last-buffer)
         ;; ("C-c C-y"     . ewhd-yank-increment)
         ("M-c"         . xah-toggle-letter-case)
         ("C-c C-="     . ewhd-increment-number-decimal)
         ("C-c C--"     . ewhd-decrement-number-decimal)
         ))


;;; Manage Secrets
(use-package auth-source
  :ensure nil
  :config
  (auth-source-pass-enable)
  )

;;; ibuffer
(use-package ibuffer
  :ensure nil
  :custom
  (ibuffer-display-summary t)
  (ibuffer-use-other-window nil)
  (ibuffer-show-empty-filter-groups t)
  :hook
  ((ibuffer . ibuffer-auto-mode)
   (ibuffer-mode . (lambda ()
                     (ibuffer-switch-to-saved-filter-groups "Main")))
   (ibuffer-mode . (lambda () (visual-line-mode -1)))
   (ibuffer-mode . (lambda () (toggle-truncate-lines 1)))
   )
  :bind (:map ibuffer-mode-map
              ("{" . ibuffer-backwards-next-marked)
              ("}" . ibuffer-forward-next-marked)
              ("[" . ibuffer-backward-filter-group)
              ("]" . ibuffer-forward-filter-group)
              ("$" . ibuffer-toggle-filter-group)
              ("<double-mouse-1>" . ibuffer-visit-buffer)
              ("M-<double-mouse-1>" . ibuffer-visit-buffer-other-window)
              )
  :config
  (defun ewhd-generate-project-filters ()
    "Generate ibuffer groups for each open project."
    (let (groups)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (let ((proj (project-current)))
            (when proj
              (let* ((root (cdr proj))
                     (label (format "Project: %s" root)))
                (push (list label (cons 'filename (buffer-file-name buf)))
                      groups))))))
      groups))


  (setq ibuffer-saved-filter-groups
        '(("Main"
           ("unsaved" (and (modified) (not (or (starred-name) (mode . magit-mode) (mode . magit-process-mode))))) ; All unsaved buffers
           ("dired"   (mode          . dired-mode))          ; Filter by mode
           ("magit"   (or
                       (mode          . magit-mode)
                       (mode          . magit-process-mode))) ; Filter by mode
           ("org/md"    (or
                         (name . "^\\*Org Agenda\\*$")
                         (mode . org-mode)
                         (and (mode . markdown-mode)
                              (not (starred-name)))))
           ("config (probably)"     (and
                                     (or
                                      (filename      . ".org")
                                      (filename      . ".yaml")
                                      (filename      . ".yml")
                                      (filename      . ".toml")
                                      (filename      . ".conf"))
                                     (not (or
                                           (mode          . magit-mode)
                                           (mode          . magit-process-mode))))) ; By filename
           ("scratch"   (name          . "^\\*scratch\\*$")) ; By regexp
           ;; ("Scheme"  (directory     . "~/scheme*"))   ; By directory
           ("stars"   (starred-name))   ; Group *starred*
           )
          ("testList"
           ("projects" ,@(ewhd-generate-project-filters)) ;; Programmatically generated filters
           ))))

;;;; dired config
;; check out http://xahlee.info/emacs/emacs/emacs_dired_tips.html

(use-package dired
  :ensure nil                           ; dired is built-in, no need to install
  :commands (dired)
  :hook (dired-mode-hook . dired-hide-details-mode)
  :config
  ;; Unbind the default C-t binding in Dired
  (define-key dired-mode-map (kbd "C-t") nil)
  
  ;; Assign C-t as a prefix in Dired mode
  (define-key dired-mode-map (kbd "C-t") 'ctl-t-map)
  
  ;; General Dired settings
  (setq dired-kill-when-opening-new-dired-buffer nil
        dired-recursive-deletes 'always ; 'always means no asking
        dired-recursive-copies 'top     ; 'top means ask every time
        delete-by-moving-to-trash t     ; move to trash instead of deleting
        dired-listing-switches
        "-aGFhlv --group-directories-first --time-style=long-iso"
					; list dirs first
        dired-dwim-target t             ; suggest target dir on split pane
        dired-free-space nil            ; Emacs 29.1
        dired-mouse-drag-files t        ; Emacs 29.1
        )

  ;; Load dired-x and configure it
  (require 'dired-x)
  (put 'dired-find-alternate-file 'disabled nil)
					; enable dired-find-alternate-file (set
					; to a)

  ;; Start in dired-omit-mode
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  
  ;; Keybindings for Dired mode
  (define-key dired-mode-map (kbd "h") 'dired-omit-mode)
  (define-key dired-mode-map (kbd "I") 'dired-hide-details-mode)
  (define-key dired-mode-map (kbd ",") #'dired-prev-dirline)
  (define-key dired-mode-map (kbd ".") #'dired-next-dirline)
  (define-key dired-mode-map (kbd "<") #'dired-prev-subdir)
  (define-key dired-mode-map (kbd ">") #'dired-next-subdir)
  (define-key dired-mode-map (kbd "Z") 'dired-do-compress-to)
  (define-key dired-mode-map (kbd "c") 'diredp-hide-subdir-nomove)
  ;; (define-key dired-mode-map (kbd "TAB") 'diredp-hide-subdir-nomove)


  ;; Set files to be hidden in dired-omit-mode
  (setq dired-omit-files
        (rx (or (seq bol (? ".") "#")         ; emacs autosave files
                (seq bol "." (not (any "."))) ; dot-files
                (seq "~" eol)                 ; backup-files
                (seq bol "CVS" eol)           ; CVS dirs
		(seq bol (or "." "..") eol)   ; Include . and ..
                ))))

(use-package wdired
  :ensure nil
  :after dired
  :commands (wdired-change-to-wdired-mode) ; lazy-load
  :config
  (setq wdired-allow-to-change-permissions t) ; edit the permission bits directly
  (setq wdired-create-parent-directories t)
  (setq wdired-allow-to-redirect-links t) ; default=t change the symlinks in
					; Editable mode
  (setq wdired-use-interactive-rename t) ; prompt for every filename change you
					; have made you when you commit the
					; changes with C-c C-c
  (setq wdired-confirm-overwrite t) ; asks if you want to overwrite files if
					; your altered filenames conflict with
					; existing files
  ;; (define-key wdired-mode-map (kbd "C-c C-c") 'wdired-abort) ; Example keybinding
  )


;;;; Window movement/shifting settings

;; previous window
(defun ewhd-other-window-backward ()
  "Select the previous window."
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-x O") 'other-window-backward)

;; winner-mode
(setq winner-dont-bind-my-keys t)
(winner-mode 1)

(global-set-key (kbd "C-c <home>") 'winner-undo)
(global-set-key (kbd "C-M-[") 'winner-undo)
(global-set-key (kbd "C-c <end>") 'winner-redo)
(global-set-key (kbd "C-M-]") 'winner-redo)

;; windmove
;; https://www.emacswiki.org/emacs/WindMove
(windmove-mode 1)

(defun ignore-error-wrapper (fn)
  "Return a new function from function FN that ignores errors.
The function wraps a function with the `ignore-errors` macro."
  (lambda ()
    (interactive)
    (ignore-errors
      (funcall fn))))

;; change windows
(global-set-key (kbd "C-c <left>")  (ignore-error-wrapper 'windmove-left))
(global-set-key (kbd "C-c <right>") (ignore-error-wrapper 'windmove-right))
(global-set-key (kbd "C-c <up>")    (ignore-error-wrapper 'windmove-up))
(global-set-key (kbd "C-c <down>")  (ignore-error-wrapper 'windmove-down))

;; override org-mode keybings
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c <left>")  (ignore-error-wrapper 'windmove-left))
  (define-key org-mode-map (kbd "C-c <right>") (ignore-error-wrapper 'windmove-right))
  (define-key org-mode-map (kbd "C-c <up>")    (ignore-error-wrapper 'windmove-up))
  (define-key org-mode-map (kbd "C-c <down>")  (ignore-error-wrapper 'windmove-down)))

;; swap window positions
(global-set-key (kbd "C-c S-<left>")  (ignore-error-wrapper 'windmove-swap-states-left))
(global-set-key (kbd "C-c S-<right>") (ignore-error-wrapper 'windmove-swap-states-right))
(global-set-key (kbd "C-c S-<up>")    (ignore-error-wrapper 'windmove-swap-states-up))
(global-set-key (kbd "C-c S-<down>")  (ignore-error-wrapper 'windmove-swap-states-down))


(provide 'ewhd-emacs-settings)

;;; Other
(use-package which-key
  ;; As of Emacs 30.1, which-key is part of Emacs core
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-key-order)
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (setq which-key-idle-delay 10)
  (setq which-key-idle-secondary-delay 0.05)
  :bind (("C-h m" . which-key-show-top-level))
  :delight which-key-mode)
;;; ewhd-emacs-settings.el ends here
