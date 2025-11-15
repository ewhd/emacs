;; ewhd emacs settings -*- lexical-binding: t; -*-


;;;; Appearance and Behavior

;; General:
(setq inhibit-startup-screen t
      custom-file (make-temp-file "emacs-custom-")
					; save all interactive customizations to
					; a temp file (permanent customizations
					; should be coded)
      visible-bell t
      help-window-select t              ; Focus new help windows when opened
      scroll-conservatively 101         ; Avoid recentering when scrolling far
      scroll-margin 2                   ; Add a margin when scrolling vertically
      mouse-wheel-scroll-amount '(1)
      sentence-end-double-space nil
      column-number-mode t
      pop-up-frames nil
      mouse-drag-and-drop-region-cross-program t
      mouse-1-click-follows-link 'double
      make-backup-files nil             ; Disable Emacs backups
      fill-column 80
      calendar-week-start-day 6
      set-mark-command-repeat-pop t
      )

(global-visual-line-mode 1)
(global-hl-line-mode 1)                 ; highlight current line
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(delete-selection-mode 1)               ; Replace region when inserting text

;; Desktop
(setq  desktop-dirname "~/.cache"        ; set /before/ enabling desktop mode
       desktop-buffers-not-to-save '("*Messages*"
				     "*scratch*"
				     "*Help*"
				     "*info*"
				     "*compilation*")
       desktop-path (list desktop-dirname)
					; ensures Emacs uses this path for
					; desktop files -- emacs won't seem to
					; look in desktop-dirname without this
					; line
       desktop-auto-save-timeout 10 
       desktop-save t                    ; always save
       )
(desktop-save-mode 1)

;; Make #+... tags look nicer
(setq-default prettify-symbols-alist
	      (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
		      '(("#+begin_src" . ?)
			("#+end_src" . ?)
			;; ("#+begin_src" . "λ")
			("#+begin_example" . ?)
			("#+end_example"   . ?)
			("#+begin_quote"   . ?)
			("#+end_quote"     . ?)
			("#+begin_comment" . ?)
			("#+end_comment"   . ?)
			("#+header:"       . ?)
			;; ("#+name:"         . ?﮸)
			("#+results:"      . ?)
			("#+call:"         . ?)
			(":properties:"    . ?)
			(":logbook:"       . ?)
			)))
(add-hook 'org-mode-hook 'prettify-symbols-mode)



;;; Revert Buffer Behavior:
;; - Automatically revert files which have been changed on disk, unless the
;; - buffer contains unsaved changes
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
					; Don't revert Buffer Menu (makes it
					; unusable)
					; https://github.com/syl20bnr/spacemacs/issues/7661#issuecomment-258481672
					; https://www.reddit.com/r/emacs/comments/t01efg/comment/iat14ob/?utm_source=share&utm_medium=web2x&context=3
;; (require 'autorevert)
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)

;; Line Numbers:
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative) ; sets the default line number type
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
                help-mode-hook
                org-agenda-mode-hook
		chart-mode
		))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Highlight Parentheses:
(show-paren-mode 1)
(setq show-paren-when-point-inside-paren nil
      show-paren-style 'mixed)

;; Parentheses Pairing Behavior:

(use-package electric
  :ensure nil
  :init
  (defun my/setup-org-electric-pair ()
    "Disable pairing for < and > only in Org mode."
    (setq-local electric-pair-inhibit-predicate
                (lambda (char)
                  (if (eq major-mode 'org-mode)
                      (or (char-equal char ?<) (char-equal char ?>))
                    (electric-pair-default-inhibit char)))))

  :config
  (electric-pair-mode 1)                ; Enable electric-pair-mode globally
  (setq electric-pair-preserve-balance t
	electric-pair-delete-adjacent-pairs t)

  ;; Apply the custom inhibit predicate only in Org mode
  (add-hook 'org-mode-hook 'my/setup-org-electric-pair)
  )




;; Window Splitting Behavior:
(setq split-height-threshold 80
      split-width-threshold 80)

(defun my-split-window-sensibly (&optional window)
  "replacement `split-window-sensibly' function which prefers vertical splits"
  (interactive)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             (with-selected-window window
               (split-window-below))))))

(setq split-window-preferred-function 'my-split-window-sensibly)

;; General Key Remapping:
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "M-v") 'kill-ring-save)
;; (global-set-key (kbd "C-S-v") 'scroll-up-command)
(global-set-key (kbd "C-:") 'comment-region)
(global-set-key (kbd "C-h n") nil)
(global-set-key (kbd "C-h C-n") nil)
;; (global-set-key (kbd "C-x 4-s") 'window-swap-states) ; not working
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "<mouse-3>") 'mouse-major-mode-menu)
(global-set-key (kbd "<C-mouse-3>") 'mouse-popup-menubar)
(global-unset-key (kbd "C-_"))
(global-set-key (kbd "C-_") 'text-scale-decrease)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-x C-d") 'dired) ; replace keybinding for list-directory
					; with dired
(global-set-key (kbd "C-x K") 'kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'consult-buffer)
					; replace keybinding for list-buffers
					; with consult-buffer
(global-set-key (kbd "M-V") 'scroll-other-window-down)
(global-set-key (kbd "C-x M-b") 'view-buffer-other-window)
(global-set-key (kbd "C-x M-f") 'find-file-other-window)


;;;; Tab Management
;; Unbind the default transpose-characters command
(global-unset-key (kbd "C-t"))

;; Define C-t as a prefix key
(define-prefix-command 'ctl-t-map)
(global-set-key (kbd "C-t") 'ctl-t-map)

;; dired has some C-t- prefixed commands for images/thumbnails, but I want these
;; tab controls to override them, which is done in the dired config below

;; new tab control keybindings
(define-key ctl-t-map (kbd "n")       'tab-next)
(define-key ctl-t-map (kbd "p")       'tab-previous)
(define-key ctl-t-map (kbd "u")       'tab-undo)
(define-key ctl-t-map (kbd "N")       'tab-new)
(define-key ctl-t-map (kbd "x")       'tab-close)
(define-key ctl-t-map (kbd "r")       'tab-reopen)
(define-key ctl-t-map (kbd "m")       'tab-move)
(define-key ctl-t-map (kbd "d")       'tab-duplicate)
(define-key ctl-t-map (kbd "<right>") 'tab-bar-switch-to-next-tab)
(define-key ctl-t-map (kbd "<left>")  'tab-bar-switch-to-prev-tab)
(define-key ctl-t-map (kbd "RET")     'tab-bar-select-tab-by-name)
(define-key ctl-t-map (kbd "b")       'tab-bar-history-back)
(define-key ctl-t-map (kbd "f")       'tab-bar-history-forward)


;;;; dired config
;; check out http://xahlee.info/emacs/emacs/emacs_dired_tips.html

(use-package dired
  :ensure nil                           ; dired is built-in, no need to install
  :commands (dired)
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

(global-set-key (kbd "C-c <end>") 'winner-redo)
(global-set-key (kbd "C-c <home>") 'winner-undo)

;; windmove
;; https://www.emacswiki.org/emacs/WindMove
(windmove-mode 1)

(defun ignore-error-wrapper (fn)
  "Return a new function that ignores errors.
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


;;;; Extra Functions
;; Toggle Letter Case

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

(global-set-key (kbd "M-c") 'xah-toggle-letter-case)


;; Increment Number
; https://www.emacswiki.org/emacs/IncrementNumber

(defun ewhd-increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
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
  (interactive "p*")
  (ewhd-increment-number-decimal (if arg (- arg) -1)))

(global-set-key (kbd "C-c C-=") 'ewhd-increment-number-decimal)
(global-set-key (kbd "C-c C--") 'ewhd-decrement-number-decimal)

(defun ewhd-increment-string (string)
  (interactive "*")
  (setq start (string-match "\\([0-9]+\\)" string))
  (setq end (match-end 0))
  (setq number (string-to-number (substring string start end)))
  (setq new-num-string (number-to-string (+ 1 number)))
  (concat (substring string 0 start) new-num-string (substring string end)))

(defun ewhd-yank-increment ()
  "Yank text, incrementing the first integer found in it."
  (interactive "*")
  (setq new-text (ewhd-increment-string (current-kill 0)))
  (insert-for-yank new-text)
  (kill-new new-text t))

;; (global-set-key (kbd "C-c C-y") 'ewhd-yank-increment)

;;; Server shutdown
;; I use this to safely shutdown the server, mostly when I want to restart it
;; after making config changes -- probably unnecessary if I use systemd to start
;; my daemon, but currently I don't https://emacs.stackexchange.com/a/55799
(defun ewhd-server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))


;; swap to last buffer
(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(global-set-key (kbd "C-0") 'switch-to-last-buffer)


