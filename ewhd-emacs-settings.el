;; ewhd emacs settings -*- lexical-binding: t; -*-


;;;; Appearance and Behavior

;; General:
(setq inhibit-startup-screen t
      custom-file (make-temp-file "emacs-custom-") ; save all interactive customizations to a temp file (permanent customizations should be coded)
      visible-bell t
      help-window-select t        ; Focus new help windows when opened
      scroll-conservatively 101   ; Avoid recentering when scrolling far
      scroll-margin 2             ; Add a margin when scrolling vertically
      mouse-wheel-scroll-amount '(1)
      sentence-end-double-space nil
      column-number-mode t
      desktop-dirname "/tmp/"
      )

(global-visual-line-mode 1)
(global-hl-line-mode 1)       ; highlight current line
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(delete-selection-mode 1)     ; Replace region when inserting text
(desktop-save-mode -1)

;; Revert Buffer Behavior:
;; - Automatically revert files which have been changed on disk, unless the buffer contains unsaved changes
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
;; Don't revert Buffer Menu (makes it unusable)
;; https://github.com/syl20bnr/spacemacs/issues/7661#issuecomment-258481672
;; https://www.reddit.com/r/emacs/comments/t01efg/comment/iat14ob/?utm_source=share&utm_medium=web2x&context=3
;(require 'autorevert)
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)

;; Line Numbers:
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative) ;sets the default line number type
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
(electric-pair-mode t)
(setq electric-pair-preserve-balance t
      electric-pair-delete-adjacent-pairs t)

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
;(global-set-key (kbd "C-x 4-s") 'window-swap-states) ; not working
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "<mouse-3>") 'mouse-major-mode-menu)
(global-set-key (kbd "<C-mouse-3>") 'mouse-popup-menubar)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-x C-d") 'dired) ;; replace keybinding for list-directory with dired
(global-set-key (kbd "C-x K") 'kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'consult-buffer) ;; replace keybinding for list-buffers with consult-buffer


;;;; dired config
;; check out http://xahlee.info/emacs/emacs/emacs_dired_tips.html

;; prevent dired from creating new buffers for every dir visited
(setq dired-kill-when-opening-new-dired-buffer nil
      dired-listing-switches "-alh"
      dired-recursive-deletes 'always  ;; 'always means no asking
      dired-recursive-copies 'top  ;; 'top means ask every time
      dired-dwim-target t  ;; automatically suggest the target dir on the split pane when you copy (C) or move (R) a file
      )

;; ;; Make dired open in the same window when using RET or ^
;; ;; http://xahlee.info/emacs/emacs/emacs_dired_tips.html
;; (put 'dired-find-alternate-file 'disabled nil) ; disables warning
;; (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
;; (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory


;; load dired-x
(with-eval-after-load 'dired
  (require 'dired-x)
  (put 'dired-find-alternate-file 'disabled nil) ;; enable dired-find-alternate-file (set to a)
  ;; start in dired-omit-mode
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (define-key dired-mode-map (kbd "h") 'dired-omit-mode)
  ;; (add-hook 'dired-mode-hook 'dired-hide-details-mode) ;; this might be needed to start in dired-hide-details-mode? Or dired+ might take care of that
  (define-key dired-mode-map (kbd "I") 'dired-hide-details-mode)
  (define-key dired-mode-map (kbd ",") #'dired-prev-dirline)
  (define-key dired-mode-map (kbd ".") #'dired-next-dirline)
  (define-key dired-mode-map (kbd "<") #'dired-prev-subdir)
  (define-key dired-mode-map (kbd ">") #'dired-next-subdir)
  (define-key dired-mode-map (kbd "Z") 'dired-do-compress-to)
  (define-key dired-mode-map (kbd "c") 'diredp-hide-subdir-nomove)
  (define-key dired-mode-map (kbd "TAB") 'diredp-hide-subdir-nomove)
  )


;; set files to be hidden in dired-omit-mode:
(setq dired-omit-files
    (rx (or (seq bol (? ".") "#")     ;; emacs autosave files
        (seq bol "." (not (any "."))) ;; dot-files
        (seq "~" eol)                 ;; backup-files
        (seq bol "CVS" eol)           ;; CVS dirs
        )))


;;;; Window movement/shifting settings

;; previous window
(defun other-window-backward ()
  "Select the previous window."
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-x O") 'other-window-backward)

;; winner-mode
(winner-mode 1)

(global-set-key (kbd "C-c S-<end>") 'winner-redo)
(global-set-key (kbd "C-c S-<home>") 'winner-undo)

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


(global-set-key (kbd "C-c S-<left>")  (ignore-error-wrapper 'windmove-left))
(global-set-key (kbd "C-c S-<right>") (ignore-error-wrapper 'windmove-right))
(global-set-key (kbd "C-c S-<up>")    (ignore-error-wrapper 'windmove-up))
(global-set-key (kbd "C-c S-<down>")  (ignore-error-wrapper 'windmove-down))


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

