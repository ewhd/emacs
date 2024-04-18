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
      dired-kill-when-opening-new-dired-buffer t   ; prevent dired from creating new buffers for every dir visited
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
(global-set-key (kbd "C-:") 'eval-expression)
(global-set-key (kbd "C-h n") nil)
(global-set-key (kbd "C-h C-n") nil)
;(global-set-key (kbd "C-x 4-s") 'window-swap-states) ; not working
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "<mouse-3>") 'mouse-major-mode-menu)
(global-set-key (kbd "<C-mouse-3>") 'mouse-popup-menubar)
