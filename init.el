;; ewhd emacs configuration -*- lexical-binding: t; -*-

;; save all interactive customizations to a temp file
;; permanent customizations should be coded
(setq custom-file (make-temp-file "emacs-custom-"))

;;;; Elpaca configuration

;; From https://github.com/progfolio/elpaca/blob/master/doc/init.el

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))


;; elpaca installs packages asyncronously after init.el is read, so we want to block (elpaca-wait) until use-package is ready as the rest of init.el will want to use use-package.
;;https://gitlab.com/jdm204/dotfiles/-/blob/master/config.org
(elpaca elpaca-use-package ; Install use-package support
  (elpaca-use-package-mode) ; Enable use-package :ensure support for Elpaca.
  )
(elpaca-wait) ; Block until current queue processed.
(setq use-package-always-ensure t)

;; End of Elpaca



;;;; Appearance and Behavior

;; General:
(setq inhibit-startup-screen t
      visible-bell t
      help-window-select t        ; Focus new help windows when opened
      scroll-conservatively 101   ; Avoid recentering when scrolling far
      scroll-margin 2             ; Add a margin when scrolling vertically
      mouse-wheel-scroll-amount '(1)
      sentence-end-double-space nil
      column-number-mode t)

(global-visual-line-mode 1)
(global-hl-line-mode 1)       ; highlight current line
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(delete-selection-mode 1)     ; Replace region when inserting text
(desktop-save-mode 1)

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
(setq display-line-numbers 'relative)
(global-display-line-numbers-mode 1)
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
(global-set-key (kbd "C-z") 'undo-tree-undo)
;; (global-set-key (kbd "C-S-v") 'scroll-up-command)
(global-set-key (kbd "C-:") 'eval-expression)
(global-set-key (kbd "C-h n") nil)
(global-set-key (kbd "C-h C-n") nil)
;(global-set-key (kbd "C-x 4-s") 'window-swap-states) ; not working
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "<mouse-3>") 'mouse-major-mode-menu)
(global-set-key (kbd "<C-mouse-3>") 'mouse-popup-menubar)

;; Delight enables you to easily customise how major and minor modes appear in the ModeLine.
;; As per use-package README: delight is invoked with the :delight keyword, which is passed a minor mode symbol, a replacement string or quoted mode-line data (in which case the minor mode symbol is guessed to be the package name with “-mode” appended at the end), both of these, or several lists of both. If no arguments are provided, the default mode name is hidden completely.
(use-package delight
;  :delight (org-indent-mode) ; This belongs in org section
  )
;(elpaca-wait) ; I'm unsure if this is needed

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium t)
  )

(use-package which-key
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

(use-package multiple-cursors
  :ensure   t
  :bind (("H-SPC" . set-rectangular-region-anchor)
         ("C-M-SPC" . set-rectangular-region-anchor)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-c C-SPC" . mc/edit-lines)
	   ("C-S-<mouse-1>" . 'mc/add-cursor-on-click)
           ))



;; (use-package undo-tree
;;   :delight undo-tree-mode
;;   :config
;;   (progn
;;     (global-undo-tree-mode t)
;;     (setq undo-tree-visualizer-timestamps t)
;;     (setq undo-tree-visualizer-diff t))
;;  )

