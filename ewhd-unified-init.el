;; ewhd emacs org mode related packages -*- lexical-binding: t; -*-

;; Great example:
;; https://github.com/andreyorst/dotfiles/blob/master/.config/emacs/init.el
;; Demonstrates use-package for non-package features

(use-package use-package
  :no-require
  :custom
  (use-package-enable-imenu-support t))



(use-package early-init
  :no-require
  :unless (featurep 'early-init)
  :config
  (load-file (locate-user-emacs-file "early-init.el")))



(use-package ewhd-emacs-general
  ;; Define identity, determine what kind of system it is on, etc.
  :load-path nil  ; Set this if ewhd-emacs-general.el is not in the default load path
  :config
  (load (locate-user-emacs-file "ewhd-emacs-general.el") nil :nomessage))



(use-package elpaca-init
  ;; Load the settings needed to make elpaca work
  ;; !!! This is required before loading any packages !!!
  :load-path nil  ; Set this if elpaca-init.el is not in the default load path
  :config
  (load (locate-user-emacs-file "elpaca-init.el") nil :nomessage))



;; (use-package local-config
;;   :no-require
;;   :preface)



(use-package savehist
  :hook (after-init . savehist-mode))



(use-package functions
  :no-require
  :config
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  
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

  (defun ewhd-split-window-sensibly (&optional window)
    "replacement `split-window-sensibly' function which prefers vertical splits"
    (interactive)
    (let ((window (or window (selected-window))))
      (or (and (window-splittable-p window t)
               (with-selected-window window
                 (split-window-right)))
          (and (window-splittable-p window)
               (with-selected-window window
                 (split-window-below))))))

  (setq split-height-threshold 80
        split-width-threshold 80
	split-window-preferred-function 'ewhd-split-window-sensibly)

  (defun ewhd-swap-other-window ()
    "Select the previous window."
    (interactive)
    (other-window -1))

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

  ;; Server shutdown
  ;; I use this to safely shutdown the server, mostly when I want to restart it after making config changes -- probably unnecessary if I use systemd to start my daemon, but currently I don't
  ;; https://emacs.stackexchange.com/a/55799
  (defun ewhd-server-shutdown ()
    "Save buffers, Quit, and Shutdown (kill) server"
    (interactive)
    (save-some-buffers)
    (kill-emacs))


  ;; swap to last buffer
  (defun ewhd-switch-to-last-buffer ()
    (interactive)
    (switch-to-buffer nil))
  
  :bind
  ("C-x O"   . ewhd-swap-other-window)
  ("M-c"     . xah-toggle-letter-case)
  ("C-c C-=" . ewhd-increment-number-decimal)
  ("C-c C--" . ewhd-decrement-number-decimal)
  ;; ("C-c C-y" . ewhd-yank-increment)
  ("C-0"     . ewhd-switch-to-last-buffer)
  )



;; (use-package defaults
;;   :no-require
;;   :preface
;;   (setq-default))



(use-package files
  :ensure nil
  :custom
  (make-backup-files nil)
  (global-auto-revert-non-file-buffers t)
  :config
  (add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode) ;; Don't revert Buffer Menu (makes it unusable)
  (global-auto-revert-mode 1) ;; Automatically revert files changed on disk unless there are unsaved changes
  )



(use-package desktop
  :ensure nil
  :config
  (add-to-list 'desktop-globals-to-save 'dirvish--history)
  (setq desktop-dirname "~/.cache" ; set the directory /before/ enabling desktop mode
	;; desktop-dirname "/var/tmp/"
	desktop-buffers-not-to-save '("*Messages*" "*scratch*" "*Help*" "*info*" "*compilation*")
	desktop-path (list desktop-dirname) ; ensures Emacs uses this path for desktop files -- emacs won't seem to look in desktop-dirname without this line
	desktop-auto-save-timeout 10
	desktop-save t ; always save
	)
  )
;; Check out: https://www.reddit.com/r/emacs/comments/1cl1msi/comment/l2s8z8o/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
;; Has some settings about making Daemon and desktop mode play nice
;; Other posts in same thread also look good



(use-package emacs
  :ensure nil
  :init
  ;; Initial setup
  (setq inhibit-startup-screen t
        custom-file (make-temp-file "emacs-custom-")
        visible-bell t
        help-window-select t
        scroll-conservatively 101
        scroll-margin 2
        mouse-wheel-scroll-amount '(1)
        sentence-end-double-space nil
        column-number-mode t
        pop-up-frames nil
        mouse-drag-and-drop-region-cross-program t
        mouse-1-click-follows-link 'double
        make-backup-files nil
	show-paren-when-point-inside-paren nil
        show-paren-style 'mixed
	electric-pair-preserve-balance t
        electric-pair-delete-adjacent-pairs t
	custom-file (make-temp-file "emacs-custom-") ; save all interactive customizations to a temp file (permanent customizations should be coded)
	read-extended-command-predicate
        #'command-completion-default-include-p  ;; Emacs 28: Hide commands in M-x which do not work in the current mode - Vertico commands are hidden in normal buffers
	enable-recursive-minibuffers t  ;; Enable recursive minibuffers
	)

  ;; UI configurations
  (global-visual-line-mode 1)  ; Activates visual line mode globally
  (global-hl-line-mode 1)      ; Highlights the current line
  (tool-bar-mode -1)           ; Disables the toolbar
  (scroll-bar-mode -1)         ; Disables the scroll bar
  (menu-bar-mode -1)           ; Disables the menu bar
  (tooltip-mode -1)            ; Disables tooltips
  (delete-selection-mode 1)    ; Replaces selected text on input
  (column-number-mode t)       ; Shows column numbers in mode line
  (show-paren-mode 1)
  (electric-pair-mode t)

  :custom  ;; Customizable variables
  (scroll-margin 2)
  (visible-bell t)
  :config
  
  :bind
  ;; General Key Remapping
  (("C-v" . yank)
   ("M-v" . kill-ring-save)
   ("C-:" . comment-region)
   ("C-h n" . nil)
   ("C-h C-n" . nil)
   ("M-z" . zap-up-to-char)
   ("<mouse-3>" . mouse-major-mode-menu)
   ("<C-mouse-3>" . mouse-popup-menubar)
   ("C-_" . text-scale-decrease)
   ("C--" . text-scale-decrease)
   ("C-+" . text-scale-increase)
   ("C-x C-d" . dired)
   ("C-x K" . kill-this-buffer)
   ("C-x C-b" . consult-buffer)
   )

  ;; Tab Management Keybindings
  (("C-t" . ctl-t-map)
   :map ctl-t-map
   (("n" . tab-next)
    ("p" . tab-previous)
    ("u" . tab-undo)
    ("N" . tab-new)
    ("x" . tab-close)
    ("r" . tab-reopen)
    ("m" . tab-move)
    ("d" . tab-duplicate)
    ("<right>" . tab-bar-switch-to-next-tab)
    ("<left>" . tab-bar-switch-to-prev-tab)
    ("RET" . tab-bar-select-tab-by-name)
    ("b" . tab-bar-history-back)
    ("f" . tab-bar-history-forward)))
  )



(use-package dired
  :ensure nil  ;; dired is built-in, no need to install
  :commands dired
  :init
  ;; General Dired settings
  (setq dired-kill-when-opening-new-dired-buffer nil
        dired-recursive-deletes 'always      ;; 'always means no asking
        dired-recursive-copies 'top          ;; 'top means ask every time
        delete-by-moving-to-trash t          ;; move to trash instead of deleting
        dired-listing-switches
        "-aGFhlv --group-directories-first --time-style=long-iso" ;; list dirs first
        dired-dwim-target t                   ;; suggest target dir on split pane
        dired-free-space nil                  ;; Emacs 29.1
        dired-mouse-drag-files t              ;; Emacs 29.1
        dired-omit-files
        (rx (or (seq bol (? ".") "#")     ;; emacs autosave files
                (seq bol "." (not (any "."))) ;; dot-files
                (seq "~" eol)                 ;; backup-files
                (seq bol "CVS" eol)           ;; CVS dirs
                (seq bol (or "." "..") eol)   ;; Include . and ..
                )))
  :config
  ;; Load dired-x and configure it
  (require 'dired-x)
  (put 'dired-find-alternate-file 'disabled nil) ;; enable dired-find-alternate-file

  ;; Start in dired-omit-mode
  (add-hook 'dired-mode-hook 'dired-omit-mode)

  :bind (:map dired-mode-map
              ("C-t" . nil)                      ;; Unbind the default C-t binding
              ("C-t" . ctl-t-map)                ;; Assign C-t as a prefix
              ("h" . dired-omit-mode)            ;; Toggle omit mode
              ("I" . dired-hide-details-mode)    ;; Hide details
              ("," . dired-prev-dirline)         ;; Move to previous directory line
              ("." . dired-next-dirline)         ;; Move to next directory line
              ("<" . dired-prev-subdir)          ;; Move to previous subdirectory
              (">" . dired-next-subdir)          ;; Move to next subdirectory
              ("Z" . dired-do-compress-to)       ;; Compress files
              ("c" . diredp-hide-subdir-nomove)  ;; Hide subdirectory without moving
	      ))



(use-package wdired
  :ensure nil
  :after dired
  :commands (wdired-change-to-wdired-mode) ; lazy-load
  :config
  (setq wdired-allow-to-change-permissions t) ; edit the permission bits directly
  (setq wdired-create-parent-directories t)
  (setq wdired-allow-to-redirect-links t) ; default=t change the symlinks in Editable mode
  (setq wdired-use-interactive-rename t) ; prompt for every filename change you have made you when you commit the changes with C-c C-c
  (setq wdired-confirm-overwrite t) ; asks if you want to overwrite files if your altered filenames conflict with existing files
  ;; (define-key wdired-mode-map (kbd "C-c C-c") 'wdired-abort) ;; Example keybinding
  )



(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("n" "~/Documents/notes/"          "Notes")
     ("e" "~/.config/emacs/"            "Emacs")
     ("d" "~/Documents/"                "Documents")
     ("D" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")
     ("t" "~/.local/share/Trash/files/" "TrashCan")
     ("h" "~/"                          "Home")))
  :config
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'

  (setq dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index))
	dirvish-attributes '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg)
	delete-by-moving-to-trash t
	dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group --time-style=long-iso --color=always"
	dirvish-default-layout '(0 0 0.5)  ; default (1 0.11 0.55)
	dirvish-layout-recipes '((0 0 0.5) (0 0 0.7) (1 0.11 0.55))
	dirvish-reuse-session 'resume)      ; retain dirvish session and resume it unless called with a directory path

  (add-hook 'dirvish-find-entry-hook
            (lambda (&rest _) (setq-local truncate-lines t)))

  ;; Unbind default C-x d to use as prefix
  (global-unset-key (kbd "C-x d"))
  (define-prefix-command 'ctl-x-d-map)
  
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   ("C-x d" . ctl-x-d-map)  ;; Assign C-x d as a prefix
   ("C-x d d" . dirvish)
   ("C-x d f" . dired)
   ("C-x d s" . dirvish-side)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("-"   . dirvish-history-last)
   ("DEL" . dired-up-directory)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)
   ("M-h" . dired-omit-mode)
   ("M"   . dirvish-move)
   ))



(use-package winner
  :ensure nil
  :bind
  ("C-c <end>"  . winner-redo) 
  ("C-c <home>" . winner-undo)
  :config
  (setq winner-dont-bind-my-keys t)
  (winner-mode 1)
  )



(use-package windmove
  :ensure nil  ;; windmove is built-in, no need to install
  :config
  ;; Enable windmove mode
  (windmove-mode 1)

  ;; Define a wrapper function to ignore errors
  (defun ignore-error-wrapper (fn)
    "Return a new function that ignores errors.
The function wraps a function with the `ignore-errors` macro."
    (lambda ()
      (interactive)
      (ignore-errors
        (funcall fn))))

  ;; Override org-mode keybindings for window movement
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c <left>")  (ignore-error-wrapper 'windmove-left))
    (define-key org-mode-map (kbd "C-c <right>") (ignore-error-wrapper 'windmove-right))
    (define-key org-mode-map (kbd "C-c <up>")    (ignore-error-wrapper 'windmove-up))
    (define-key org-mode-map (kbd "C-c <down>")  (ignore-error-wrapper 'windmove-down)))

  :bind
  ;; Change windows using C-c + arrow keys
  (("C-c <left>"  . (ignore-error-wrapper 'windmove-left))
   ("C-c <right>" . (ignore-error-wrapper 'windmove-right))
   ("C-c <up>"    . (ignore-error-wrapper 'windmove-up))
   ("C-c <down>"  . (ignore-error-wrapper 'windmove-down))
   
   ;; Swap window positions using C-c S + arrow keys
   ("C-c S-<left>"  . (ignore-error-wrapper 'windmove-swap-states-left))
   ("C-c S-<right>" . (ignore-error-wrapper 'windmove-swap-states-right))
   ("C-c S-<up>"    . (ignore-error-wrapper 'windmove-swap-states-up))
   ("C-c S-<down>"  . (ignore-error-wrapper 'windmove-swap-states-down)))
  )



(use-package denote
  :ensure t
  :init
  (require 'denote-org-extras)
  (denote-rename-buffer-mode 1)
  :custom
  (denote-prompts '(title keywords subdirectory))
  (denote-directory (expand-file-name "~/Documents/notes/"))
  (denote-file-type 'org)
  (denote-known-keywords '())
  :bind (("C-c d d" . denote)
	 ("C-c d l" . denote-link)
	 ("C-c d f" . list-denotes)
	 ("C-c d t" . denote-template)
	 ("C-c d d" . denote)
	 )
  :config
  ;; (setq denote-templates `( ;;(test . "test")
  ;; ))
  :hook ((dired-mode . denote-dired-mode))
  )



(use-package denote-explore
  :ensure t
  :custom
  (denote-explore-network-directory "~/Documents/notes/graphs/")  ;; Location of graph files
  (denote-explore-network-filename "denote-network")
  (denote-explore-network-format 'graphviz)  ;; Output format
  (denote-explore-network-graphviz-filetype "svg")
  (denote-explore-network-keywords-ignore '("bib"))  ;; Exlude keywords or regex
  )



(use-package denote-menu
  :ensure t)



(use-package consult-notes
  :ensure t
  :commands (consult-notes
             consult-notes-search-in-all-notes
             ;; if using org-roam 
             ;; consult-notes-org-roam-find-node
             ;; consult-notes-org-roam-find-node-relation
	     )
  :config
  (setq consult-notes-file-dir-sources '(("Documents"  ?d  "~/Documents/CF/")
					 ;; ("Local" ?l "./")
					 )) ;; Set notes dir(s), see below
  ;; Set org-roam integration, denote integration, or org-heading integration e.g.:
  ;; (setq consult-notes-org-headings-files '("~/path/to/file1.org"
  ;; "~/path/to/file2.org"))
  ;; (consult-notes-org-headings-mode)
  (when (locate-library "denote")
    (consult-notes-denote-mode))
  ;; (setq consult-notes-denote-files-function (function denote-directory-text-only-files))  ;; search only for text files in denote dir

  )



(use-package delight
  :ensure t)



(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium t)
  )



(use-package transient
  ;; Ensure Transient is the most recent version (needed for magit):
  :ensure t)



(use-package expand-region
  :ensure t
  :config
  (global-unset-key (kbd "C-\\"))
  :bind (("C-\\" . er/expand-region)
	 ("C-'" . er/mark-outside-quotes)
	 )
  )



(use-package all-the-icons
  ;; must run M-x all-the-icons-install-fonts to install fonts per machine
  :ensure t
  :if (display-graphic-p))



;; Magit:
(use-package magit
  :after transient
  :ensure t
  :bind (("C-x g" . magit))
  :config
  ;; https://emacs.stackexchange.com/questions/60200/magit-remove-local-branches-that-were-merged-into-another-branch/81021#81021
  (defun ewhd-delete-merged-branches ()
    "Remove local branches already merged into another branch"
    (interactive)
    (magit-fetch-all-prune)
    (let* ((default-branch
            (read-string "Default branch: " (magit-get-current-branch)))
           (merged-branches
            (magit-git-lines "branch"
                             "--format" "%(refname:short)"
                             "--merged"
                             default-branch))
           (branches-to-delete
            (remove default-branch merged-branches)))
      (if branches-to-delete
          (if (yes-or-no-p (concat "Delete branches? ["
                                   (mapconcat 'identity branches-to-delete ", ") "]"))
              (magit-branch-delete branches-to-delete))
        (message "Nothing to delete"))))
  
  (transient-append-suffix 'magit-branch "C"
    '("K" "delete all merged" ewhd-delete-merged-branches))  
  )



(use-package transpose-frame
  ;; https://www2.lib.uchicago.edu/keith/emacs/minimacs.html
  :defer t
  :commands
  (transpose-frame
   flip-frame
   flop-frame
   rotate-frame
   rotate-frame-clockwise
   rotate-frame-anticlockwise)
  :bind (("C-x 6" . transpose-frame)))



(use-package activities
  :disabled t
  :init
  (activities-mode)
  (activities-tabs-mode)
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)  ;; Prevent `edebug' default bindings from interfering.
  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b"   . activities-switch-buffer)
   ("C-x C-a g"   . activities-revert)
   ("C-x C-a l"   . activities-list)))



(use-package olivetti
  :hook ((text-mode . (lambda ()
                        (unless (and buffer-file-name
                                     (string-match-p "gtd" (file-name-nondirectory buffer-file-name)))
                          (olivetti-mode 1)))))
  :config
  (add-hook 'olivetti-mode-on-hook (lambda () (olivetti-set-width 80))))



(use-package which-key
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-key-order)
  (setq which-key-show-early-on-C-h t)  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-idle-delay 10)  ;; make sure which-key doesn't show normally but refreshes quickly after it is triggered.
  (setq which-key-idle-secondary-delay 0.05)
  :bind (("C-h m" . which-key-show-top-level))
  :delight which-key-mode)



(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-wrapper))
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
	 ))



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



(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  :bind
  (("C-z"   . undo-fu-only-undo)
   ("C-S-z" . undo-fu-only-redo)
   ))



(use-package vundo
  :commands (vundo)
  :config
  (setq vundo-compact-display t ;; Take less on-screen space.
	vundo-glyph-alist vundo-unicode-symbols) ;; Use prettier Unicode characters

  ;; Better contrasting highlight.
  (custom-set-faces
   '(vundo-node ((t (:foreground "#808080"))))
   '(vundo-stem ((t (:foreground "#808080"))))
   '(vundo-highlight ((t (:foreground "#FFFF00"))))
   )

  :bind (("C-M-u" . vundo)
	 :map vundo-mode-map
	 ("l" . vundo-forward)
	 ("<right>" . vundo-forward)
	 ("h" . vundo-backward)
	 ("<left>" . vundo-backward)
	 ("j" . vundo-next)
	 ("<down>" . vundo-next)
	 ("k" . vundo-previous)
	 ("<up>" . vundo-previous)
	 ("<home>" . vundo-stem-root)
	 ("<end>" . vundo-stem-end)
	 ("q" . vundo-quit)
	 ("C-g" . vundo-quit)
	 ("RET" . vundo-confirm)
	 ))



;;;; Completion Tools:
(use-package vertico
  ;; Vertico sorts history by position, so make sure savehist-mode is one
  :ensure t
  :init
  (vertico-mode))



(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))



(use-package embark
  :ensure t
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none))))
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'
   ))



(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  )



(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h"   . consult-history)
         ("C-c k"   . consult-kmacro)
         ;; ("C-c m" . consult-man)
         ("C-c i"   . consult-info)
         ([remap Info-search] . consult-info)

         ;; C-x bindings in `ctl-x-map'
         ;; ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b"   . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#"     . consult-register-load)
         ("M-'"     . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#"   . consult-register)
         ;; Other custom bindings
         ("M-y"     . consult-yank-pop)                ;; orig. yank-pop

	 ;; M-g bindings in `goto-map'
         ;; ("M-g e" . consult-compile-error)
         ;; ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g"   . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o"   . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m"   . consult-mark)
         ("M-g k"   . consult-global-mark)
         ("M-g i"   . consult-imenu)
         ("M-g I"   . consult-imenu-multi)

	 ;; M-s bindings in `search-map'
         ("M-s d"   . consult-find)                  ;; Alternative: consult-fd
         ("M-s D"   . consult-locate)
         ("M-s g"   . consult-grep)
         ("M-s G"   . consult-git-grep)
         ("M-s r"   . consult-ripgrep)
         ("M-s l"   . consult-line)
         ("M-s L"   . consult-line-multi)
         ("M-s k"   . consult-keep-lines)
         ("M-s u"   . consult-focus-lines)
         ;; Isearch integration
         ("M-s e"   . consult-isearch-history)
         :map isearch-mode-map
         ("M-e"   . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s"   . consult-history)                 ;; orig. next-matching-history-element
         ("M-r"   . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )



(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))



;;;; Citations
;; https://kristofferbalintona.me/posts/202206141852/#advising-citar-org-update-pre-suffix
;; https://amerygration.com/Blog/citation_handling_in_emacs.html
;; https://www.miskatonic.org/2024/01/08/org-citations-basic/
;; https://lucidmanager.org/productivity/bibliographic-notes-in-emacs-with-citar-denote/
;; https://www.bibtex.org/Format/
;; https://www.bibtex.com/e/entry-types/

(use-package citar
  :after org
  :ensure t
  :custom
  (org-cite-global-bibliography '("~/Documents/notes/bib/references.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))



(use-package citar-embark
  :after citar embark
  :ensure t
  :custom
  (citar-at-point-function 'embark-act)
  :config
  ;; (citar-embark-mode) ;; Causes "eldoc error: (void-function org-element--property)" when enabled
  )



(use-package citar-denote
  :after citar denote
  :custom
  (citar-open-always-create-notes t)
  :init
  (citar-denote-mode)
  :bind
  (("C-c w b c" . citar-create-note)
   ("C-c w b n" . citar-denote-open-note)
   ("C-c w b x" . citar-denote-nocite)
   :map org-mode-map
   ("C-c w b k" . citar-denote-add-citekey)
   ("C-c w b K" . citar-denote-remove-citekey)
   ("C-c w b d" . citar-denote-dwim)
   ("C-c w b e" . citar-denote-open-reference-entry)))



;;;;;; ORG-MODE
(use-package org
  :ensure nil
  :delight (org-indent-mode)
  :custom
  (org-global-properties  ; set global effort estimates:
   '(("Effort_ALL" . "0:00 0:07 0:15 0:30 0:45 1:00 1:30 2:00 2:30 3:00")))
  :bind (("C-c l" . org-store-link)
	 ("C-S-v" . scroll-other-window)
	 ("M-V"   . scroll-other-window-down)
	 ("C-c ." . org-time-stamp)
	 ("C-S-l" . org-toggle-link-display)
	 ("C-c a" . org-agenda)
	 :map org-mode-map
	 ("C-'"   . nil)  ;; Unbind C-' in org-mode so that it doesn't conflict with expand-region
	 :map org-agenda-mode-map                  ;; Mode-specific bindings
	 ("C-z"   . org-agenda-undo)
	 ("M-e"   . org-agenda-set-effort)
	 )
  :hook (org-agenda-mode . (lambda ()       ;; Hook for org-agenda-mode
                             (visual-line-mode -1)
                             (toggle-truncate-lines 1)))
  :config
  (setq
   org-startup-indented t
   org-startup-folded t
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"
   org-src-fontify-natively t
   org-src-window-setup 'current-window ;; edit in current window
   org-src-strip-leading-and-trailing-blank-lines t
   org-src-preserve-indentation t ;; do not put two spaces on the left
   org-src-tab-acts-natively t
   org-log-into-drawer t
   org-startup-with-inline-images t   ; only displays in the format [[file:path-to-file]], nothing else.
   org-image-actual-width '(300)
   org-duration-format 'h:mm
   org-clock-report-include-clocking-task t
   org-refile-use-outline-path 'file  ; Allow refiling to the top (file) level, rather than just to headings (which is the default)
   org-outline-path-complete-in-steps nil  ; setting org-refile-use-outline-path to 'file causes problems with auto-complete that prevents designating headings within the file. This fixes that.
   org-complete-tags-always-offer-all-agenda-tags t ;; suggest tags from all agenda files
   
   )

  ;; Use all org files as targets for refile, and refresh the list of refile targets each time org-refile is run
  (defun ewhd-org-refile-targets (&rest _)
    "Return a list of all .org files in the notes directory and subdirectories."
    (seq-filter
     (lambda (file)
       (not (string-match-p "/\\.#" file)))  ; Ignore files starting with .#
     (directory-files-recursively "~/Documents/notes" "\\.org$")))

  (defun ewhd-refresh-refile-targets (&rest _)
    "Refresh org-refile-targets before running org-refile."
    (setq org-refile-targets
          `((,(ewhd-org-refile-targets) . (:maxlevel . 3)))))

  (advice-add 'org-refile :before #'ewhd-refresh-refile-targets)

  ;; org TODO keywords:
  (setq org-todo-keywords
	'((sequence "TODO(t)"
		    "NEXT(n)"
		    "STRT(s)"
		    "WAIT(w@/!)"
		    "REVW(r!)"
		    "HOLD(h@/!)"
		    "|"
		    "CANC(x)"
		    "DONE(d)"
		    )
	  (sequence "OPEN(o)"
		    "EVNT(e)"
		    "|"
		    "COMP(c)")
	  (sequence "PROJ(p)"
		    ;; "PROJECT - ON-HOLD(@/)"
		    "|"
		    "FNSH(f)"
		    ))
	org-todo-keyword-faces ;; available named colors: https://www.raebear.net/computers/emacs-colors/
	'(("TODO". "purple")
	  ("NEXT" . "magenta")
	  ("STRT" . "pink")
	  ("WAIT" . "sky blue")
	  ("REVW" . "orange")
	  ("HOLD" . "cyan")
	  ("CANC" . "green")
	  ("DONE" . "green")
	  ("OPEN" . (:foreground "white" :background "purple"))
	  ("EVNT" . (:foreground "white" :background "blue"))
	  ("COMP" . (:foreground "white" :background "dark green"))
	  ("PROJ" . (:foreground "red" :weight bold))
	  ;; ("PROJECT - ON-HOLD" . (:foreground "cyan" :weight bold))
	  ("FNSH" . (:foreground "green" :weight bold))))

  ;; ORG TAGS
  (setq org-tag-faces ;; available named colors: https://www.raebear.net/computers/emacs-colors/
	'(("Urg"   . "dark orange")
          ("Imp"   . "orange red")
	  ("Prcs"  . "turquoise")
	  ("Imrsv" . "dodger blue")
	  ("Fnsh"  . "lawn green")
	  ("Cmplx" . "dark violet")
	  ("Dstr"  . "yellow2")
	  ("Dlybl" . "wheat")
	  ))

  ;; set colors in column view
  (defun ewhd-org-add-tag-color-in-column-view ()
    (font-lock-add-keywords nil
			    '(("\\(:Urg:\\)"   1 '(:foreground "dark orange" :weight bold) t)
			      ("\\(:Imp:\\)"   1 '(:foreground "orange red" :weight bold) t)
			      ("\\(:Prcs:\\)"  1 '(:foreground "turquoise" :weight bold) t)
			      ("\\(:Imrsv:\\)" 1 '(:foreground "dodger blue" :weight bold) t)
			      ("\\(:Fnsh:\\)"  1 '(:foreground "lawn green" :weight bold) t)
			      ("\\(:Cmplx:\\)" 1 '(:foreground "dark violet" :weight bold) t)
			      ("\\(:Dstr:\\)"  1 '(:foreground "yellow2" :weight bold) t)
			      ("\\(:Dlybl:\\)" 1 '(:foreground "wheat" :weight bold) t)) ;; change this--it's a little too close to the default off-white
			    t))
  (add-hook 'org-mode-hook #'ewhd-org-add-tag-color-in-column-view)


  ;; ORG PRIORITY
  (setq org-highest-priority ?A
	org-default-priority ?D
	org-lowest-priority ?D
	org-priority-faces '((?A . "red")
			     (?B . "tomato")
			     (?C . "orange")
			     (?D . "white")
			     ))


  ;; ORG-AGENDA
  ;; this function recursively searches ~/Documents and  adds any org file which contains gtd as a keyword to org-agenda-files
  ;; the regex here should exclude anything starting with a '.', ending with a '~' or a '#'
  (defun ewhd-refresh-org-agenda-files ()
    "Refresh the list of `org-agenda-files` dynamically."
    (setq org-agenda-files
          (seq-filter
           'file-exists-p
           (directory-files-recursively "~/Documents" "^[^\.#].*_gtd.*\\.org$"))))

  (add-hook 'org-agenda-mode-hook 'ewhd-refresh-org-agenda-files) ;; run ewhd-refresh-org-agenda-files whenever agenda-mode is called
  (ewhd-refresh-org-agenda-files)


  ;; org-agenda-mode behavior
  (setq org-agenda-window-setup 'current-window  ; agenda takes current window
	org-agenda-restore-windows-after-quit t  ; restore window configuration on exit
	org-agenda-start-with-follow-mode nil
	org-columns-default-format-for-agenda "%45ITEM %4TODO %1PRIORITY %4Effort(Estim){:}  %4CLOCKSUM(Clock) %20ALLTAGS"
	org-agenda-clockreport-parameter-plist '(:link t :maxlevel 5) ; set the depth of headers referenced by org-agenda-clockreport-mode
	org-agenda-time-grid '((daily today require-timed)
			       (600 900 1200 1500 1800 2100)
			       "......" "----------------------" nil)
	org-agenda-skip-scheduled-if-done t
	org-agenda-skip-deadline-if-done t
	org-agenda-include-deadlines t
	org-agenda-include-diary t
	org-agenda-block-separator 9472
	org-agenda-compact-blocks nil
	org-agenda-start-with-log-mode t
	org-agenda-hide-tags-regexp (regexp-opt '("cf" "gtd"))  ;; hides specific tags
	org-agenda-remove-tags t  ;; hides all tags
	org-agenda-show-inherited-tags t
	org-tags-exclude-from-inheritance '("gtd"
					    "Proj"
					    "Cmplx"
					    "Fnsh"
					    "Dstr"
					    "Dlybl"
					    "Prcs"
					    "Imrsv")
	;; org-agenda-compact-blocks t
	org-agenda-timegrid-use-ampm t
	org-agenda-confirm-kill t
	)

  (setq ewhd-org-agenda-prefix-formats
	'(((agenda   . " %?s%-12t")   ; Format 1
           (timeline . " %s")
           (todo     . " ")
           (tags     . " ")
           (search   . " "))
          ((agenda   . " %-12:c%-s%-12t")   ; Format 2
           (timeline . " %s")
           (todo     . " %-12:c")
           (tags     . " %-12:c")
           (search   . " %-12:c"))
          ((agenda   . " %?s%-12t")   ; Format 3
           (timeline . " %s")
           (todo     . " %-5e")
           (tags     . " %i [TAG] %s")
           (search   . " "))
	  ))

  (defun set-org-agenda-prefix-format (n)
    "Set `org-agenda-prefix-format` to the Nth format in `ewhd-org-agenda-prefix-formats`."
    (interactive "nChoose prefix format (1, 2, or 3): ")
    (let ((format (nth (1- n) ewhd-org-agenda-prefix-formats)))
      (if format
          (progn
            (setq org-agenda-prefix-format format)
            (message "Using org-agenda-prefix-format %d" n)
            (when (get-buffer "*Org Agenda*")
	      (org-agenda-redo)))
	(message "Invalid format number"))))
  ;; (global-unset-key (kbd "L"))
  (define-prefix-command 'org-agenda-prefix-format-map)
  (define-key org-agenda-mode-map (kbd "L") 'org-agenda-prefix-format-map)
  (define-key org-agenda-prefix-format-map
	      (kbd "1") (lambda () (interactive) (set-org-agenda-prefix-format 1)))
  (define-key org-agenda-prefix-format-map
	      (kbd "2") (lambda () (interactive) (set-org-agenda-prefix-format 2)))
  (define-key org-agenda-prefix-format-map
	      (kbd "3") (lambda () (interactive) (set-org-agenda-prefix-format 3)))
  (set-org-agenda-prefix-format 1) ;; set initial prefix format
  ) ;; END OF ORG BLOCK



;;;; OTHER ORG-MODE PACKAGES
(use-package org-modern
  :ensure t
  ;; :hook
  ;; (org-mode . org-modern-mode)
  ;; (org-agenda-finalize . org-modern-agenda)
  :config
  (setq global-org-modern-mode t
	org-modern-hide-stars t    ; adds extra indentation
	org-modern-table t
	org-modern-list '((?* . "•") (?+ . "‣"))
	org-modern-block-name '("" . "")  ; or other chars; so top bracket is drawn promptly
	)
  )



(use-package org-super-agenda
  :ensure t
  :after org
  :init
  (org-super-agenda-mode 1))



(use-package org-ql
  :ensure t
  :after org)



;; Show hidden emphasis markers
(use-package org-appear
  :ensure t
  :hook
  (org-mode . org-appear-mode)
  :config
  (setq
   org-appear-autolinks nil
   org-appear-autosubmarkers t
   ;; org-appear-delay .7
   )
  )



;;https://github.com/alphapapa/org-ql/blob/master/examples.org
;; https://github.com/alphapapa/org-ql/issues/79

;; (setq org-super-agenda-groups
;;       '((:log t :order 0)
;;         (:name "Schedule" :time-grid t)
;;         (:name "Finishable" :tag "Fnsh")
;;         (:name "Priority A" :priority "A")
;;         (:name "Complex" :tag "Cmplx")))

;; (add-hook 'org-agenda-mode-hook 'org-super-agenda-mode)

;; (setq org-agenda-custom-commands
;;       '(("c" "Custom Agenda"
;;          ((agenda "")
;;           (alltodo "")))))



(use-package org-super-agenda
  :after org
  :custom
  (setq org-agenda-custom-commands
	'(("z" "Super view"
           ((agenda "" ((org-agenda-span 8)
			(org-agenda-use-time-grid t)
			;; (org-super-agenda-groups
			;;  '((:name "Today"
			;;           :time-grid t
			;;           :order 1)
			;;    ))
			))
            (alltodo "" ((org-agenda-overriding-header "The only sin is impatience")
			 ;; (org-agenda-span 7)
			 (org-super-agenda-groups
                          '(
                            (:name "Upcoming"
                                   :deadline future
                                   :scheduled future
                                   :order 40)
                            (:name "Overdue"
                                   :deadline past
                                   :order 10)
                            (:name "Behind Schedule"
                                   :scheduled past
                                   :order 20)
			    (:name "Prioritize:" :priority "A" :order 50)
			    (:name "Distractions:" :tag "Dstr" :order 65)
                            (:name "Then:" :and (:todo "NEXT" :priority>= "C") :order 60)
                            (:name "Continue:" :todo "STRT" :order 70)
                            (:name "Someday:"
                                   :priority<= "D"
				   :todo "HOLD"
                                   :order 120)
                            (:name "Other Todo:" :todo "TODO" :order 80)
                            (:name "Misc."
                                   :todo ("REVW" "WAIT")
                                   :order 90)
                            (:todo "OPEN" :order 150)
                            (:discard (:todo ("PROJ")))
                            ))))))
	  ("x" "Super view - no agenda"
           ((alltodo "" ((org-agenda-overriding-header "The only sin is impatience")
			 ;; (org-agenda-span 7)
			 (org-super-agenda-groups
                          '(
                            (:name "Upcoming"
                                   :deadline future
                                   :scheduled future
                                   :order 40)
                            (:name "Overdue"
                                   :deadline past
                                   :order 10)
                            (:name "Behind Schedule"
                                   :scheduled past
                                   :order 20)
			    (:name "Prioritize:" :priority "A" :order 50)
			    (:name "Distractions:" :tag "Dstr" :order 65)
                            (:name "Then:" :and (:todo "NEXT" :priority>= "C") :order 60)
                            (:name "Continue:" :todo "STARTED" :order 70)
                            (:name "Someday:"
                                   :priority<= "D"
				   :todo "HOLD"
                                   :order 120)
                            (:name "Other Todo:" :todo "TODO" :order 80)
                            (:name "Misc."
                                   :todo ("REVIEW" "WAITING")
                                   :order 90)
                            (:todo "OPEN" :order 150)
                            (:discard (:todo ("PROJECT - OPEN" "PROJECT - ON-HOLD")))
                            ))))))
	  )))



(use-package beancount-mode
  :ensure (:host github :repo "beancount/beancount-mode" :main "beancount.el")
  ;; :init
  ;; (add-to-list 'load-path "~/.config/emacs/elpaca/repos/beancount-mode/")
  ;; (require 'beancount)
  :config
  (add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))  ;; Automatically enable beancount-mode in .beancount files
  (add-hook 'beancount-mode-hook #'outline-minor-mode)  ;; Automatically enable outline-mode.
  (setq beancount-outline-regexp "\\(\\*+\\)")  ;; Make sure we don't accidentally pick up ;;; as headers. Use org section headers only.
  (add-hook 'beancount-mode-hook #'flymake-bean-check-enable)  ;; Enables on-the-fly checks on your ledger file using bean-check via flymake
  (unless (assq 'python-logging compilation-error-regexp-alist-alist)  ;; Support parsing Python logging errors, with a suitable logging.basicConfig() format.
    
    (add-to-list
     'compilation-error-regexp-alist-alist
     '(python-logging "\\(ERROR\\|WARNING\\):\\s-*\\([^:]+\\):\\([0-9]+\\)\\s-*:" 2 3))
    
    (add-to-list
     'compilation-error-regexp-alist 'python-logging)
    )

  ;; Initialize beancount-mode-map
  (unless (boundp 'beancount-mode-map)
    (setq beancount-mode-map (make-sparse-keymap)))

  ;; Experimental: Bind a key to reformat the entire file using bean-format.
  (defun beancount-format-file--experimental ()
    (interactive)
    (let ((line-no (line-number-at-pos)))
      (call-process-region (point-min) (point-max) "bean-format" t (current-buffer))
      (goto-line line-no)
      (recenter)
      ))
  (define-key beancount-mode-map (kbd "C-c F") 'beancount-format-file--experimental)

  (defvar beancount-balance-command
    (concat
     "select account, sum(position) "
     "where account ~ '%s' "
     "group by 1 "
     "order by 1"))

  (defun beancount-query-balance-at-point ()
    "Run a balance command for the account at point."
    (interactive)
    (let ((account (thing-at-point 'beancount-account)))
      (beancount--run beancount-query-program
                      (file-relative-name buffer-file-name)
                      (format beancount-balance-command account))))

  :bind
  ;; Add movement between sections (replaces default outline-minor-mode
  ;; keybindings with org-style keybindings)
  (("C-c C-n" . #'outline-next-visible-heading)
   ("C-c C-p" . #'outline-previous-visible-heading)
   ("C-c C-u" . #'outline-up-heading))
  (kbd "C-c J" . #'beancount-query-balance-at-point)
  )




;; stuff I'm playing around with just on my desktop for now
(when (string= system-name "ewhd-t730-debian")
  (setq org-columns-default-format-for-agenda
        "%45ITEM %4TODO %1PRIORITY %4Effort(Estim){:}  %4CLOCKSUM(Clock) %38ALLTAGS"
	org-agenda-tags-column 80
	org-agenda-remove-tags nil
	))

(when (string= system-name "ewhd-book")
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (setq org-columns-default-format-for-agenda
        "%45ITEM %4TODO %1PRIORITY %4Effort(Estim){:}  %4CLOCKSUM(Clock) %38ALLTAGS"
	org-agenda-tags-column 'auto
	org-agenda-remove-tags nil
	org-tags-column -50
	))
