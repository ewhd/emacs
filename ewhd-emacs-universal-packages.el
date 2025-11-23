;; ewhd emacs universal packages -*- lexical-binding: t; -*-

;; Ensure Transient is the most recent version (needed for magit):
(use-package transient
  :ensure t)


;; bufler
(use-package bufler
  :ensure t
  ;; (bufler :fetcher github :repo "alphapapa/bufler.el"
  ;; :files (:defaults (:exclude "helm-bufler.el")))
  :config
  (setq bufler-column-Path-max-width 80)
  )


;; yaml-mode
(use-package yaml-mode
  :ensure t
  :mode (("\\.ya?ml\\(?:\\.tmpl\\)?\\'" . yaml-mode)
         ("\\.ya?ml\\.j2\\'" . yaml-mode))
  :hook (yaml-mode . (lambda () (setq-local tab-width 2)))
  )


;; centered-cursor-mode
(use-package centered-cursor-mode
  :ensure t
  :bind
  ("C-M-_" . centered-cursor-mode)
  :config
  (unbind-key "C-v" ccm-map)
  (setq ccm-recenter-at-end-of-file t)
  (setq ccm-vpos-init '(round (ccm-visible-text-lines) 3))
  (add-to-list 'ccm-ignored-commands 'recenter-top-bottom)
  )


;; persistent-scratch
(use-package persistent-scratch
  :ensure t
  ;; :after no-littering
  ;; :custom
  ;; (persistent-scratch-save-file (no-littering-expand-var-file-name "scratch"))
  :config
  (persistent-scratch-setup-default)    ; enable autosave and restore the last
					; saved state
  (setq persistent-scratch-autosave-interval '(idle . 30))
  )

;; Expand-Region
(use-package expand-region
  :ensure t
  :bind (("C-'" . er/expand-region)
	 ("C-\"" . er/mark-outside-quotes)
	 )
  )

;; aggressive-indent-mode
(use-package aggressive-indent
  :hook ((clojure-mode . aggressive-indent-mode)
	 (emacs-lisp-mode . aggressive-indent-mode)
	 (css-mode . aggressive-indent-mode))
  )

;;;; Pretty icons
;; must run M-x all-the-icons-install-fonts to install fonts per machine
;; (use-package all-the-icons
;;   :ensure t
;;   :if (display-graphic-p))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-completion
  :ensure t
  :hook ((vertico-mode . nerd-icons-completion-mode)
         (ivy-mode . nerd-icons-completion-mode)
         (helm-mode . nerd-icons-completion-mode)))


;;;; Source Control:
;; git-timemachine
(use-package git-timemachine
  :ensure t)

;; Magit:
(use-package ov.el
  ;; Needed for magit-log date headers, by alphapapa
  ;; If I ever use it for anything else I'll move it elsewhere in the config
  :ensure t
  )

(use-package magit
  :after (transient ov)
  :ensure t
  :bind (("C-x g" . magit))
  :config
  ;; Function to remove local branches already merged into another branch
  ;; https://emacs.stackexchange.com/questions/60200/magit-remove-local-branches-that-were-merged-into-another-branch/81021#81021
  (defun ewhd-delete-merged-branches ()
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
  

  (defun unpackaged/magit-log--add-date-headers (&rest _ignore)
    "Add date headers to Magit log buffers."
    (when (derived-mode-p 'magit-log-mode)
      (save-excursion
        (ov-clear 'date-header t)
        (goto-char (point-min))
        (cl-loop with last-age
                 for this-age = (-some--> (ov-in 'before-string 'any (line-beginning-position) (line-end-position))
                                  car
                                  (overlay-get it 'before-string)
                                  (get-text-property 0 'display it)
                                  cadr
                                  (s-match (rx (group (1+ digit) ; number
                                                      " "
                                                      (1+ (not blank))) ; unit
                                               (1+ blank) eos)
                                           it)
                                  cadr)
                 do (when (and this-age
                               (not (equal this-age last-age)))
                      (ov (line-beginning-position) (line-beginning-position)
                          'after-string (propertize (concat " " this-age "\n")
                                                    'face 'magit-section-heading)
                          'date-header t)
                      (setq last-age this-age))
                 do (forward-line 1)
                 until (eobp)))))

  (define-minor-mode unpackaged/magit-log-date-headers-mode
    "Display date/time headers in `magit-log' buffers."
    :global t
    (if unpackaged/magit-log-date-headers-mode
        (progn
          ;; Enable mode
          (add-hook 'magit-post-refresh-hook #'unpackaged/magit-log--add-date-headers)
          (advice-add #'magit-setup-buffer-internal :after #'unpackaged/magit-log--add-date-headers))
      ;; Disable mode
      (remove-hook 'magit-post-refresh-hook #'unpackaged/magit-log--add-date-headers)
      (advice-remove #'magit-setup-buffer-internal #'unpackaged/magit-log--add-date-headers))

    (unpackaged/magit-log-date-headers-mode 1)))


;; Chezmoi
(use-package chezmoi
  :ensure t
  )


;;;; Navigation
;; Dired+
;; I've been a little unsure of the proper syntax with elpaca, whether it should be :elpaca or :ensure, but based on this link I'm going with :ensure
;; https://github.com/weirdNox/dotfiles/blob/d9b1da6d60bde008d1b8f95a411b2e3e399dc30c/config/.config/emacs/config.org#diredfl-and-dired
;; (use-package dired+
;;   :ensure (dired+ :host github :repo "emacsmirror/dired-plus")
;;   :after dired
;;   :bind
;;   ()
;;   :config
;; Additional dired+ specific configurations here
;; Hide details by default
;; (setq diredp-hide-details-initially-flag t)

;; (diredp-toggle-find-file-reuse-dir 1)  ;; start with diredp set to "reuse" buffer

;; (define-key dired-mode-map (kbd "S-") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory

;; Enable image previews
;; (setq diredp-image-preview-in-tooltip 128)
;; )

;; (use-package dired-subtree
;;   :ensure t
;;   :after dired
;;   :config
;;   (setq dired-subtree-use-backgrounds nil)

;;   (defun ewhd-dired-subtree-toggle ()
;;     "Toggle the subtree and reflect the dired-omit-mode from the parent Dired buffer."
;;     (interactive)
;;     (dired-subtree-toggle)
;;     ;; Check if dired-omit-mode is enabled in the parent buffer
;;     (if (and (derived-mode-p 'dired-mode)
;;              dired-omit-mode)
;;         (dired-omit-mode 1)  ; Enable dired-omit-mode in the subtree if it's on in the parent
;;       (dired-omit-mode -1))) ; Disable dired-omit-mode in the subtree if it's off in the parent

;;   :bind
;;   ( :map dired-mode-map
;;     ("<tab>" . ewhd-dired-subtree-toggle)
;;     ("TAB" . ewhd-dired-subtree-toggle)
;;     ("<backtab>" . dired-subtree-remove)
;;     ("S-TAB" . dired-subtree-remove))
;; )

;; (use-package dired-preview
;;   :after dired
;;   :ensure t
;;   ;; :defer 1
;;   ;; :hook (after-init . dired-preview-global-mode)
;;   :bind
;;   ( :map dired-mode-map
;;     ("P" . dired-preview-mode)
;;     ("S-<up>" . dired-preview-page-up)
;;     ("S-<down>" . dired-preview-page-down))
;;   :config
;;   (setq dired-preview-max-size (* (expt 2 20) 10))
;;   (setq dired-preview-delay 0.5)
;;   (setq dired-preview-ignored-extensions-regexp
;;         (concat "\\."
;;                 "\\(gz\\|"
;;                 "zst\\|"
;;                 "tar\\|"
;;                 "xz\\|"
;;                 "rar\\|"
;;                 "zip\\|"
;;                 "iso\\|"
;;                 "epub"
;;                 "\\)"))
;;   (setq dired-preview-display-action-alist
;;         '((display-buffer-in-side-window)
;;           ;; (side . bottom)
;;           ;; (window-height . 0.4)
;;           (side . right)
;;           (window-width . 0.5)
;;           (preserve-size . (t . t))
;;           (window-parameters . ((mode-line-format . none)
;;                                 (header-line-format . none))))))

;; avy
(use-package avy
  :ensure t
  :bind
  (("C-/" . avy-goto-char-timer)
   ("C-\\" . 'avy-goto-line)
   ("C-|" . 'avy-goto-end-of-line)
   :map isearch-mode-map
   ("M-j" . avy-isearch)
   )
  :config
  (setq avy-timeout-seconds 1.5)

  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  (defun avy-action-flyspell (pt)
    (save-excursion
      (goto-char pt)
      (when (require 'flyspell nil t)
        (flyspell-auto-correct-word)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line
	(alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line
	(alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line
	(alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char
	(alist-get ?\; avy-dispatch-alist) 'avy-action-flyspell
	(alist-get ?. avy-dispatch-alist) 'avy-action-embark
	)


  (setq avy-single-candidate-jump nil)  

  (defun ewhd-avy-show-actions-auto (&rest _)
    "Automatically display Avy action shortcuts in the minibuffer with formatted columns, trimming 'avy-action-' prefix and adding colors."
    (when avy-dispatch-alist
      (let* ((actions (mapcar (lambda (entry)
				(let* ((action (cdr entry))
                                       (action-name (if (functionp action)
							(symbol-name action)  ;; Extract function name
                                                      action)))  ;; Otherwise, it's already a string
                                  (let ((action-name-colored
					 (propertize
                                          (replace-regexp-in-string "^avy-action-" "" action-name)
                                          'face '(:foreground "cyan"))))
                                    ;; Format the final string
                                    (format "%s: %s"
                                            (char-to-string (car entry))
                                            action-name-colored))))
                              avy-dispatch-alist))
             (max-action-length (apply 'max (mapcar #'length actions)))
             (column-width (+ max-action-length 2))  ;; Add some space between columns
             (max-columns (/ (- (frame-width) 4) column-width))  ;; Full frame width
             (rows (seq-partition actions max-columns))
             (formatted-actions (mapconcat
				 (lambda (row)
                                   (mapconcat (lambda (action)
						(format (concat "%-" (number-to-string column-width) "s") action))
                                              row ""))
				 rows "\n")))
	(message "%s" formatted-actions))))


  (advice-add 'avy-read :before #'ewhd-avy-show-actions-auto)
  )

;; Dirvish
(use-package dirvish
  :disabled t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("n" "~/Documents/notes/"          "Notes")
     ("e" "~/.config/emacs/"            "Emacs")
     ("c" "~/.config/"                  "Config")
     ("lb" "~/.local/bin/"              "Local Bin")
     ("ls" "~/.local/share/"            "Local Share")
     ("C" "~/.local/share/chezmoi"      "Chezmoi")
     ("d" "~/Documents/"                "Documents")
     ("D" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")
     ("t" "~/.local/share/Trash/files/" "TrashCan")
     ("h" "~/"                          "Home")))
  :config
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group --time-style=long-iso --color=always"
        ;; "-aGFhl --sort-version --group-directories-first --time-style=long-iso" 
	)
  (setq dirvish-default-layout nil) ; default (1 0.11 0.55)
  (setq dirvish-layout-recipes '((0 0 0.5) (0 0 0.7) (1 0.11 0.55)))
					; default ((0 0 0.4) (0 0 0.8) (1 0.08 0.8) (1 0.11 0.55))

  (add-hook 'dirvish-find-entry-hook
            (lambda (&rest _) (setq-local truncate-lines t)))
  (setq dirvish-reuse-session 'resume) ; retain dirvish session and resume it unless called with a directory path

  ;; Unbind default C-x d to use as prefix
  (global-unset-key (kbd "C-x d"))
  (define-prefix-command 'ctl-x-d-map)
  (global-set-key (kbd "C-x d") 'ctl-x-d-map)
  
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
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
   ()
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)
   ("M-h" . dired-omit-mode)
   ("M"   . dirvish-move)
   ))

;;;; Layout
;; transpose-frame
;; https://www2.lib.uchicago.edu/keith/emacs/minimacs.html
(use-package transpose-frame
  :defer t
  :commands
  (transpose-frame
   flip-frame
   flop-frame
   rotate-frame
   rotate-frame-clockwise
   rotate-frame-anticlockwise)
  :bind (("C-x 6" . transpose-frame)))


;; activities
(use-package activities
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)

  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)))

;; Olivetti
;; https://emacs.stackexchange.com/a/47989/43990
(use-package olivetti
  :hook (
	 ;; (dired-mode . (lambda () (setup-olivetti 120)))
	 (org-mode . (lambda () (setup-olivetti 100)))
	 )
  :config
  (defun setup-olivetti (width)
    "Set up olivetti-mode with a specific body width."
    (unless olivetti-mode
      (setq olivetti-body-width width)
      (olivetti-mode 1))))



;;;; Modeline:
;; Delight enables you to easily customise how major and minor modes appear in the ModeLine.
;; As per use-package README: delight is invoked with the :delight keyword, which is passed a minor mode symbol, a replacement string or quoted mode-line data (in which case the minor mode symbol is guessed to be the package name with “-mode” appended at the end), both of these, or several lists of both. If no arguments are provided, the default mode name is hidden completely.
(use-package delight
					;  :delight (org-indent-mode) ; This belongs in org section
  )
                                        ;(elpaca-wait) ; I'm unsure if this is needed

;; telephone-line
(use-package telephone-line
  :config
  (telephone-line-defsegment* ewhd-telephone-line-file-name-absolute-path-segment ()
    (propertize
     (if (buffer-file-name)
         (abbreviate-file-name (buffer-file-name))
       (buffer-name))
     'face 'mode-line-buffer-id))

  (telephone-line-defsegment* ewhd-telephone-line-major-mode-segment ()
    (cond
     ((eq major-mode 'emacs-lisp-mode) "λ")
     ((eq major-mode 'dired-mode) " ")
     ((eq major-mode 'help-mode) "")
     ((eq major-mode 'python-mode) "")
     ((eq major-mode 'sh-mode) "")
     ((eq major-mode 'lisp-mode) "")
     (t mode-name))) ;; fallback to normal mode-name

  (defface ewhd-mode-line-custom-face
    '((t (
          :foreground "red"
          :background nil
          :inherit nil)))
    "Custom modeline face")

  ;; (telephone-line-defsegment* ewhd-telephone-line-airline-position-segment ()
  ;;   (eval (propertize "%l:%c" 'face 'mode-line-custom-face)))

  ;; (telephone-line-defsegment* ewhd-telephone-line-airline-position-segment ()
  ;;   " %l:%c")
  (telephone-line-defsegment* ewhd-telephone-line-airline-position-segment ()
    ;; (let ((current (line-number-at-pos))  ; causes lag to line/column number update
    ;;       (total   (line-number-at-pos (point-max)))
    ;;       (column  (current-column)))
    ;;   (format "%d/%d:%d" current total column))
    "%l:%c"
    )

  ;; --- Cache variables ---
  (defvar ewhd-chezmoi--source-files-cache nil
    "Cached list of absolute paths to chezmoi source files.")

  (defvar ewhd-chezmoi--target-files-cache nil
    "Cached list of absolute paths to chezmoi target files.")

  (defvar ewhd-chezmoi--cache-timestamp 0
    "Time (in seconds) when the chezmoi file cache was last updated.")

  (defvar ewhd-chezmoi--cache-ttl 300
    "Time-to-live (seconds) for the chezmoi file cache. Refresh after this interval.")

  (defface ewhd-chezmoi-source-face
    '((t :foreground "#61AFEF" :weight bold))
    "Face for chezmoi source files in the modeline.")

  (defface ewhd-chezmoi-target-face
    '((t :foreground "#98C379" :weight bold))
    "Face for chezmoi target files in the modeline.")

  (defun ewhd-chezmoi--refresh-cache ()
    "Refresh cached lists of source and target files."
    (setq ewhd-chezmoi--source-files-cache
          (split-string
           (shell-command-to-string "chezmoi managed -p source-absolute")
           "\n" t))
    (setq ewhd-chezmoi--target-files-cache
          (split-string
           (shell-command-to-string "chezmoi managed -p absolute")
           "\n" t))
    (setq ewhd-chezmoi--cache-timestamp (float-time)))

  (defun ewhd-chezmoi--ensure-cache ()
    "Ensure the cache is fresh, refresh if older than TTL."
    (when (> (- (float-time) ewhd-chezmoi--cache-timestamp) ewhd-chezmoi--cache-ttl)
      (ewhd-chezmoi--refresh-cache)))

  ;; --- Buffer check functions ---
  (defun ewhd-chezmoi-source-buffer-tracked-p ()
    "Return non-nil if the current buffer's file is a chezmoi source file."
    (ewhd-chezmoi--ensure-cache)
    (and buffer-file-name
         (member buffer-file-name ewhd-chezmoi--source-files-cache)))

  (defun ewhd-chezmoi-target-buffer-tracked-p ()
    "Return non-nil if the current buffer's file is a chezmoi target file."
    (ewhd-chezmoi--ensure-cache)
    (and buffer-file-name
         (member buffer-file-name ewhd-chezmoi--target-files-cache)))

  (telephone-line-defsegment* ewhd-telephone-line-buffer-segment ()
    "Display buffer info with a chezmoi indicator if relevant."
    (let ((chezmoi-indicator
           (cond
            ((ewhd-chezmoi-source-buffer-tracked-p) "") ;; source icon 
            ((ewhd-chezmoi-target-buffer-tracked-p) "") ;; target icon 
            (t "-"))))
      `(""
        mode-line-modified
        mode-line-client
        mode-line-remote
        ,chezmoi-indicator)))



  (setq telephone-line-lhs
        '((evil   . (ewhd-telephone-line-buffer-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment
                     ))
          (nil    . (
                     ;; telephone-line-simple-minor-mode-segment
                     ;; telephone-line-buffer-segment
                     ;; ewhd-modeline-buffer-name-segment
                     ewhd-telephone-line-file-name-absolute-path-segment
                     ))))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (ewhd-telephone-line-major-mode-segment))
          (evil   . (ewhd-telephone-line-airline-position-segment))))

  (telephone-line-mode 1)
  )


;;;; Theme:
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium t)
  )


;;;; Display Key Context
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


;;;; Spelling:
(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map
	      ("C-;" . flyspell-correct-wrapper)
	      )
  )

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; (use-package spell-fu
  ;; :config
  ;; (setq spell-fu-word-delimit-camel-case t)
  ;; )

;; (add-hook 'org-mode-hook
;;   (lambda ()
;;     (setq spell-fu-faces-exclude
;;      '(org-block-begin-line
;;        org-block-end-line
;;        org-code
;;        org-date
;;        org-drawer org-document-info-keyword
;;        org-ellipsis
;;        org-link
;;        org-meta-line
;;        org-properties
;;        org-properties-value
;;        org-special-keyword
;;        org-src
;;        org-tag
;;        org-verbatim))
;;     (spell-fu-mode)))

;; (add-hook 'emacs-lisp-mode-hook
;;   (lambda ()
;;     (spell-fu-mode)))


;;;; Multiple Cursors:
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



;;;; Undo/Redo:
(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

;; Vundo config mostly borrowed from https://www.reddit.com/r/emacs/comments/txwwfi/vundo_is_great_visual_undotree_for_emacs28/
(use-package vundo
  :commands (vundo)

  :config
  ;; Take less on-screen space.
  (setq vundo-compact-display t)

  ;; Use prettier Unicode characters
  (setq vundo-glyph-alist vundo-unicode-symbols)
  
  ;; Better contrasting highlight.
  (custom-set-faces
    '(vundo-node ((t (:foreground "#808080"))))
    '(vundo-stem ((t (:foreground "#808080"))))
    '(vundo-highlight ((t (:foreground "#FFFF00")))))

  ;; Use `HJKL` VIM-like motion, also Home/End to jump around.
  (define-key vundo-mode-map (kbd "l") #'vundo-forward)
  (define-key vundo-mode-map (kbd "<right>") #'vundo-forward)
  (define-key vundo-mode-map (kbd "h") #'vundo-backward)
  (define-key vundo-mode-map (kbd "<left>") #'vundo-backward)
  (define-key vundo-mode-map (kbd "j") #'vundo-next)
  (define-key vundo-mode-map (kbd "<down>") #'vundo-next)
  (define-key vundo-mode-map (kbd "k") #'vundo-previous)
  (define-key vundo-mode-map (kbd "<up>") #'vundo-previous)
  (define-key vundo-mode-map (kbd "<home>") #'vundo-stem-root)
  (define-key vundo-mode-map (kbd "<end>") #'vundo-stem-end)
  (define-key vundo-mode-map (kbd "q") #'vundo-quit)
  (define-key vundo-mode-map (kbd "C-g") #'vundo-quit)
  (define-key vundo-mode-map (kbd "RET") #'vundo-confirm))

(global-set-key (kbd "C-M-u") 'vundo)



;;;; Completion Tools:
;; Vertico:
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (setq vertico-count 15)
  )

;; Persist history over Emacs restarts. Vertico sorts history by position.
(savehist-mode 1)

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :init
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

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Marginalia:
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; Embark:
(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

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
                 (window-parameters (mode-line-format . none)))))

;; Orderless:
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))


;; Consult:
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ;; ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ;; ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ;; ("M-g e" . consult-compile-error)
         ;; ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

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



;;;; Posframe and related add-ons
(use-package posframe
  :ensure t
  )

(use-package vertico-posframe
  :ensure t
  :requires vertico posframe
  :config
  (vertico-posframe-mode 1)
  (setq vertico-posframe-parameters
	'((left-fringe . 8)
          (right-fringe . 8))
        vertico-posframe-border-width 3
        vertico-posframe-min-height 3
        vertico-posframe-max-width 100  ; Don't go too wide on ultrawide monitors
	)
  
  (set-face-background 'vertico-posframe nil)
  (set-face-background 'vertico-posframe-border "LightSlateGray")


  )

(use-package transient-posframe
  :ensure (transient-posframe :host github :repo "yanghaoxie/transient-posframe")
  ;; :load-path "path/to/transient-posframe.el"
  :requires (transient posframe)
  :config
  (transient-posframe-mode))

(use-package which-key-posframe
  :ensure (which-key-posframe :host github :repo "yanghaoxie/which-key-posframe")
  :requires (which-key posframe)
  :config
  (which-key-posframe-mode))


;;;; Financial
;; beancount-mode
(use-package beancount
  :ensure (:host github :repo "beancount/beancount-mode" :main "beancount.el")
  ;; :init
  ;; (add-to-list 'load-path "~/.config/emacs/elpaca/repos/beancount-mode/")
  ;; (require 'beancount)
  :config
  ;; Automatically enable beancount-mode in .beancount files
  (add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))
  
  ;; Automatically enable outline-mode.
  (add-hook 'beancount-mode-hook #'outline-minor-mode)

  ;; Make sure we don't accidentally pick up ;;; as headers. Use org section headers only.
  (setq beancount-outline-regexp "\\(\\*+\\)")

  ;; Enables on-the-fly checks on your ledger file using bean-check via flymake
  (add-hook 'beancount-mode-hook #'flymake-bean-check-enable)

  ;; Support parsing Python logging errors, with a suitable
  ;; logging.basicConfig() format.
  (require 'compile)
  (unless (assq 'python-logging compilation-error-regexp-alist-alist)
    
    (add-to-list
     'compilation-error-regexp-alist-alist
     '(python-logging "\\(ERROR\\|WARNING\\):\\s-*\\([^:]+\\):\\([0-9]+\\)\\s-*:" 2 3))
    
    (add-to-list
     'compilation-error-regexp-alist 'python-logging)
    )
  )

;; Initialize beancount-mode-map
(unless (boundp 'beancount-mode-map)
  (setq beancount-mode-map (make-sparse-keymap)))

;; Add movement between sections (replaces default outline-minor-mode
;; keybindings with org-style keybindings)
(define-key beancount-mode-map (kbd "C-c C-n") #'outline-next-visible-heading)
(define-key beancount-mode-map (kbd "C-c C-p") #'outline-previous-visible-heading)
(define-key beancount-mode-map (kbd "C-c C-u") #'outline-up-heading)

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
  
(define-key beancount-mode-map (kbd "C-c J") #'beancount-query-balance-at-point)


;;;; Markdown
;; Markdown-Mode
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"      . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do))
  :config
  ;; Set custom markdown preview function
  (setq markdown-live-preview-window-function #'markdown-live-preview-window-eww)

  ;; always open the preview window at the right
  (setq markdown-split-window-direction 'right)

  ;; delete exported HTML file after markdown-live-preview-export is called
  (setq markdown-live-preview-delete-export 'delete-on-export)
  )

;;; AI integration
(use-package gptel
  :ensure t
  :commands (gptel)
  :init
  ;; Default model for most queries
  (setq gptel-default-model "gpt-4o-mini")
  ;; Optional: set a fallback model
  (setq gptel-fallback-model "gpt-3.5-turbo")
  ;; Fetch API key securely from auth-source
  (setq gptel-api-key
        (auth-source-pick-first-password
         :host "api.openai.com" :user 'emacs :port nil))
  :config
  ;; Keybinding to open GPTel quickly
  (global-set-key (kbd "C-c a") 'gptel)

  ;; Function to send selected region to GPTel
  (defun ewhd-gptel-region (start end)
    "Send the active region to GPTel and show the response in a new buffer."
    (interactive "r")
    (let ((prompt (buffer-substring-no-properties start end)))
      (gptel prompt)))
  ;; Optional: switch model quickly in an active GPTel buffer
  (defun ewhd-gptel-set-model (model)
    "Set the GPTel model in the current buffer."
    (interactive
     (list (completing-read "Model: " '("gpt-4o-mini" "gpt-4" "gpt-3.5-turbo"))))
    (setq-local gptel-default-model model)
    (message "GPTel model set to %s" model))
  )

;;End
