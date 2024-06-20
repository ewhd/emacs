;; ewhd emacs universal packages -*- lexical-binding: t; -*-



;; Ensure Transient is the most recent version (needed for magit):
(use-package transient
  :ensure t)

;;;; Source Control:
;; Magit:
(use-package magit
  :after transient
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
  )


;;;; Navigation
;; Dired+
;; I've been a little unsure of the proper syntax with elpaca, whether it should be :elpaca or :ensure, but based on this link I'm going with :ensure
;; https://github.com/weirdNox/dotfiles/blob/d9b1da6d60bde008d1b8f95a411b2e3e399dc30c/config/.config/emacs/config.org#diredfl-and-dired
(use-package dired+
  :ensure (dired+ :host github :repo "emacsmirror/dired-plus")
  :after dired
  :bind
  ()
  :config
  ;; Additional dired+ specific configurations here
  ;; Hide details by default
  (setq diredp-hide-details-initially-flag t)
  
  ;; (diredp-toggle-find-file-reuse-dir 1)  ;; start with diredp set to "reuse" buffer
  
  ;; (define-key dired-mode-map (kbd "S-") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory
  
  ;; Enable image previews
  (setq diredp-image-preview-in-tooltip 128)
  
  ;; Define custom keybindings
  (define-key dired-mode-map (kbd "C-c C-e") 'diredp-do-emacs-command)
  )


;; transpose-frame
;; https://www2.lib.uchicago.edu/keith/emacs/minimacs.html
(use-package transpose-frame :defer t
  :commands
  (transpose-frame
   flip-frame
   flop-frame
   rotate-frame
   rotate-frame-clockwise
   rotate-frame-anticlockwise))


;;;; Modeline:
;; Delight enables you to easily customise how major and minor modes appear in the ModeLine.
;; As per use-package README: delight is invoked with the :delight keyword, which is passed a minor mode symbol, a replacement string or quoted mode-line data (in which case the minor mode symbol is guessed to be the package name with “-mode” appended at the end), both of these, or several lists of both. If no arguments are provided, the default mode name is hidden completely.
(use-package delight
;  :delight (org-indent-mode) ; This belongs in org section
  )
;(elpaca-wait) ; I'm unsure if this is needed


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


;;End
