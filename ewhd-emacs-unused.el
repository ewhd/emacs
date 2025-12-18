;; ewhd emacs unused -*- lexical-binding: t; -*-

(use-package perspective
  :ensure nil
  :disable t
  :bind
  ;; ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-x x")) ; pick your own prefix key here
  (persp-state-default-file "~/.cache/emacs/perspective-save")
  (persp-load)
  :init
  (persp-mode)
  :bind
  ("C-x b" . persp-switch-to-buffer*)
  ("C-x p" . persp-switch)
  :config
  ;; previous-buffer and next-buffer can be made Perspective-aware using the
  ;; switch-to-prev-buffer-skip variable as follows:
  (setq switch-to-prev-buffer-skip
        (lambda (win buff bury-or-kill)
          (not (persp-is-current-buffer buff))))

  ;; play nice with consult
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)

  ;; persp-ibuffer
  (add-hook 'ibuffer-hook
            (lambda ()
              (persp-ibuffer-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))
  (with-eval-after-load 'ibuffer
    (require 'ibuf-ext)
    (advice-add #'ibuffer-visit-buffer :override #'ibuffer-visit-buffer-persp)
    (defun ibuffer-visit-buffer-persp (&optional single)
      "Override 'ibuffer-visit-buffer with support perspective."
      (interactive "P")
      (let ((buffer (ibuffer-current-buffer t)))
        (if (bound-and-true-p persp-mode)
            (unless (persp-is-current-buffer buffer)
              (let ((other-persp (persp-buffer-in-other-p buffer)))
                (persp-switch (cdr other-persp)))))
        (switch-to-buffer buffer)
        (when single (delete-other-windows)))))
  )



(use-package persp-mode
  :ensure nil
  :disabled t
  :config
  (setq persp-auto-resume-time 3 ;; No autoload buffers
        persp-set-last-persp-for-new-frames t
        persp-reset-windows-on-nil-window-conf nil
        persp-autokill-buffer-on-remove t
        persp-add-buffer-on-after-change-major-mode nil
        persp-kill-foreign-buffer-behaviour 'kill
        persp-nil-name "main")
  (persp-mode 1)
  ;; (with-eval-after-load "term"
  ;;   (persp-def-auto-persp "term"
  ;;                         :parameters '((dont-save-to-file . t))
  ;;                         :mode 'term-mode
  ;;                         :dyn-env '(after-switch-to-buffer-functions ;; prevent recursion
  ;;                                    (persp-add-buffer-on-find-file nil)
  ;;                                    persp-add-buffer-on-after-change-major-mode)
  ;;                         :hooks '(after-switch-to-buffer-functions)
  ;;                         :switch 'window))

  ;;   (setq persp-show-modestring t)
  ;;   (add-to-list 'mode-line-misc-info
  ;;                ;; '(:eval (format " [%s]" (persp-current-name)))
  ;;                '(:eval (format "[%s]" (string-join (persp-names-current-frame) ",")))
  ;;                )
  ;;   ;; perspective adds ("" (:eval (persp-mode-line))) to global-mode-string,
  ;;   ;; and mode-line-misc-info is set to '((global-mode-string (""
  ;;   ;; global-mode-string))), and telephone-line-misc-info-segment calls up and
  ;;   ;; loads mode-line-misc-string. This might be helpful to implementing
  ;;   ;; modeline support for persp-mode
  )


;; activities
;; This covers why I don't use this:
;; https://github.com/alphapapa/activities.el/issues/46
(use-package activities
  :init
  (activities-mode)
  (activities-tabs-mode)  ; this will activate tab-bar-mode
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)
  :custom
  (activities-bookmark-store t)
  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list))
  :config
  ;; Example: Define an activity for a project
  (activities-define 'notes
                     :setup (lambda ()
                              (find-file "~/Documents/tmpnote.org")
                              (split-window-right)
                              (find-dired "~/Documents/notes")
                              ))
  )


(use-package lsp-mode
  :ensure nil
  :disabled t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "s-l") ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
  :hook
  (python-mode . lsp)
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)



;; must run M-x all-the-icons-install-fonts to install fonts per machine
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))



;; Dired+
;; I've been a little unsure of the proper syntax with elpaca, whether it should be :elpaca or :ensure, but based on this link I'm going with :ensure
;; https://github.com/weirdNox/dotfiles/blob/d9b1da6d60bde008d1b8f95a411b2e3e399dc30c/config/.config/emacs/config.org#diredfl-and-dired
(use-package dired+
  :disabled t
  :ensure nil ;(dired+ :host github :repo "emacsmirror/dired-plus")
  :after dired
  :bind
  ()
  :config
  Additional dired+ specific configurations here
  Hide details by default
  (setq diredp-hide-details-initially-flag t)

  (diredp-toggle-find-file-reuse-dir 1)  ;; start with diredp set to "reuse" buffer

  (define-key dired-mode-map (kbd "S-") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory

  Enable image previews
  (setq diredp-image-preview-in-tooltip 128)
  )


(use-package dired-subtree
  :disabled t
  :ensure t
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)

  (defun ewhd-dired-subtree-toggle ()
    "Toggle the subtree and reflect the dired-omit-mode from the parent Dired buffer."
    (interactive)
    (dired-subtree-toggle)
    ;; Check if dired-omit-mode is enabled in the parent buffer
    (if (and (derived-mode-p 'dired-mode)
             dired-omit-mode)
        (dired-omit-mode 1)  ; Enable dired-omit-mode in the subtree if it's on in the parent
      (dired-omit-mode -1))) ; Disable dired-omit-mode in the subtree if it's off in the parent

  :bind
  ( :map dired-mode-map
    ("<tab>" . ewhd-dired-subtree-toggle)
    ("TAB" . ewhd-dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  )

(use-package dired-preview
  :after dired
  :disabled t
  :ensure t
  ;; :defer 1
  ;; :hook (after-init . dired-preview-global-mode)
  :bind
  ( :map dired-mode-map
    ("P" . dired-preview-mode)
    ("S-<up>" . dired-preview-page-up)
    ("S-<down>" . dired-preview-page-down))
  :config
  (setq dired-preview-max-size (* (expt 2 20) 10))
  (setq dired-preview-delay 0.5)
  (setq dired-preview-ignored-extensions-regexp
        (concat "\\."
                "\\(gz\\|"
                "zst\\|"
                "tar\\|"
                "xz\\|"
                "rar\\|"
                "zip\\|"
                "iso\\|"
                "epub"
                "\\)"))
  (setq dired-preview-display-action-alist
        '((display-buffer-in-side-window)
          ;; (side . bottom)
          ;; (window-height . 0.4)
          (side . right)
          (window-width . 0.5)
          (preserve-size . (t . t))
          (window-parameters . ((mode-line-format . none)
                                (header-line-format . none))))))



(use-package spell-fu
  :ensure nil
  :disabled t
  :config
  (setq spell-fu-word-delimit-camel-case t)

  (add-hook 'org-mode-hook
            (lambda ()
              (setq spell-fu-faces-exclude
                    '(org-block-begin-line
                      org-block-end-line
                      org-code
                      org-date
                      org-drawer org-document-info-keyword
                      org-ellipsis
                      org-link
                      org-meta-line
                      org-properties
                      org-properties-value
                      org-special-keyword
                      org-src
                      org-tag
                      org-verbatim))
              (spell-fu-mode)))

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (spell-fu-mode)))
  )


;; Desktop
;; (setq desktop-dirname "~/.cache"        ; set /before/ enabling desktop mode
;;       desktop-buffers-not-to-save '("*Messages*"
;; 				    "*scratch*"
;; 				    "*Help*"
;; 				    "*info*"
;; 				    "*compilation*"
;; 				    "*eww*")
;;       desktop-path (list desktop-dirname)
;; 					; ensures Emacs uses this path for
;; 					; desktop files -- emacs won't seem to
;; 					; look in desktop-dirname without this
;; 					; line
;;       desktop-auto-save-timeout 10
;;       desktop-save t                    ; always save
;;       )
;; (desktop-save-mode 1)


;; Modeline
;; (setq mode-line-format
;;       '("%e" ;; Error message
;;         ;; mode-line-front-space
;;         " "
;;         (:propertize
;;          (""
;;           mode-line-client
;;           mode-line-modified
;;           mode-line-remote
;;           mode-line-window-dedicated)
;;          display (min-width (5.0))
;;          )
;;         ;; " %b" ;; Buffer name
;;         ;; " (%f)" ;; Full file path in parentheses
;;         (:eval (if (buffer-file-name)
;;                    (format "[%s]" (abbreviate-file-name (buffer-file-name)))
;;                  (format "[%s]" (buffer-name))))
;;         ;; " [%p]" ;; Percentage of buffer scrolled
;;         " [%l:%c]" ;; Line and column number
;;         "%M" ;; Major mode
;;         mode-line-misc-info
;;         ;; " %(" ;; Start of minor modes
;;         ;; (:eval (propertize (mapconcat #'symbol-name minor-mode-list " ") 'face 'mode-line-highlight))
;;         ;; ") " ;; End of minor modes
;;         ;; " %s" ;; Process indicator
;;         (vc-mode vc-mode)
;;         ))


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


(use-package modalka
  :ensure t
  :disabled t
  :config
  ;; Translate regular keybindings
  (modalka-define-kbd "W" "M-w")
  (modalka-define-kbd "Y" "M-y")
  (modalka-define-kbd "a" "C-a")
  (modalka-define-kbd "b" "C-b")
  (modalka-define-kbd "e" "C-e")
  (modalka-define-kbd "f" "C-f")
  (modalka-define-kbd "g" "C-g")
  (modalka-define-kbd "n" "C-n")
  (modalka-define-kbd "p" "C-p")
  (modalka-define-kbd "w" "C-w")
  (modalka-define-kbd "y" "C-y")
  (modalka-define-kbd "K" "C-k")
  (modalka-define-kbd "m" "C-SPC")

  ;; Bind a command directly
  ;; (define-key modalka-mode-map (kbd "Q") #'my-command)

  ;; Bind a prefix key for another keymap
  ;; (define-key modalka-mode-map "x" ctl-x-map)
  ;; (define-key ctl-x-map (kbd "e") #'eval-last-sexp)
  ;; (define-key ctl-x-map (kbd "s") #'save-buffer)

  ;; Activate modalka-mode
  ;; (global-set-key (kbd "<return>") #'modalka-mode)
  ;; (global-set-key (kbd "C-m") #'modalka-mode)

  ;; Enable modalka minor mode everywhere by default (except minibuffer)
  ;; (modalka-global-mode 1)
  
  ;; Disable modalka-mode in these major modes:
  ;; (add-to-list 'modalka-excluded-modes 'magit-status-mode)

  ;; Enable modalka-mode when other certain modes are called:
  ;; (add-hook 'text-mode-hook #'modalka-mode)
  ;; (add-hook 'prog-mode-hook #'modalka-mode)

  ;; Set cursor for visual feedback
  (setq-default cursor-type '(bar . 1)) ; normal
  (setq modalka-cursor-type 'box)       ; modalka-mode
  )


;; I like this package, but it doens't apply to any window which is vertically
;; split. Which isn't /actually/ a problem for my needs? And is maybe actually
;; the right choice? But I'm annoyed I can't control that, so I'm sticking with
;; just using olivetti everywhere.
(use-package perfect-margin
  :ensure t
  :disabled t
  :custom
  (perfect-margin-visible-width 90)  
  ;; :hook (after-init . perfect-margin-mode)
  :config
  ;; enable perfect-mode
  ;; (perfect-margin-mode t)  
  
  ;; auto-center minibuffer windows
  ;; (setq perfect-margin-ignore-filters nil)
  
  ;; auto-center special windows
  ;; (setq perfect-margin-ignore-regexps nil)
  
  ;; add additinal bding on margin area
  ;; (dolist (margin '("<left-margin> " "<right-margin> "))
  ;;   (global-set-key (kbd (concat margin "<mouse-1>")) 'ignore)
  ;;   (global-set-key (kbd (concat margin "<mouse-3>")) 'ignore)
  ;;   (dolist (multiple '("" "double-" "triple-"))
  ;;     (global-set-key (kbd (concat margin "<" multiple "wheel-up>")) 'mwheel-scroll)
  ;;     (global-set-key (kbd (concat margin "<" multiple "wheel-down>")) 'mwheel-scroll)))
  )
