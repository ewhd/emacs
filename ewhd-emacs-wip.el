;; ewhd emacs work in progress -*- lexical-binding: t; -*-

(use-package casual

  :ensure t
  :bind (
         :map dired-mode-map
         ("C-c ?" . casual-dired-tmenu)
         ("s"     . casual-dired-sort-by-tmenu)
         ("/"     . casual-dired-search-replace-tmenu)
         :map emacs-lisp-mode-map
         ("C-c ?" . casual-elisp-tmenu)
         :map help-mode-map
         ("C-c ?" . casual-help-tmenu)
         ;; :map ibuffer-mode-map
         ;; ("C-c ?" . casual-ibuffer-tmenu)
         ;; ("F" . casual-ibuffer-filter-tmenu)
         ;; ("s" . casual-ibuffer-sortby-tmenu)
         ;; :map Man-mode-map
         ;; ("C-c ?" . casual-man-tmenu)
         ;; ("n" . casual-lib-browse-forward-paragraph)
         ;; ("p" . casual-lib-browse-backward-paragraph)
         ;; ("[" . Man-previous-section)
         ;; ("]" . Man-next-section)
         ;; ("j" . next-line)
         ;; ("k" . previous-line)
         ;; ("K" . Man-kill)
         ;; ("o" . casual-man-occur-optsions)
         )
  :hook
  (ibuffer-mode . ewhd-ibuffer-keys)
  :config
  ;; dired ediff config
  (casual-ediff-install) ; run this to enable Casual Ediff
  (add-hook 'ediff-keymap-setup-hook
            (lambda ()
              (keymap-set ediff-mode-map "C-c ?" #'casual-ediff-tmenu)))

  ;; keymap load hooked to ibuffer, since ibuffer-mode-map doesn't exist until
  ;; ibuffer is loaded for the first time
  (defun ewhd-ibuffer-keys ()
    (keymap-set ibuffer-mode-map "C-c ?" #'casual-ibuffer-tmenu)
    (keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu)
    (keymap-set ibuffer-mode-map "s" #'casual-ibuffer-sortby-tmenu)
    )
  )



(defun ewhd-window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))



;; (use-package project
;;   :bind-keymap ("C-c p" . project-prefix-map)
;;   ;; Other project.el configurations...
;;   )

;; Unbind the old prefix
(global-unset-key (kbd "C-x p"))

;; Bind the new prefix
(define-key global-map (kbd "C-c p") project-prefix-map)

(use-package perspective
  :ensure t
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



;; (use-package persp-mode
;;   :ensure t
;;   :config
;;   (setq persp-auto-resume-time 3 ;; No autoload buffers
;;         persp-set-last-persp-for-new-frames t
;;         persp-reset-windows-on-nil-window-conf nil
;;         persp-autokill-buffer-on-remove t
;;         persp-add-buffer-on-after-change-major-mode nil
;;         persp-kill-foreign-buffer-behaviour 'kill
;;         persp-nil-name "main")
;;   (persp-mode 1)
;;   ;; (with-eval-after-load "term"
;;   ;;   (persp-def-auto-persp "term"
;;   ;;                         :parameters '((dont-save-to-file . t))
;;   ;;                         :mode 'term-mode
;;   ;;                         :dyn-env '(after-switch-to-buffer-functions ;; prevent recursion
;;   ;;                                    (persp-add-buffer-on-find-file nil)
;;   ;;                                    persp-add-buffer-on-after-change-major-mode)
;;   ;;                         :hooks '(after-switch-to-buffer-functions)
;;   ;;                         :switch 'window))

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
;;   )




(use-package tabspaces
  :ensure t
  :init
  (setq tab-bar-show t) ; needed to make tabs-bar visible, I think?
  (setq tabspaces-keymap-prefix (kbd "C-x x"))
  (tabspaces-mode 1)
  :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
  ;; :commands (tabspaces-switch-or-create-workspace
  ;;            tabspaces-open-or-create-project-and-workspace)

  ;; (use-package tabspaces
  ;;   :ensure t
  ;;   :init (setq tabspaces-keymap-prefix (kbd "C-x x"))
  ;;   :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
  ;;   ;; :commands (tabspaces-switch-or-create-workspace
  ;;   ;; tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-initialize-project-with-todo t)
  (tabspaces-todo-file-name "project-todo.org")
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)
  ;; additional options
  (tabspaces-fully-resolve-paths t) ; Resolve relative project paths to absolute
  (tabspaces-exclude-buffers '("*Messages*" "*Compile-Log*")) ; Additional buffers to exclude
  (tab-bar-new-tab-choice "*scratch*")
  :config
  (defvar tabspaces-command-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C") 'tabspaces-clear-buffers)
      ;; (define-key map (kbd "b") 'tabspaces-switch-to-buffer)
      (define-key map (kbd "d") 'tabspaces-close-workspace)
      (define-key map (kbd "k") 'tabspaces-kill-buffers-close-workspace)
      (define-key map (kbd "o") 'tabspaces-open-or-create-project-and-workspace)
      (define-key map (kbd "r") 'tabspaces-remove-current-buffer)
      (define-key map (kbd "R") 'tabspaces-remove-selected-buffer)
      (define-key map (kbd "s") 'tabspaces-switch-or-create-workspace)
      (define-key map (kbd "t") 'tabspaces-switch-buffer-and-tab)
      (define-key map (kbd "w") 'tabspaces-show-workspaces)
      (define-key map (kbd "T") 'tabspaces-toggle-echo-area-display)
      map)
    "Keymap for tabspace/workspace commands after `tabspaces-keymap-prefix'.")
  )
