;;; Customized from  Emacs4CL 0.5.0
;;; <https://github.com/susam/emacs4cl>

;; Must install sbcl on host system

;; SLIME
(use-package slime
  :ensure t
  :init
  ;; Set SBCL as the Lisp program
  (add-to-list 'exec-path "/usr/local/bin")
  (setq inferior-lisp-program "sbcl")
  :config
  ;; Optional: additional SLIME configuration can go here
  )

;; Paredit
(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode
          lisp-mode
          lisp-interaction-mode
          ielm-mode
          ;; eval-expression-minibuffer-setup ;; interferes with eval-expression in the minibuffer
          slime-repl-mode) . enable-paredit-mode)
  :config
  ;; Override SLIME REPL backward-delete to play nicely with paredit
  (defun ewhd-override-slime-del-key ()
    (define-key slime-repl-mode-map
		(read-kbd-macro paredit-backward-delete-key) nil))
  (add-hook 'slime-repl-mode-hook #'ewhd-override-slime-del-key))

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode
          lisp-mode
          lisp-interaction-mode
          ielm-mode
          slime-repl-mode) . rainbow-delimiters-mode)
  :config
  ;; Customize colors
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#666") ; dark gray
  )
