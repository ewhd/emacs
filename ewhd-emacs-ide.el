;;; ewhd-emacs-ide.el --- coding related packages
;; -*- lexical-binding: t; -*-

;;; Treesit
(use-package treesit
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             ;; Note the version numbers. These are the versions that
             ;; are known to work with Combobulate *and* Emacs.
             '(
               ;; (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
               ;; (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               ;; (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               ;; (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               ;; (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               ;; (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional. Combobulate works in both xxxx-ts-modes and
  ;; non-ts-modes.

  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js2-mode . js-ts-mode)
             (bash-mode . bash-ts-mode)
             (conf-toml-mode . toml-ts-mode)
             (go-mode . go-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  ;; (mp-setup-install-grammars)  ;
  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
  ;;
  )

;; Auto-install treesit grammars
(use-package treesit-auto
  :ensure t
  :requires treesit
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  ;; (setq (
  ;;        ;; (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
  ;;        (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
  ;;        ;; (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
  ;;        ;; (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
  ;;        (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
  ;;        (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
  ;;        (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
  ;;        ;; (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
  ;;        (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
  ;;        ;; (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
  ;;        ;; (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
  ;;        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
  )



;; This compat install is only needed to circumvent a bug where elpaca can't
;; find compat v30 even though it's on elpa First you must rename
;; ~/.config/emacs/elpaca/repos/compat to compat.bak to obscure it, then run
;; this from a freshly started emacs. It'll still throw an error but it will
;; work (use-package compat :ensure (:host github :repo "emacs-compat/compat"
;; :main "compat.el") )


;;; Code Tranversal
(use-package combobulate
  ;; enhances buffer editing by intelligently reorganizing, formatting, or
  ;; annotating text to keep it tidy and structured
  :ensure (combobulate :host "github.com" :repo "mickeynp/combobulate")
  :requires treesit
  :custom
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (combobulate-key-prefix "C-c o")
  :hook ((prog-mode . combobulate-mode))
  ;; Amend this to the directory where you keep Combobulate's source
  ;; code.
  ;; :load-path ("path-to-git-checkout-of-combobulate")
  )


(use-package expand-region
  :ensure t
  :bind (("C-'" . er/expand-region)
	       ("C-\"" . er/mark-outside-quotes)
	       ))


;;; Syntax Enforcement
(use-package aggressive-indent
  ;; Automatically re-indents your code as you type.
  :hook ((clojure-mode . aggressive-indent-mode)
	       (emacs-lisp-mode . aggressive-indent-mode)
	       (css-mode . aggressive-indent-mode))
  )

(use-package paredit
  ;; Automatically maintaining balanced parentheses and proper S-expression
  ;; structure
  :ensure t
  :hook ((emacs-lisp-mode
          lisp-mode
          lisp-interaction-mode
          ielm-mode
          ;; eval-expression-minibuffer-setup ;; interferes with eval-expression in the minibuffer
          slime-repl-mode) . enable-paredit-mode)
  :bind
  (:map paredit-mode-map
        ("C-M-<left>" . paredit-backward)
        ("C-M-<right>" . paredit-forward))
  :config
  ;; Override SLIME REPL backward-delete to play nicely with paredit
  (defun ewhd-override-slime-del-key ()
    (define-key slime-repl-mode-map
	              (read-kbd-macro paredit-backward-delete-key) nil))
  (add-hook 'slime-repl-mode-hook #'ewhd-override-slime-del-key)
  (with-eval-after-load 'paredit
    (define-key paredit-mode-map (kbd "C-<left>") nil)
    (define-key paredit-mode-map (kbd "C-<right>") nil)
    )
  )


(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode
          lisp-mode
          lisp-interaction-mode
          ielm-mode
          slime-repl-mode) . rainbow-delimiters-mode)
  :config
  ;; Customize colors
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#c66") ; red
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6") ; green
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#69f") ; blue
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6") ; yellow
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc") ; cyan
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c") ; magenta
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc") ; light gray
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#999") ; medium gray
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#666") ; dark gray
  )


;;; Dependencies
(use-package envrc
  ;; Emacs integrates with direnv, a tool that automatically sets environment
  ;; variables based on the current directory.
  :ensure t
  ;; :hook (after-init . envrc-global-mode)
  :config
  (envrc-global-mode 1)
  )


;;; Common Lisp
(use-package slime
  ;; REPL for Common Lisp
  ;; Must install sbcl on host system
  :ensure t
  :init
  ;; Set SBCL as the Lisp program
  (add-to-list 'exec-path "/usr/local/bin")
  (setq inferior-lisp-program "sbcl")
  :config
  ;; Optional: additional SLIME configuration can go here
  )


;;; Python

;; requires: pip install python-lsp-server pycodestyle pyflakes mccabe more
;; thorough: pip install python-lsp-server pylint pylsp-mypy python-lsp-black
;; pycodestyle pyflakes mccabe

(use-package python
  :ensure nil
  :hook ((python-ts-mode . eglot-ensure))
  :mode (("\\.py\\'" . python-ts-mode))
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "-i --simple-prompt")
  (python-shell-completion-setup-code nil)
  )

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(provide 'ewhd-emacs-ide)

;;; ewhd-emacs-ide.el ends here
