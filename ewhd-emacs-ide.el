
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

(use-package combobulate
  :ensure (combobulate :host "github" :repo "mickeynp/combobulate")
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

;;;; Auto-install treesit grammars
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


;; (use-package lsp-mode
;;   :ensure t
;;   :commands (lsp lsp-deferred)
;;   :init
;;   (setq lsp-keymap-prefix "s-l") ;; Or 'C-l', 's-l'
;;   :config
;;   (lsp-enable-which-key-integration t)
;;   :hook
;;   (python-mode . lsp)
;;   )

;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)

;; (use-package lsp-treemacs
;;   :ensure t
;;   :commands lsp-treemacs-errors-list)


;; This compat install is only needed to circumvent a bug where elpaca can't find compat v30 even though it's on elpa
;; First you must rename ~/.config/emacs/elpaca/repos/compat to compat.bak to obscure it, then run this from a freshly started emacs. It'll still throw an error but it will work
;; (use-package compat
;;   :ensure (:host github :repo "emacs-compat/compat" :main "compat.el")
;;   )

;; Completion at point
(use-package corfu
  :ensure t
  ;; :after compat
  ;; Optional customizations
  :custom
  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (read-extended-command-predicate
   #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete) ;; this basically just has tab doing what C-tab is set to do in the candidate overlay, i.e. completion-at-point

  
  (corfu-cycle t)  ;; Enable cycling for `corfu-next/previous'
  (corfu-auto nil) ; disable auto completion so corfu-candidate-overlay can work
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt) ;; Preselect the prompt
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert)       ; insert previewed candidate
  (corfu-on-exact-match nil)            ; Don't auto expand tempel snippets
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-S-SPC"       . corfu-insert-separator)
              ("TAB"           . corfu-next)
              ("S-<backspace>" . corfu-previous)
              ("S-TAB"         . corfu-previous)
              ("S-<return>"    . corfu-insert)
              ("<right>"       . corfu-separator)
              ("RET"           . nil)
	      ("<escape>"      . corfu-quit))

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)                ; Popup completion info
  )

(use-package corfu-candidate-overlay
  :ensure t
  :after corfu
  :config
  ;; enable corfu-candidate-overlay mode globally
  ;; this relies on having corfu-auto set to nil
  (corfu-candidate-overlay-mode +1)
  ;; bind Ctrl + TAB to trigger the completion popup of corfu
  (global-set-key (kbd "C-<tab>") 'completion-at-point)
  ;; bind Ctrl + Shift + Tab to trigger completion of the first candidate
  ;; (keybing <iso-lefttab> may not work for your keyboard model)
  (global-set-key (kbd "C-S-<iso-lefttab>") 'corfu-candidate-overlay-complete-at-point)
  (set-face-attribute 'corfu-candidate-overlay-face nil :foreground "#8E8371")
  )


;; In theory dabbrev and cape go well with corfu, but I don't really understand what they do yet, but here's some maybe code:
(use-package dabbrev
  :ensure nil
  :custom
  (dabbrev-check-other-buffers t)  ;; Search across other buffers
  (dabbrev-case-fold-search nil)  ;; Make case-sensitive
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package cape
  :ensure t
  :config
  ;; Add dabbrev to completion sources
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))


;; Provides integration with external direnv on a per-buffer basis
(use-package envrc
  :ensure t
  ;; :hook (after-init . envrc-global-mode)
  :config
  (envrc-global-mode 1)
  )


(use-package python
  :ensure nil
  :hook ((python-ts-mode . eglot-ensure))
  :mode (("\\.py\\'" . python-ts-mode))
  )
