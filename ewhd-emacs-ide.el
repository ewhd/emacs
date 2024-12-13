(use-package lsp-mode
  :ensure t
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



