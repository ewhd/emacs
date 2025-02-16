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
  (tab-always-indent 'complete)  ;; this basically just has tab doing what C-tab is set to do in the candidate overlay, i.e. completion-at-point

  
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto nil)                  ; disable auto completion so corfu-candidate-overlay can work
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert) ; insert previewed candidate
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
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
  (corfu-popupinfo-mode) ; Popup completion info
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
