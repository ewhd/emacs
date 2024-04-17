;; ewhd emacs org mode related packages -*- lexical-binding: t; -*-


;;;;;; ORG-MODE
(use-package org
  :ensure nil
  :bind (("C-c l" . org-store-link)
         ("C-S-v" . scroll-other-window)
         ("M-V" . scroll-other-window-down)
	 ("C-c ." . org-time-stamp)
	 ("C-S-l" . org-toggle-link-display)
         )
  :config
  (setq
   org-startup-indented t
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   ;;org-hide-emphasis-markers t
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
   )
)


(use-package org-modern
    :ensure t
    ;; :custom
    ;; (org-modern-hide-stars t)		; adds extra indentation
    ;; (org-modern-table t)
    ;; (org-modern-list 
    ;;  '(;; (?- . "-")
    ;;    (?* . "•")
    ;;    (?+ . "‣")))
    ;; ;(org-modern-block-name '("" . "")) ; or other chars; so top bracket is drawn promptly
    ;; :hook
    ;; (org-mode . org-modern-mode)
    ;; (org-agenda-finalize . org-modern-agenda)
    :config
    (setq org-modern-mode t)
    )

(setq org-modern-hide-stars t)                ; adds extra indentation
(setq org-modern-table t)
(setq org-modern-list
 '(;; (?- . "-")
   (?* . "•")
   (?+ . "‣")))
(org-modern-block-name '("" . "")) ; or other chars; so top bracket is drawn promptly



;;;; ORG-MODE GENERAL BEHAVIOR
(setq
   ;; Edit settings
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   ;;org-hide-emphasis-markers t
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
   )








;;;;;; OTHER ORG-MODE PACKAGES
;;;; Show hidden emphasis markers
(use-package org-appear
  :ensure t
  :hook
  (org-mode . org-appear-mode)
  :config
  (setq
   org-appear-autolinks nil
   org-appear-autosubmarkers t
   ;org-appear-delay .7
   )
  )

