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
;  (setq org-indent-mode t)
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
    )



(setq
   ;; Edit settings
   ;; org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   ;;org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"
   )


(setq org-modern-hide-stars t)                ; adds extra indentation
(setq org-modern-table t)
(setq org-modern-list
 '(;; (?- . "-")
   (?* . "•")
   (?+ . "‣")))
(org-modern-block-name '("" . "")) ; or other chars; so top bracket is drawn promptly





;;;;;; OTHER ORG-MODE PACKAGES
;;;; Show hidden emphasis markers
(use-package org-appear
  :hook
  (org-mode . org-appear-mode)
  :config
  (setq ;org-appear-autolinks nil
        org-appear-autosubmarkers t
        ;org-appear-delay .7
        )
  )

