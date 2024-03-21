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
    :custom
    (org-modern-hide-stars t)		; adds extra indentation
    (org-modern-table t)
    (org-modern-list 
     '(;; (?- . "-")
       (?* . "•")
       (?+ . "‣")))
    ;(org-modern-block-name '("" . "")) ; or other chars; so top bracket is drawn promptly
    :hook
    (org-mode . org-modern-mode)
    (org-agenda-finalize . org-modern-agenda))



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

