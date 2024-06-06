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
   org-startup-folded t
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   org-hide-emphasis-markers t
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
   org-duration-format 'h:mm
   org-clock-report-include-clocking-task t
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
    (setq global-org-modern-mode t)
    )

(setq org-modern-hide-stars t)                ; adds extra indentation
(setq org-modern-table t)
(setq org-modern-list
 '(;; (?- . "-")
   (?* . "•")
   (?+ . "‣")))
(setq org-modern-block-name '("" . "")) ; or other chars; so top bracket is drawn promptly



;;;; ORG-MODE GENERAL BEHAVIOR
;; I think this is all included in the use-package block above, and should probably be removed
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


;;;; ORG TODO
;; global effort estimates:
(customize-set-variable 'org-global-properties
                        '(("Effort_ALL" . "0:00 0:07 0:15 0:30 0:45 1:00 1:30 2:00 2:30 3:00")))

;; org TODO keywords:
(setq org-todo-keywords
 '((sequence
    "TODO(t)"
    "NEXT(n)"
    "STARTED(s)"
    "WAITING(w@/!)"
    "REVIEW(r!)"
    "HOLD(h@/!)"
    "|"
    "CANCELED(x)"
    "DONE(d)"
    )
   (sequence
    "OPEN(o)"
    "EVENT(e)"
    "|"
    "COMPLETE(c)")
   (sequence
    "PROJECT - OPEN(p)"
    "PROJECT - ON-HOLD(@/)"
    "|"
    "FINISHED(f)"
    )
   )
 )
(setq org-todo-keyword-faces
  '(("TODO". "purple")
    ("NEXT" . "magenta")
    ("STARTED" . "pink")
    ("WAITING" . "blue")
    ("REVIEW" . "orange")
    ("HOLD" . "cyan")
    ("CANCELED" . "green")
    ("DONE" . "green")
    ("OPEN" . (:foreground "white" :background "purple"))
    ("EVENT" . (:foreground "white" :background "blue"))
    ("COMPLETE" . (:foreground "white" :background "dark green"))
    ("PROJECT - OPEN" . (:foreground "red" :weight bold))
    ("PROJECT - ON-HOLD" . (:foreground "cyan" :weight bold))
    ("FINISHED" . (:foreground "green" :weight bold))))


;;;; ORG-AGENDA
(global-set-key "\C-ca" 'org-agenda)

;; this snippet recursively searches note and  adds any org file which contains gtd as a keyword to org-agenda-files
;; the regex here should exclude anything starting with a '.', ending with a '~' or a '#'
(setq org-agenda-files
      (seq-filter
       'file-exists-p
       (directory-files-recursively "~/Documents" "^[^\.#].*_gtd.*\\.org$")))




;; org-agenda-mode behavior
(setq org-agenda-window-setup 'current-window  ; agenda takes current window
      org-agenda-restore-windows-after-quit t  ; restore window configuration on exit
      org-agenda-start-with-follow-mode nil
      org-columns-default-format-for-agenda "%25ITEM %4TODO %1PRIORITY %4Effort(Estim){:}  %4CLOCKSUM(Clock) %20ALLTAGS"
      org-agenda-clockreport-parameter-plist '(:link t :maxlevel 5) ; set the depth of headers referenced by org-agenda-clockreport-mode
      org-agenda-time-grid '(
			     (daily today require-timed)
			     (600 900 1200 1500 1800 2100)
			     "......" "----------------------" nil)
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-include-diary t
      org-agenda-block-separator 9472
      org-agenda-compact-blocks nil
      org-agenda-start-with-log-mode t
      org-agenda-hide-tags-regexp (regexp-opt '("cf" "gtd"))  ;; hides specific tags
      org-agenda-remove-tags t  ;; hides all tags
      )
(add-hook 'org-agenda-mode-hook
	  ;; Disables word-wrap and enables truncate-line in agenda buffers
          (lambda ()
            (visual-line-mode -1)
            (toggle-truncate-lines 1)))

;; Formatting (truncating) fields in agenda-view:
(defvar ewhd-org-agenda-extended-prefix t)
(defun ewhd-org-agenda-toggle-extended-prefix ()
  "Toggles whether category/file name appears or not at the left
  of entries in agenda listings. Useful to unclutter listings."
  (interactive)
  (if ewhd-org-agenda-extended-prefix
      (progn 
        (setq ewhd-org-agenda-extended-prefix nil)
        (setq org-agenda-prefix-format
              '((agenda  . "  %?-12t% s")
                (timeline  . "  % s")
                (todo  . "  ")
                (tags  . "  ")
                (search . "  ")))
        )
    (setq ewhd-org-agenda-extended-prefix t)
    (setq org-agenda-prefix-format
          '((agenda  . "  %-12:c%?-12t% s")
            (timeline  . "  % s")
            (todo  . "  %-12:c")
            (tags  . "  %-12:c")
            (search . "  %-12:c")))
    
    )
  (org-agenda-redo))

;; hide extended prefix info by default
(setq ewhd-org-agenda-extended-prefix nil)
(setq org-agenda-prefix-format
      '((agenda  . "  %?-12t% s")
        (timeline  . "  % s")
        (todo  . "  ")
        (tags  . "  ")
        (search . "  ")))

(add-hook 
 'org-mode-hook
 (lambda ()
   (define-key org-agenda-keymap   "L" 'ewhd-org-agenda-toggle-extended-prefix)
   (define-key org-agenda-mode-map "L" 'ewhd-org-agenda-toggle-extended-prefix)
   ))


;;;;;; OTHER ORG-MODE PACKAGES
;;;; organize org agenda better
(use-package org-super-agenda
    :ensure t
    :after org
    :config
    (org-super-agenda-mode)
    (setq org-agenda-custom-commands
          '(("z" "Super view"
             (
	      (agenda "" ((org-agenda-span 'day)
                          (org-super-agenda-groups
                           '(
			     (:name "Today"
                                    :time-grid t
                                    :date today
                                    :scheduled today
				    :deadline today
                                    :order 1)
			     (:discard (:anything t))
			     ))))
	      ;; (agenda "" ((org-agenda-start-on-weekend nil)
	      ;; 		  (org-agenda-span 7)
	      ;; 		  (org-agenda-start-day "+1d")
              ;;             (org-super-agenda-groups
              ;;              '(
	      ;; 		     (:name "This Week"
	      ;; 			    :time-grid nil
              ;;                       :scheduled future :not (:scheduled today)
	      ;; 			    :deadline future :not (:deadline today)
              ;;                       :order 2)
	      ;; 		     (:discard (:anything t))
	      ;; 		     ))))
              (alltodo "" ((org-agenda-overriding-header "")
			   (org-agenda-span 7)
                           (org-super-agenda-groups
                            '(
                              (:name "Due Within 7 Days"
                                     :deadline future
				     :scheduled future
                                     :order 100)
                              (:name "Overdue"
                                     :deadline past
                                     :order 10)
			      (:name "Behind Schedule"
				     :scheduled past
				     :order 20)
			      (:todo "NEXT" :priority>= "B" :order 60)
			      (:todo "STARTED" :order 70)
                              (:name "Someday"
                                     :priority<= "C"
                                     :order 200)
			      (:todo "TODO" :order 80)
                              (:name "Misc."
				     :todo ("REVIEW" "WAITING" "HOLD" )
                                     :order 90)
                              (:todo "OPEN" :order 150)
			      ;(:discard (:tag "PROJECT"))
                              ))))
	      ))
	    ("wd" "Work Day"
             (
	      (agenda "" ((org-agenda-span 'day)
                          (org-super-agenda-groups
                           '(
			     (:name "Today"
                                    :time-grid t
                                    :date today
                                    :scheduled today
				    :deadline today
                                    :order 1)
			     (:discard (:anything t))
			     ))))
              (tags-todo "cf" ((org-agenda-overriding-header "")
			   (org-agenda-span 7)
                           (org-super-agenda-groups
                            '(
                              (:name "Due Within 7 Days"
                                     :deadline future
				     :scheduled future
                                     :order 100)
                              (:name "Overdue"
                                     :deadline past
                                     :order 10)
			      (:name "Behind Schedule"
				     :scheduled past
				     :order 20)
			      (:todo "NEXT" :priority>= "B" :order 60)
			      (:todo "STARTED" :order 70)
                              (:name "Someday"
                                     :priority<= "C"
                                     :order 200)
			      (:todo "TODO" :order 80)
                              (:name "Misc."
				     :todo ("REVIEW" "WAITING" "HOLD" )
                                     :order 90)
                              (:todo "OPEN" :order 150)
			      ;(:discard (:tag "PROJECT"))
                              ))))
	      ))
	    ("ww" "Work Week"
             (
	      (agenda "" ((org-agenda-start-on-weekend nil)
	      		  ;; (org-agenda-span 7)
	      		  ;; (org-agenda-start-day "+1d")
                          (org-super-agenda-groups
                           '(
	      		     (:name "This Week"
	      			    :time-grid t
                                    :scheduled future
	      			    :deadline future)
	      		     (:discard (:anything t))
	      		     ))))
	      ))
	    )
    )
)

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

