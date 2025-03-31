;; ewhd emacs org mode related packages -*- lexical-binding: t; -*-


;;;;;; ORG-MODE
(use-package org
  :ensure nil
  :init
  (require 'org-agenda)
  :bind (("C-c l" . org-store-link)
         ("C-S-v" . scroll-other-window)
	 ("C-c ." . org-time-stamp)
	 ("C-S-l" . org-toggle-link-display)
	 ("C-x S" . org-save-all-org-buffers)
	 ("C-c C-x C-s" . org-archive-to-archive-sibling)
	 )
  :config
  (define-key org-mode-map (kbd "C-'") nil)
					; Unbind C-' in org-mode so that it
					; doesn't conflict with expand-region
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
   org-src-window-setup 'current-window ; edit in current window
   org-src-strip-leading-and-trailing-blank-lines t
   org-src-preserve-indentation t       ; do not put two spaces on the left
   org-src-tab-acts-natively t
   org-log-into-drawer t
   org-startup-with-inline-images t     ; only displays in the format
					; [[file:path-to-file]], nothing else.
   org-image-actual-width '(300)
   org-duration-format 'h:mm
   org-clock-report-include-clocking-task t
   org-clock-out-remove-zero-time-clocks t
   org-todo-repeat-to-state t           ; Repeat to previous state
   )
  )

(use-package org-tempo
  :ensure nil
  :config
  (with-eval-after-load 'org-tempo
    (add-to-list 'org-structure-template-alist '("se" . "src elisp"))
    (add-to-list 'org-structure-template-alist '("sb" . "src bash"))
    (add-to-list 'org-structure-template-alist '("sp" . "src python"))
    ))

;;;; OTHER ORG-MODE PACKAGES
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
(setq org-modern-block-name '("" . "")) ; or other chars; so top bracket is
					; drawn promptly


(use-package org-super-agenda
  :ensure t
  :after org
  :init
  (org-super-agenda-mode 1))

(use-package org-ql
  :ensure t
  :after org)

;; Show hidden emphasis markers
(use-package org-appear
  :ensure t
  :hook
  (org-mode . org-appear-mode)
  :config
  (setq
   org-appear-autolinks nil
   org-appear-autosubmarkers t
   ;; org-appear-delay .7
   )
  )

;;;; ORG-MODE GENERAL BEHAVIOR
;; I think this is all included in the use-package block above, and should
;; probably be removed
(setq
 ;; Edit settings
 org-fold-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t
 ;;org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"
 org-src-fontify-natively t
 org-src-window-setup 'current-window   ; edit in current window
 org-src-strip-leading-and-trailing-blank-lines t
 org-src-preserve-indentation t         ; do not put two spaces on the left
 org-src-tab-acts-natively t
 org-log-into-drawer t
 org-startup-with-inline-images t       ; only displays in the format
					; [[file:path-to-file]], nothing else.
 org-image-actual-width '(300)
 )


;;;; ORG REFILE
;; Set list of legitimate refile targets
;; (setq org-refile-targets
;;       (mapcar (lambda (file) (cons file '(:maxlevel . 2)))
;;               (append org-agenda-files
;;                       '("~/Documents/notes/20240504T125856--backburner.org"
;;                         "~/Documents/notes/20240504T125919--someday-maybe.org"))))

;; Use all org files as targets for refile, and refresh the list of refile
;; targets each time org-refile is run
(defun ewhd-org-refile-targets (&rest _)
  "Return a list of all .org files in the notes directory and subdirectories."
  (seq-filter
   (lambda (file)
     (not (string-match-p "/\\.#" file)))  ; Ignore files starting with .#
   (directory-files-recursively "~/Documents/notes" "\\.org$")))

(defun ewhd-refresh-refile-targets (&rest _)
  "Refresh org-refile-targets before running org-refile."
  (setq org-refile-targets
        `((,(ewhd-org-refile-targets) . (:maxlevel . 3)))))

(advice-add 'org-refile :before #'ewhd-refresh-refile-targets)

;; Allow refiling to the top (file) level, rather than just to headings (which
;; is the default)
(setq org-refile-use-outline-path 'file)

;; setting org-refile-use-outline-path to 'file causes problems with
;; auto-complete that prevents designating headings within the file. This fixes
;; that.
(setq org-outline-path-complete-in-steps nil)



;;;; ORG TODO
;; global effort estimates:
(customize-set-variable 'org-global-properties
                        '(("Effort_ALL" . "0:00 0:07 0:15 0:30 0:45 1:00 1:30 2:00 2:30 3:00")))

;; org TODO keywords:
(setq org-todo-keywords
      '((sequence
	 "IDEA(i)"    ; Just an idea, which could become anything!
	 "TASK(a)"    ; A task without defined dependencies or prep/steps yet
         "TODO(t)"    ; Task components like dependencies and prep/steps have
					; been defined
         "PREP(p)"    ; Prep steps started but unfinished
         "NEXT(n)"    ; All prep done and dependencies resolved: ready for
					; action steps
         "STRT(s)"    ; Action steps started but unfinished
	 "SCHD(S)"    ; No further action can be taken until some time
         "WAIT(W@/!)" ; Waiting on some external factor
         "DPND(D@)"   ; task is blocked by another, separate task which must be
					; complete
         "REVW(r!)"
         ;; "HOLD(h@/!)"
         "|"
         "CANC(x)"
         "DONE(d)"
	 )
	(sequence
         "OPEN(o)"
         "EVNT(e)"
         "|"
         "COMP(c)")
        (sequence
         "PROJ(P)"
         ;; "PROJECT - ON-HOLD(@/)"
         "|"
         "FNSH(f)"
	 )
	)
      )

;; available named colors: https://www.raebear.net/computers/emacs-colors/
(setq org-todo-keyword-faces
      '(("IDEA" . "sienna1")
	("TASK" . "plum1")
	("TODO" . "purple")
	("PREP" . "orchid")
	("NEXT" . "magenta")
	("STRT" . "hot pink")
	("WAIT" . "sky blue")
	("SCHD" . "DeepSkyBlue2")
	("DPND" . "cyan")
	("REVW" . "orange")
	;; ("HOLD" . "cyan")
	("CANC" . "green")
	("DONE" . "green")
	("OPEN" . (:foreground "white" :background "purple"))
	("EVNT" . (:foreground "white" :background "blue"))
	("COMP" . (:foreground "white" :background "dark green"))
	("PROJ" . (:foreground "red" :weight bold))
	;; ("PROJECT - ON-HOLD" . (:foreground "cyan" :weight bold))
	("FNSH" . (:foreground "green" :weight bold))))


;;;; ORG TAGS
;; suggest tags from all agenda files:
(setq org-complete-tags-always-offer-all-agenda-tags t)

;; available named colors: https://www.raebear.net/computers/emacs-colors/
(setq org-tag-faces
      '(("Urg"   . "dark orange")
        ("Imp"   . "orange red")
	("Prcs"  . "turquoise")
	("Imrsv" . "dodger blue")
	("Fnsh"  . "lawn green")
	("Cmplx" . "dark violet")
	("Dstr"  . "yellow2")
	("Dlybl" . "wheat")
	))



;; set colors in column view
(defun ewhd-org-add-tag-color-in-column-view ()
  (font-lock-add-keywords
   nil
   '(("\\(:Urg:\\)"   1 '(:foreground "dark orange" :weight bold) t)
     ("\\(:Imp:\\)"   1 '(:foreground "orange red" :weight bold) t)
     ("\\(:Prcs:\\)"  1 '(:foreground "turquoise" :weight bold) t)
     ("\\(:Imrsv:\\)" 1 '(:foreground "dodger blue" :weight bold) t)
     ("\\(:Fnsh:\\)"  1 '(:foreground "lawn green" :weight bold) t)
     ("\\(:Cmplx:\\)" 1 '(:foreground "dark violet" :weight bold) t)
     ("\\(:Dstr:\\)"  1 '(:foreground "yellow2" :weight bold) t)
     ("\\(:Dlybl:\\)" 1 '(:foreground "wheat" :weight bold) t))
					; change Dlybl -- it's a little too
					; close to the default off-white
   t))

(add-hook 'org-mode-hook #'ewhd-org-add-tag-color-in-column-view)


;;;; ORG PRIORITY
;; Define priorities
(setq org-highest-priority ?A
      org-default-priority ?D
      org-lowest-priority ?D)

;; Set colors
;; available named colors: https://www.raebear.net/computers/emacs-colors/
(setq org-priority-faces '((?A . "red")
                           (?B . "tomato")
                           (?C . "orange")
			   (?D . "white")
			   ))


;;;; ORG-AGENDA

(global-set-key "\C-ca" 'org-agenda)
(define-key org-agenda-mode-map (kbd "C-z") 'org-agenda-undo)
(define-key org-agenda-mode-map (kbd "M-e") 'org-agenda-set-effort)

;; this function recursively searches ~/Documents and adds any org file which
;; contains gtd as a keyword to org-agenda-files the regex here should exclude
;; anything starting with a '.', ending with a '~' or a '#'
(defun ewhd-refresh-org-agenda-files ()
  "Refresh the list of `org-agenda-files` dynamically."
  (setq org-agenda-files
        (seq-filter
         'file-exists-p
         (directory-files-recursively "~/Documents" "^[^\.#].*_gtd.*\\.org$"))))

;; run ewhd-refresh-org-agenda-files whenever agenda-mode is called:
(add-hook 'org-agenda-mode-hook 'ewhd-refresh-org-agenda-files)
(ewhd-refresh-org-agenda-files)


;; org-agenda-mode behavior
(setq org-agenda-window-setup 'current-window  ; agenda takes current window
      org-agenda-restore-windows-after-quit t
					; restore window configuration on exit
      org-agenda-start-with-follow-mode nil
      org-columns-default-format-for-agenda "%45ITEM %4TODO %1PRIORITY %4Effort(Estim){:}  %4CLOCKSUM(Clock) %38ALLTAGS"
      org-agenda-clockreport-parameter-plist '(:link t :maxlevel 5)
					; set the depth of headers referenced by
					; org-agenda-clockreport-mode
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
      org-agenda-hide-tags-regexp (regexp-opt '("cf"
						"gtd"
						"sys"
						"DFR"
						"NOW"
						"WK"
						"MTH"
						"Q1"
						"Q2"
						"Q3"
						"Q4"
						))
					; hides specific tags
      org-agenda-remove-tags nil        ; t = hides all tags
      org-agenda-show-inherited-tags t
      org-tags-exclude-from-inheritance '("gtd" "Proj" "Cmplx" "Fnsh" "Dstr" "Dlybl" "Prcs" "Imrsv")
      ;; org-agenda-compact-blocks t
      org-agenda-timegrid-use-ampm t
      org-agenda-confirm-kill t
      )

(add-hook 'org-agenda-mode-hook
	  ;; Disables word-wrap and enables truncate-line in agenda buffers
          (lambda ()
            (visual-line-mode -1)
            (toggle-truncate-lines 1)))


;; Formatting (truncating) fields in agenda-view:
;; (defvar ewhd-org-agenda-extended-prefix t)
;; (defun ewhd-org-agenda-toggle-extended-prefix ()
;;   "Toggles whether category/file name appears or not at the left
;;   of entries in agenda listings. Useful to unclutter listings."
;;   (interactive)
;;   (if ewhd-org-agenda-extended-prefix
;;       (progn 
;;         (setq ewhd-org-agenda-extended-prefix nil)
;;         (setq org-agenda-prefix-format
;;               '((agenda  . "  %?-12t% s")
;;                 (timeline  . "  % s")
;;                 (todo  . "  ")
;;                 (tags  . "  ")
;;                 (search . "  ")))
;;         )
;;     (setq ewhd-org-agenda-extended-prefix t)
;;     (setq org-agenda-prefix-format
;;           '((agenda  . "  %-12:c%?-12t% s")
;;             (timeline  . "  % s")
;;             (todo  . "  %-12:c")
;;             (tags  . "  %-12:c")
;;             (search . "  %-12:c")))

;;     )
;;   (org-agenda-redo))

;; ;; hide extended prefix info by default
;; (setq ewhd-org-agenda-extended-prefix nil)
;; (setq org-agenda-prefix-format
;;       '((agenda  . "  %?-12t% s")
;;         (timeline  . "  % s")
;;         (todo  . "  ")
;;         (tags  . "  ")
;;         (search . "  ")))

;; (add-hook 
;;  'org-mode-hook
;;  (lambda ()
;;    (define-key org-agenda-keymap   "L" 'ewhd-org-agenda-toggle-extended-prefix)
;;    (define-key org-agenda-mode-map "L" 'ewhd-org-agenda-toggle-extended-prefix)
;;    ))

(setq ewhd-org-agenda-prefix-formats
      '(((agenda   . " %?s%-12t")       ; Format 1
         (timeline . " %s")
         (todo     . " ")
         (tags     . " ")
         (search   . " "))
        ((agenda   . " %-12:c%-s%-12t") ; Format 2
         (timeline . " %s")
         (todo     . " %-12:c")
         (tags     . " %-12:c")
         (search   . " %-12:c"))
        ((agenda   . " %?s%-12t")       ; Format 3
         (timeline . " %s")
         (todo     . " %-5e")
         (tags     . " %i [TAG] %s")
         (search   . " "))
	))

(defun set-org-agenda-prefix-format (n)
  "Set `org-agenda-prefix-format` to the Nth format in `ewhd-org-agenda-prefix-formats`."
  (interactive "nChoose prefix format (1, 2, or 3): ")
  (let ((format (nth (1- n) ewhd-org-agenda-prefix-formats)))
    (if format
        (progn
          (setq org-agenda-prefix-format format)
          (message "Using org-agenda-prefix-format %d" n)
          (when (get-buffer "*Org Agenda*")
            (org-agenda-redo)))
      (message "Invalid format number"))))
;; (global-unset-key (kbd "L"))
(define-prefix-command 'org-agenda-prefix-format-map)
(define-key org-agenda-mode-map (kbd "L") 'org-agenda-prefix-format-map)
(define-key org-agenda-prefix-format-map (kbd "1") (lambda () (interactive) (set-org-agenda-prefix-format 1)))
(define-key org-agenda-prefix-format-map (kbd "2") (lambda () (interactive) (set-org-agenda-prefix-format 2)))
(define-key org-agenda-prefix-format-map (kbd "3") (lambda () (interactive) (set-org-agenda-prefix-format 3)))
(set-org-agenda-prefix-format 1)



;;https://github.com/alphapapa/org-ql/blob/master/examples.org
;; https://github.com/alphapapa/org-ql/issues/79

;; (setq org-super-agenda-groups
;;       '((:log t :order 0)
;;         (:name "Schedule" :time-grid t)
;;         (:name "Finishable" :tag "Fnsh")
;;         (:name "Priority A" :priority "A")
;;         (:name "Complex" :tag "Cmplx")))

;; (add-hook 'org-agenda-mode-hook 'org-super-agenda-mode)




(setq ewhd-super-agenda-groups
      '((:discard (:category ("system")))
	(:discard (:tag ("DFR")))
	(:name "Check Scheduling:"
	       :and (:todo "SCHD" :not (:scheduled t))
	       :order 45)
	(:name "Overdue"
	       :deadline past
	       :order 10)
	(:name "Behind Schedule"
	       :scheduled past
	       :order 20)
	(:name "Check on these:"
	       :todo ("SCHD" "DPND" "WAIT" "REVW")
	       :order 64)
	(:name "Prioritize:" :priority "A" :order 50)
	(:name "Distractions:" :tag "Dstr" :order 63)
	(:name "Take Action:"
	       :and (:todo ("NEXT" "STRT") :priority>= "C")
	       :order 60)
	(:name "Prepare:"
	       :and (:todo ("TODO" "PREP") :priority>= "C")
	       :order 61)
	(:name "Inbox"
	       :tag "inbox"
	       :order 200)
	(:name "Define:"
	       :and (:todo ("IDEA" "TASK") :priority>= "C")
	       :order 62)
	(:name "Someday:"
	       :priority<= "D"
	       :order 120)
	(:name "Other Todo:" :todo "TODO" :order 80)
	(:todo "OPEN" :order 150)
	(:discard (:todo ("PROJ")))))

(setq ewhd-super-agenda-groups-system-only
      '((:discard (:not (:category "system")))
	(:discard (:tag ("DFR")))
	(:name "Bugs"
	       :todo "BUG_")
	(:name "Recently added / ready to add to Production"
	       :todo ("PROD" "SHIP"))
	(:name "Development and Testing"
	       :todo ("DEV_" "TEST"))
	(:name "Tasks"
	       :todo "TASK")
	(:name "Think about:"
	       :todo ("IDEA" "STDY" "REVW"))
	))

(setq ewhd-super-agenda-next-week
      '((:discard (:not (:tag ("DFR"))))
	(:name "" :tag "WK")
	(:discard (:not (:tag ("WK"))))
	))

(setq ewhd-super-agenda-next-month
      '((:discard (:not (:tag ("DFR"))))
	(:name "" :tag "MTH")
	(:discard (:not (:tag ("MTH"))))
	))

(setq ewhd-super-agenda-next-Q1
      '((:discard (:not (:tag ("DFR"))))
	(:name "" :tag "Q1")
	(:discard (:not (:tag ("Q1"))))
	))

(setq ewhd-super-agenda-next-Q2
      '((:discard (:not (:tag ("DFR"))))
	(:name "" :tag "Q2")
	(:discard (:not (:tag ("Q2"))))
	))

(setq ewhd-super-agenda-next-Q3
      '((:discard (:not (:tag ("DFR"))))
	(:name "" :tag "Q3")
	(:discard (:not (:tag ("Q3"))))
	))

(setq ewhd-super-agenda-next-Q4
      '((:discard (:not (:tag ("DFR"))))
	(:name "" :tag "Q4")
	(:discard (:not (:tag ("Q4"))))
	))

(setq ewhd-super-agenda-next-year
      '((:discard (:not (:tag ("DFR"))))
	(:name "" :tag "WK")
	(:discard (:not (:tag ("WK"))))
	))



(setq org-agenda-custom-commands
      '(("z" "Super view"
         ((agenda "" ((org-agenda-span 8)
		      (org-agenda-use-time-grid t)))
	  (org-ql-block '(and (or (scheduled :to 15)
				  (deadline :to 15))
			      (not (todo "DONE" "CANC" "COMP" "FNSH"))
			      )
			((org-ql-block-header "Upcoming (next 15 days):\n")))
          (alltodo "" ((org-agenda-overriding-header "The only sin is impatience")
                       (org-super-agenda-groups ewhd-super-agenda-groups)))))
	("x" "Super view - no agenda"
         ((org-ql-block '(and (or (scheduled :to 15)
				  (deadline :to 15))
			      (not (todo "DONE" "CANC" "COMP" "FNSH"))
			      )
			((org-ql-block-header "Upcoming (next 15 days):\n")))
	  (alltodo "" ((org-agenda-overriding-header "The only sin is impatience")
                       (org-super-agenda-groups ewhd-super-agenda-groups)))))
	("y" "System's View"
	 alltodo "" ((org-agenda-overriding-header "The only sin is impatience")
                     (org-super-agenda-groups ewhd-super-agenda-groups-system-only)))
	;; ("dw" "Deferred: Next Week"
	;;  ((org-ql-block '(and (and (tags "DFR" "WK"))
	;; 		      (todo)
	;; 		      (not (done)))
	;; 		((org-ql-block-header "Next Week:")))))
	("dw" "Deferred: Next Week"
	 ((alltodo "" ((org-agenda-overriding-header "Next Week")
                       (org-super-agenda-groups ewhd-super-agenda-next-week)))))
	("dm" "Deferred: Next Month"
	 ((alltodo "" ((org-agenda-overriding-header "Next Week")
		       (org-super-agenda-groups ewhd-super-agenda-next-week)))
	  (alltodo "" ((org-agenda-overriding-header "Next Month")
		       (org-super-agenda-groups ewhd-super-agenda-next-month)))))
	("d1" "Deferred: Next Q1"
	 ((alltodo "" ((org-agenda-overriding-header "Next Week")
		       (org-super-agenda-groups ewhd-super-agenda-next-week)))
	  (alltodo "" ((org-agenda-overriding-header "Next Month")
		       (org-super-agenda-groups ewhd-super-agenda-next-month)))
	  (alltodo "" ((org-agenda-overriding-header "Next Q1")
	               (org-super-agenda-groups ewhd-super-agenda-next-Q1)))))
	("d2" "Deferred: Next Q2"
	 ((alltodo "" ((org-agenda-overriding-header "Next Week")
		       (org-super-agenda-groups ewhd-super-agenda-next-week)))
	  (alltodo "" ((org-agenda-overriding-header "Next Month")
		       (org-super-agenda-groups ewhd-super-agenda-next-month)))
	  (alltodo "" ((org-agenda-overriding-header "Next Q2")
	               (org-super-agenda-groups ewhd-super-agenda-next-Q2)))))
	("d3" "Deferred: Next Q3"
	 ((alltodo "" ((org-agenda-overriding-header "Next Week")
		       (org-super-agenda-groups ewhd-super-agenda-next-week)))
	  (alltodo "" ((org-agenda-overriding-header "Next Month")
		       (org-super-agenda-groups ewhd-super-agenda-next-month)))
	  (alltodo "" ((org-agenda-overriding-header "Next Q3")
	               (org-super-agenda-groups ewhd-super-agenda-next-Q3)))))
	("d4" "Deferred: Next Q4"
	 ((alltodo "" ((org-agenda-overriding-header "Next Week")
		       (org-super-agenda-groups ewhd-super-agenda-next-week)))
	  (alltodo "" ((org-agenda-overriding-header "Next Month")
		       (org-super-agenda-groups ewhd-super-agenda-next-month)))
	  (alltodo "" ((org-agenda-overriding-header "Next Q4")
	               (org-super-agenda-groups ewhd-super-agenda-next-Q4)))))
	;; ("dy" "Deferred: Next Year")
	("da" "Deferred: All"
	 ((alltodo "" ((org-agenda-overriding-header "Next Week")
		       (org-super-agenda-groups ewhd-super-agenda-next-week)))
	  (alltodo "" ((org-agenda-overriding-header "Next Month")
		       (org-super-agenda-groups ewhd-super-agenda-next-month)))
	  (alltodo "" ((org-agenda-overriding-header "Next Q1")
	               (org-super-agenda-groups ewhd-super-agenda-next-Q1)))
	  (alltodo "" ((org-agenda-overriding-header "Next Q2")
	               (org-super-agenda-groups ewhd-super-agenda-next-Q2)))
	  (alltodo "" ((org-agenda-overriding-header "Next Q3")
	               (org-super-agenda-groups ewhd-super-agenda-next-Q3)))
	  (alltodo "" ((org-agenda-overriding-header "Next Q4")
	               (org-super-agenda-groups ewhd-super-agenda-next-Q4)))))
	))




;; stuff I'm playing around with just on my desktop for now
(when (string= system-name "ewhd-t730-debian")
  (setq
   ;; org-columns-default-format-for-agenda "%45ITEM %4TODO %1PRIORITY %4Effort(Estim){:}  %4CLOCKSUM(Clock) %38ALLTAGS"
   org-agenda-tags-column 80
   ;; org-agenda-remove-tags nil
   ))

(when (string= system-name "ewhd-book")
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (setq
   ;; org-columns-default-format-for-agenda "%45ITEM %4TODO %1PRIORITY %4Effort(Estim){:}  %4CLOCKSUM(Clock) %38ALLTAGS"
   org-agenda-tags-column 'auto
   ;; org-agenda-remove-tags nil
   org-tags-column -50
   )
  )

(when (string= system-name "ewhd-t480-debian")
  )
