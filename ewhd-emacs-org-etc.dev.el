;; ewhd emacs org mode related packages -*- lexical-binding: t; -*-

;;;; Nice bullets
(use-package org-superstar
    :config
    (setq org-superstar-special-todo-items t)
    (add-hook 'org-mode-hook (lambda ()
                               (org-superstar-mode 1)))
    )




;;;; ORG-MODE GENERAL BEHAVIOR
;; src block indentation / editing / syntax highlighting
(setq org-src-fontify-natively t
      org-src-window-setup 'current-window ;; edit in current window
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-preserve-indentation t ;; do not put two spaces on the left
      org-src-tab-acts-natively t)

;; move the logs generated by state changes into a drawer and out of sight
(setq org-log-into-drawer t)

;; timestamp org files when saved
(defun ewhd/update-org-modified-property ()
  "If a file contains a #+modified' property update it to contain
  the current date/time"
  (interactive)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+modified:[[:blank:]]*" (point-max) t)
      (progn
        (kill-line)
        (insert (format-time-string "%Y-%m-%d-T%H%M"))))))

(defun ewhd-org-mode-before-save-hook ()
  (when (and (eq major-mode 'org-mode) (eq nil (equal "emacs-config.org" (buffer-name))))
    (ewhd/update-org-modified-property)))

(add-hook 'before-save-hook #'ewhd-org-mode-before-save-hook)
        


;;;; ORG-CAPTURE
(global-set-key "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo [gtd]" entry (file "~/Dropbox/org/gtd/gtd.org") "* TODO %i%?")
	      ("i" "Todo [inbox]" entry (file+headline "~/Dropbox/org/gtd/inbox.org" "Tasks") "* TODO %i%?")
	      ("c" "Capture [inbox]" entry (file+headline "~/Dropbox/org/gtd/inbox.org" "Capture") "* %i%?")
        ("T" "Tickler" entry (file+headline "~/Dropbox/org/gtd/tickler.org" "Tickler") "* %i%? \n %U")))



;;;; ORG-REFILE
;; Set list of legitimate refile targets
(setq org-refile-target-list (list "~/Dropbox/org/gtd/gtd.org" "~/Dropbox/org/gtd/inbox.org"))

;; org-refile will allow refiling to any refile target, up to 1 level of headings deep
(setq org-refile-targets '(( org-refile-target-list :maxlevel . 1)))

;; allow refiling to the top (file) level, rather than just to headings (which is the defaults)
(setq org-refile-use-outline-path 'file)

;; correct for helm weirdness resulting from setting org-refile-use-outline-path which only allows the top level file to be displayed and not its headings. This fixes that.
(setq org-outline-path-complete-in-steps nil)



;;;; ORG TODO
;; global effort estimates:
(customize-set-variable 'org-global-properties
                        '(("Effort_ALL" . "0:00 0:07 0:15 0:30 0:45 1:00 1:30 2:00 2:30 3:00")))

;; org TODO keywords:
(setq org-todo-keywords
 '((sequence
    "TODO(t)"
    "NEXT(n)"
    "STARTED(s!)"
    "WAITING(w@/!)"
    "REVIEW(r)"
    "HOLD(h@/!)"
    "|"
    "CANCELED(x!)"
    "DONE(d!)")
   (sequence
    "OPEN(o)"
    "EVENT(e)"
    "|"
    "COMPLETE(c)")
   (sequence
    "PROJECT - OPEN(p!)"
    "PROJECT - ON-HOLD(@/!)"
    "|"
    "FINISHED(f!)")))
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

;; org TODO state triggers
(setq org-todo-state-tags-triggers
;; Triggers for state changes
      (quote (
              ;; Move to TODO removes the review, next, started, waiting, hold, open, event, and project tags
              ("TODO" ("REVIEW") ("NEXT") ("STARTED") ("WAITING") ("HOLD") ("OPEN") ("EVENT") ("PROJECT"))

              ;; Move to REVIEW adds review tag
              ;; Move to REVIEW removes next, waiting, hold, open, event, and project tags
              ("REVIEW" ("REVIEW" . t) ("NEXT") ("WAITING") ("HOLD") ("OPEN") ("EVENT") ("PROJECT"))

              ;; Move to NEXT adds next tag
              ;; Move to NEXT removes review, waiting, hold, open, event, and project tags
              ("NEXT" ("NEXT" . t) ("REVIEW") ("WAITING") ("HOLD") ("OPEN") ("EVENT") ("PROJECT"))

              ;; Move to STARTED adds the started tag
              ;; Move to STARTED removes review, next, waiting, hold, open, event, and project tags
              ("STARTED" ("STARTED" . t) ("REVIEW") ("NEXT") ("WAITING") ("HOLD") ("OPEN") ("EVENT") ("PROJECT"))

              ;; Move to WAITING adds the waiting tag
              ;; Move to WAITING removes the next, hold, open, event, and project tags
              ("WAITING" ("WAITING" . t) ("NEXT") ("HOLD") ("OPEN") ("EVENT") ("PROJECT"))

	            ;; Move to HOLD adds the hold tag
	            ;; Move to HOLD removes next, waiting, open, event, and project tags
              ("HOLD" ("HOLD" . t) ("NEXT") ("WAITING") ("OPEN") ("EVENT") ("PROJECT"))

              ;; Move to OPEN adds the open tag
	            ;; Move to OPEN removes the review, next, waiting, hold, event, and project tags
              ("OPEN" ("OPEN" . t) ("REVIEW") ("NEXT") ("WAITING") ("HOLD") ("EVENT") ("PROJECT"))

              ;; Move to EVENT add the event tag
	            ;; Move to EVENT removes the review, next, waiting, hold, open, and project tags
	            ("EVENT" ("EVENT" . t) ("REVIEW") ("NEXT") ("WAITING") ("HOLD") ("OPEN") ("PROJECT"))
	      
              ;; Move to canceled adds the canceled tag
	            ;; Move to canceled removes the project tag
              ("CANCELED" ("CANCELED" . t) ("PROJECT"))

	            ;; Move to done removes the canceled and project tags
	            ("DONE" ("CANCELED") ("PROJECT"))
              
              ;; Move to any todo state removes the canceled tag
              (todo ("CANCELED"))
              ;; Move to any done state removes review, next, started, waiting, open, and event tags
              (done ("REVIEW") ("NEXT") ("STARTED") ("WAITING") ("OPEN") ("EVENT"))

              ;; Assigning "PROJECT - OPEN" or "FINISHED" states adds the project tag and removes review, next, started, waiting, hold, open, and event tags
              ;; Assigning the "PROJECT - ON-HOLD" state adds the project and hold tags and removes review, next, started, waiting, hold, open, and event tags
              ("PROJECT - OPEN" ("PROJECT" . t) ("REVIEW") ("NEXT") ("STARTED") ("WAITING") ("HOLD") ("OPEN") ("EVENT"))
              ("PROJECT - ON-HOLD" ("PROJECT" . t) ("HOLD" . t) ("REVIEW") ("NEXT") ("STARTED") ("WAITING"))
              ("FINISHED" ("PROJECT" . t) ("REVIEW") ("NEXT") ("STARTED") ("WAITING") ("HOLD") ("OPEN") ("EVENT"))

              ;; Assigning no keyword removes all tags
              ("" ("REVIEW") ("NEXT") ("STARTED") ("WAITING") ("HOLD") ("OPEN") ("EVENT") ("PROJECT") ("CANCELED"))
             )))

(setq org-tags-exclude-from-inheritance '("NEXT" "STARTED" "PROJECT" "OPEN" "EVENT"))




;;;;;; ORG-AGENDA
(global-set-key "\C-ca" 'org-agenda)
(setq org-agenda-files (list
			"~/Dropbox/org/gtd/gtd.org"    "~/Dropbox/org/gtd/people.org" "~/Dropbox/org/gtd/cf-gtd.org"
			"~/Dropbox/org/gtd/career-gtd.org"
			"~/Dropbox/org/gtd/finance-gtd.org"
			"~/Dropbox/org/gtd/fun-gtd.org"
			"~/Dropbox/org/gtd/family-gtd.org"
			"~/Dropbox/org/gtd/health-gtd.org"
			"~/Dropbox/org/gtd/romance-gtd.org"
			"~/Dropbox/org/gtd/spirit-gtd.org"))


;;;; ORG-AGENDA-MODE
(setq org-agenda-window-setup 'current-window)  ; agenda takes current window
(setq org-agenda-restore-windows-after-quit t)  ; restore window configuration on exit
(setq org-agenda-start-with-follow-mode nil)
(setq org-columns-default-format-for-agenda "%25ITEM %4TODO %1PRIORITY %4Effort(Estim){:}  %4CLOCKSUM(Clock) %20ALLTAGS")
(add-hook 'org-agenda-mode-hook
;; Disables word-wrap and enables truncate-line in agenda buffers
          (lambda ()
            (visual-line-mode -1)
            (toggle-truncate-lines 1)))



;;;; ORG-AGENDA-VIEW
(setq org-agenda-time-grid '(
			     (daily today require-timed)
			     (600 900 1200 1500 1800 2100)
			     "......" "----------------------" nil)
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-include-diary t
      org-agenda-block-separator 9472
      org-agenda-compact-blocks nil
      org-agenda-start-with-log-mode nil)

;; Formatting (truncating) fields in agenda-view:
(defvar my-org-agenda-list-category t)
(defun my-org-agenda-toggle-list-category ()
  "Toggles whether category/file name appears or not at the left
  of entries in agenda listings. Useful to unclutter listings."
  (interactive)
  (if my-org-agenda-list-category
      (progn 
        (setq my-org-agenda-list-category nil)
        (setq org-agenda-prefix-format
              '((agenda  . "  %-12:c%?-12t% s")
                (timeline  . "  % s")
                (todo  . "  %-12:c")
                (tags  . "  %-12:c")
                (search . "  %-12:c")))
        )
    (setq my-org-agenda-list-category t)
    (setq org-agenda-prefix-format
          '((agenda  . "  %?-12t% s")
            (timeline  . "  % s")
            (todo  . "  ")
            (tags  . "  ")
            (search . "  ")))
    )
  (org-agenda-redo))

(add-hook 
 'org-mode-hook
 (lambda ()
   (define-key org-agenda-keymap   "L" 'my-org-agenda-toggle-list-category)
   (define-key org-agenda-mode-map "L" 'my-org-agenda-toggle-list-category)
   ))


;;;; ORG-SUPER-AGENDA

(use-package org-super-agenda
    :after org
    :config
    (org-super-agenda-mode)
    (setq org-agenda-custom-commands
          '(("z" "Super view"
             ((agenda "" ((org-agenda-span 'day)
                          (org-super-agenda-groups
                           '((:name "Today"
                                    :time-grid t
                                    :date today
                                    :todo "TODAY"
                                    :scheduled today
                                    :order 1)))))
              (alltodo "" ((org-agenda-overriding-header "")
                           (org-super-agenda-groups
                            '(
                              (:name "On Hold"
                                      :tag "HOLD"
                                      :order 30)
			      (:name "Review"
				     :tag "REVIEW"
				     :order 8)
                              (:name "Overdue"
                                     :deadline past
                                     :order 1)
			      (:name "Behind Schedule"
				     :scheduled past
				     :order 2)
                              (:name "Today"
                                     :deadline today
				     :scheduled today
                                     :order 3)
			      (:name "Next"
                                     :todo "NEXT"
                                     :order 6)
			      (:name "Started"
				     :todo "STARTED"
			             :order 7)
                              ;; (:name "Important"
                              ;;        :tag "Important"
                              ;;        :priority "A"
                              ;;        :order 7)
                              (:name "Other Items"
                                     :todo "TODO"
                                     :priority nil
                                     :order 8)
                              (:name "Waiting"
                                     :tag "WAITING"
                                     :order 9)
                              (:name "Due Soon"
                                     :deadline future
                                     :order 10)
                              (:name "Someday"
                                     :priority "C"
                                     :order 20)
                              (:name "Projects"
                                     :todo "PROJECT - OPEN"
                                     :order 25)
			      ;(:discard (:tag "PROJECT"))
                              ))))))
	    ("w" "Work niew"
             ((agenda "" ((org-agenda-span 'day)
                          (org-super-agenda-groups
                           '((:name "Today"
                                    :time-grid t
                                    :date today
                                    :todo "TODAY"
                                    :scheduled today
                                    :order 1)))))
              (alltodo "" ((org-agenda-overriding-header "")
                           (org-super-agenda-groups
                            '(
			      (:discard (:not (:tag "CF")))
                              (:name "On Hold"
                                      :tag "HOLD"
                                      :order 30)
			      (:name "Review"
				     :tag "REVIEW"
				     :order 8)
                              (:name "Overdue"
                                     :deadline past
                                     :order 1)
			      (:name "Behind Schedule"
				     :scheduled past
				     :order 2)
                              (:name "Today"
                                     :deadline today
				     :scheduled today
                                     :order 3)
			      (:name "Next"
                                     :todo "NEXT"
                                     :order 6)
			      (:name "Started"
				     :todo "STARTED"
			             :order 7)
                              (:name "Other Items"
                                     :todo "TODO"
                                     :priority nil
                                     :order 8)
                              (:name "Waiting"
                                     :tag "WAITING"
                                     :order 9)
                              (:name "Due Soon"
                                     :deadline future
                                     :order 10)
                              (:name "Someday"
                                     :priority "C"
                                     :order 20)
                              (:name "Projects"
                                     :todo "PROJECT - OPEN"
                                     :order 25)
                              ))))
	      ))
	    ("b" "everything But work"
             ((agenda "" ((org-agenda-span 'day)
                          (org-super-agenda-groups
                           '((:name "Today"
                                    :time-grid t
                                    :date today
                                    :todo "TODAY"
                                    :scheduled today
                                    :order 1)))))
              (alltodo "" ((org-agenda-overriding-header "")
                           (org-super-agenda-groups
                            '(
			      (:discard (:tag "CF"))
                              (:name "On Hold"
                                      :tag "HOLD"
                                      :order 30)
			      (:name "Review"
				     :tag "REVIEW"
				     :order 8)
                              (:name "Overdue"
                                     :deadline past
                                     :order 1)
			      (:name "Behind Schedule"
				     :scheduled past
				     :order 2)
                              (:name "Today"
                                     :deadline today
				     :scheduled today
                                     :order 3)
			      (:name "Next"
                                     :todo "NEXT"
                                     :order 6)
			      (:name "Started"
				     :todo "STARTED"
			             :order 7)
                              (:name "Other Items"
                                     :todo "TODO"
                                     :priority nil
                                     :order 8)
                              (:name "Waiting"
                                     :tag "WAITING"
                                     :order 9)
                              (:name "Due Soon"
                                     :deadline future
                                     :order 10)
                              (:name "Someday"
                                     :priority "C"
                                     :order 20)
                              (:name "Projects"
                                     :todo "PROJECT - OPEN"
                                     :order 25)
                              ))))
	      ))
           )
    )
)








;;;; ORG-DOWNLOAD
(use-package org-download
  :after org
  :defer nil
  :custom
  (org-download-method 'attach)
  (org-download-image-dir nil)
  ;; on Windows it is possible to use org-download-screenshot without using imagemagick convert, but by using powershell:
  (org-download-screenshot-method "powershell -c Add-Type -AssemblyName System.Windows.Forms;$image = [Windows.Forms.Clipboard]::GetImage();$image.Save('%s', [System.Drawing.Imaging.ImageFormat]::Png)")
  ;; (org-download-method '+org/org-download-method)
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  (org-image-actual-width 300)
  :bind
  ("C-M-y" .
   (lambda (&optional noask)
     (interactive "P")
     (let ((file
            (if (not noask)
                (read-string (format "Filename [%s]: " org-download-screenshot-basename)
                             nil nil org-download-screenshot-basename)
              nil)))
       (org-download-screenshot file))))
  :config
  (require 'org-download)
)


;; org-attach configuration:
(defun ewhd-org-attach-id-folder-format (id)
  "Translate an UUID ID into a folder-path.
Custom format for how Org translates ID properties to a path for
attachments.  Useful if ID is generated with custom timestamp."
  (format "%s/%s"
	  (substring id 0 7)
	  (substring id 2)))

(setq org-attach-id-to-path-function-list
  '(ewhd-org-attach-id-folder-format
    org-attach-id-ts-folder-format
    org-attach-id-uuid-folder-format))





;;;;;; ORG-ROAM
(use-package org-roam
      :init
      (setq org-roam-v2-ack t)
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/Dropbox/org/")
	    (org-id-method 'ts)  ;; changes org-roam default id method form uuid to a timestamp
	    (org-id-ts-format "%Y-%m-%d-T%H%M.%S.%2N")  ;; formats the timestamp method to create a unique but also human readable id
      ;(org-roam-completion everywhere t)
      :bind (;:map org-roam-mode-map
              ("C-c n l" . org-roam-buffer-toggle)
              ("C-c n f" . org-roam-node-find)
              ("C-c n g" . org-roam-graph)
              ("C-c n t" . org-roam-tag-add)
              ("C-c n T" . org-roam-tag-delete)
              ;:map org-mode-map
              ("C-c n i" . org-roam-node-insert))
       :config
       (org-roam-setup)

       )

;;;; ORG-ROAM TEMPLATES
;; org-roam-capture-templates:
(setq org-roam-capture-templates
         '(
;	   ("d" "default" plain "%?"
;            :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
;	    :unnarrowed t)
           ("t" "test" plain "%?"
	    :target (file+head "%<%Y%m%d%H>-${slug}.org"
			       "#+title: ${title}\n#+created: %<%Y%m%d%H%M%S>")
            :unnarrowed t)
	   ("b" "better" plain "%?"
	    :target (file+head "%<%Y%m%dT%H%M>-${slug}.org" "#+title:    ${title}\n%[~/Dropbox/org/templates/org-roam-header-template.org]\n%[~/Dropbox/org/templates/org-roam-dailies-template.org]")
            :unnarrowed t)
	   ("z" "zeta" plain "\n%?"
	    :target (file+head "%<%Y%m%dT%H%M>-${slug}.org"
			       "#+title:    ${title}
#+created:  %<%Y-%m-%d-T%H%M>
#+modified: <>
#+filetags: \n\n")
            :unnarrowed t)))


;; org-roam-dailies-capture-templates:
(setq org-roam-dailies-directory "daily/")

(setq org-roam-dailies-capture-templates
      '(("d" "default" plain "%?"
         :if-new (file+head "%<%Y-%m-%d>.org" "#+title:    %<%Y-%m-%d>\n%[~/Dropbox/org/templates/org-roam-header-template.org]\n%[~/Dropbox/org/templates/org-roam-dailies-template.org]"))))


;; org-roam-node-display-template:
(setq org-roam-node-display-template
  "${title:20}  ${file:9} ${tags:*}")

(advice-add #'org-roam-node--format-entry :override #'my/org-roam-node--format-entry)

(defun my/org-roam-get-subdirectory (filename)
  "Return the first subdirectory of FILENAME."
  (car (f-split filename)))

(defun my/org-roam-node--format-entry (template node &optional width)
  "Formats NODE for display in the results list.
WIDTH is the width of the results list.
TEMPLATE is the processed template used to format the entry."
  (pcase-let ((`(,tmpl . ,tmpl-width) template))
    (org-roam-format-template
     tmpl
     (lambda (field _default-val)
       (pcase-let* ((`(,field-name ,field-width) (split-string field ":"))
                    (getter (intern (concat "org-roam-node-" field-name)))
                    (field-value (funcall getter node)))
         (when (and (equal field-name "file")
                    field-value)
           (setq field-value (format "%s"
				     (my/org-roam-get-subdirectory
				      (file-relative-name field-value org-roam-directory)))))  ;; this bit was copied from nobiot's version
         (when (and (equal field-name "olp")
                    field-value)
           (setq field-value (string-join field-value " > ")))
         (when (and field-value (not (listp field-value)))
           (setq field-value (list field-value)))
         (setq field-value (mapconcat
                            (lambda (v)
                              (concat (or (cdr (assoc field-name org-roam-node-template-prefixes))
                                          "")
                                      v))
                            field-value " "))
         (setq field-width (cond
                            ((not field-width)
                             field-width)
                            ((string-equal field-width "*")
                             (if width
                                 (- width tmpl-width)
                               tmpl-width))
                            ((>= (string-to-number field-width) 0)
                             (string-to-number field-width))))
         (when field-width
           (let* ((truncated (truncate-string-to-width field-value field-width 0 ?\s))
                  (tlen (length truncated))
                  (len (length field-value)))
             (if (< tlen len)
                 ;; Make the truncated part of the string invisible. If strings
                 ;; are pre-propertized with display or invisible properties, the
                 ;; formatting may get messed up. Ideally, truncated strings are
                 ;; not preformatted with these properties. Face properties are
                 ;; allowed without restriction.
                 (put-text-property tlen len 'invisible t field-value)
               ;; If the string wasn't truncated, but padded, use this string instead.
               (setq field-value truncated))))
         field-value)))))


;;;; ORG-ROAM-UI
(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

