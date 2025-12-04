;; ewhd emacs configuration -*- lexical-binding: t; -*-

;; The purpose of this file is to load and configure denote.el

(use-package denote
  :ensure t
  :init
  (denote-rename-buffer-mode 1)
  :custom
  (denote-prompts '(title keywords subdirectory))
  (denote-directory (expand-file-name "~/Documents/notes/"))
  (denote-file-type 'org)
  (denote-known-keywords '())
  :bind (("C-c d d" . denote)
	 ("C-c d l" . denote-link)
	 ("C-c d f" . list-denotes)
	 ("C-c d t" . denote-template)
	 ("C-c d d" . denote)
	 )
  :config
  (setq denote-templates `( ;;(test . "test")
	                   ))
  :hook ((dired-mode . denote-dired-mode))
  )

(use-package denote-org
  :ensure t
  :commands
  ;; I list the commands here so that you can discover them more
  ;; easily.  You might want to bind the most frequently used ones to
  ;; the `org-mode-map'.
  ( denote-org-link-to-heading
    denote-org-backlinks-for-heading

    denote-org-extract-org-subtree

    denote-org-convert-links-to-file-type
    denote-org-convert-links-to-denote-type

    denote-org-dblock-insert-files
    denote-org-dblock-insert-links
    denote-org-dblock-insert-backlinks
    denote-org-dblock-insert-missing-links
    denote-org-dblock-insert-files-as-headings))

(use-package denote-explore
  :ensure t
  :custom
  ;; Location of graph files
  (denote-explore-network-directory "~/Documents/notes/graphs/")
  (denote-explore-network-filename "denote-network")
  ;; Output format
  (denote-explore-network-format 'graphviz)
  (denote-explore-network-graphviz-filetype "svg")
  ;; Exlude keywords or regex
  (denote-explore-network-keywords-ignore '("bib"))

  ;; :bind
  ;; (;; Statistics
  ;;  ("C-c w x c" . denote-explore-count-notes)
  ;;  ("C-c w x C" . denote-explore-count-keywords)
  ;;  ("C-c w x b" . denote-explore-barchart-keywords)
  ;;  ("C-c w x e" . denote-explore-barchart-filetypes)
  ;;  ;; Random walks
  ;;  ("C-c w x r" . denote-explore-random-note)
  ;;  ("C-c w x l" . denote-explore-random-link)
  ;;  ("C-c w x k" . denote-explore-random-keyword)
  ;;  ("C-c w x x" . denote-explore-random-regex)
  ;;  ;; Denote Janitor
  ;;  ("C-c w x d" . denote-explore-identify-duplicate-notes)
  ;;  ("C-c w x z" . denote-explore-zero-keywords)
  ;;  ("C-c w x s" . denote-explore-single-keywords)
  ;;  ("C-c w x o" . denote-explore-sort-keywords)
  ;;  ("C-c w x w" . denote-explore-rename-keyword)
  ;;  ;; Visualise denote
  ;;  ("C-c w x n" . denote-explore-network)
  ;;  ("C-c w x v" . denote-explore-network-regenerate)
  ;;  ("C-c w x D" . denote-explore-degree-barchart))
  )

(use-package citar-denote
  :ensure t
  :disabled t)


(use-package denote-search
  :ensure t
  :bind
  ;; Customize keybindings to your liking
  ;; (("C-c s s" . denote-search)
  ;;  ("C-c s d" . denote-search-marked-dired-files)
  ;;  ("C-c s r" . denote-search-files-referenced-in-region))
  :custom
  ;; Disable help string (set it once you learn the commands)
  ;; (denote-search-help-string "")
  ;; Display keywords in results buffer
  (denote-search-format-heading-function #'denote-search-format-heading-with-keywords))

(use-package denote-silo
  :ensure t
  :disabled t
  ;; Bind these commands to key bindings of your choice.
  :commands ( denote-silo-create-note
              denote-silo-open-or-create
              denote-silo-select-silo-then-command
              denote-silo-dired
              denote-silo-cd )
  :config
  ;; Add your silos to this list.  By default, it only includes the
  ;; value of the variable `denote-directory'.
  (setq denote-silo-directories
        (list denote-directory
              "~/Documents/personal/"
              "~/Documents/work/")))

(use-package denote-journal
  :ensure t
  ;; Bind those to some key for your convenience.
  :commands ( denote-journal-new-entry
              denote-journal-new-or-existing-entry
              denote-journal-link-or-create-entry )
  :hook (calendar-mode . denote-journal-calendar-mode)
  :config
  ;; Use the "journal" subdirectory of the `denote-directory'.  Set this
  ;; to nil to use the `denote-directory' instead.
  (setq denote-journal-directory
        (expand-file-name "journal" denote-directory))
  ;; Default keyword for new journal entries. It can also be a list of
  ;; strings.
  (setq denote-journal-keyword "journal")
  ;; Read the doc string of `denote-journal-title-format'.
  (setq denote-journal-title-format 'day-date-month-year))

(use-package denote-sequence
  :ensure t
  :disabled t
  :bind
  ;; ( :map global-map
  ;; Here we make "C-c n s" a prefix for all "[n]otes with [s]equence".
  ;; This is just for demonstration purposes: use the key bindings
  ;; that work for you.  Also check the commands:
  ;;
  ;; - `denote-sequence-new-parent'
  ;; - `denote-sequence-new-sibling'
  ;; - `denote-sequence-new-child'
  ;; - `denote-sequence-new-child-of-current'
  ;; - `denote-sequence-new-sibling-of-current'
  ;; ("C-c n s s" . denote-sequence)
  ;; ("C-c n s f" . denote-sequence-find)
  ;; ("C-c n s l" . denote-sequence-link)
  ;; ("C-c n s d" . denote-sequence-dired)
  ;; ("C-c n s r" . denote-sequence-reparent)
  ;; ("C-c n s c" . denote-sequence-convert))
  :config
  ;; The default sequence scheme is `numeric'.
  (setq denote-sequence-scheme 'alphanumeric)
  )

(use-package denote-denote-zettel-interface
  :ensure t
  :disabled t)

(use-package denote-menu
  :ensure t)

(use-package consult-denote
  :ensure t
  :bind
  (("C-c n f" . consult-denote-find)
   ("C-c n g" . consult-denote-grep))
  :config
  (consult-denote-mode 1))

(use-package consult-notes
  :ensure t
  :commands (consult-notes
             consult-notes-search-in-all-notes
             ;; if using org-roam 
             ;; consult-notes-org-roam-find-node
             ;; consult-notes-org-roam-find-node-relation
	     )
  :config
  (setq consult-notes-file-dir-sources '(("Documents"  ?d  "~/Documents/CF/")
					 ;; ("Local" ?l "./")
					 )) ;; Set notes dir(s), see below
  ;; Set org-roam integration, denote integration, or org-heading integration e.g.:
  ;; (setq consult-notes-org-headings-files '("~/path/to/file1.org"
  ;; "~/path/to/file2.org"))
  ;; (consult-notes-org-headings-mode)
  (when (locate-library "denote")
    (consult-notes-denote-mode))
  ;; search only for text files in denote dir
  ;; (setq consult-notes-denote-files-function (function denote-directory-text-only-files))
  )
