;;; ewhd emacs work in progress -*- lexical-binding: t; -*-



;; Source - https://stackoverflow.com/a/11701899
;; Posted by Nicolas Dudebout
;; Retrieved 2025-12-04, License - CC BY-SA 3.0

(defun ewhd-toggle-show-trailing-whitespace ()
  "Toggle show-trailing-whitespace between t and nil"
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))


(setq flymake-no-changes-timeout nil)


;; (use-package project
;;   :bind-keymap ("C-c p" . project-prefix-map)
;;   ;; Other project.el configurations...
;;   )

;; Unbind the old prefix
;; (global-unset-key (kbd "C-x p"))

;; Bind the new prefix
;; (define-key global-map (kbd "C-c p") project-prefix-map)



(define-key text-mode-map (kbd "M-n") 'forward-line)
(define-key text-mode-map (kbd "M-p") '(lambda () (interactive) (forward-line -1)))
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "M-p") (lambda () (interactive) (forward-line -1)))))




(use-package hideshow
  :disabled t
  :hook
  (prog-mode . hs-minor-mode)
  :bind
  ("C-<tab>" . hs-cycle)
  ("C-<iso-lefttab>" . hs-global-cycle)
  ("C-S-<tab>" . hs-global-cycle))
(defun hs-cycle (&optional level)
  (interactive "p")
  (let (message-log-max
        (inhibit-message t))
    (if (= level 1)
        (pcase last-command
          ('hs-cycle
           (hs-hide-level 1)
           (setq this-command 'hs-cycle-children))
          ('hs-cycle-children
           ;; TODO: Fix this case. `hs-show-block' needs to be
           ;; called twice to open all folds of the parent
           ;; block.
           (save-excursion (hs-show-block))
           (hs-show-block)
           (setq this-command 'hs-cycle-subtree))
          ('hs-cycle-subtree
           (hs-hide-block))
          (_
           (if (not (hs-already-hidden-p))
               (hs-hide-block)
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))))
      (hs-hide-level level)
      (setq this-command 'hs-hide-level))))

(defun hs-global-cycle ()
  (interactive)
  (pcase last-command
    ('hs-global-cycle
     (save-excursion (hs-show-all))
     (setq this-command 'hs-global-show))
    (_ (hs-hide-all))))




(setq-default cursor-type t)


(use-package treesit-jump
  :ensure (:host github :repo "dmille56/treesit-jump" :files ("*.el" "treesit-queries"))
  :config
  ;; Optional: add some queries to filter out of results (since they can be too
  ;; cluttered sometimes)
  (setq treesit-jump-queries-filter-list '("inner" "test" "param")))


(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys)
  )


(use-package modalka
  :ensure t
  ;; :disabled t
  :init

  :preface
  (defun mk-modalka-mode-no-git-commit ()
    "Enable ‘modalka-mode’ unless get edit git commit message."
    (unless (string-equal (buffer-name) "COMMIT_EDITMSG")
      (modalka-mode 1)))
  :hook
  (
   (text-mode . mk-modalka-mode-no-git-commit)
   (prog-mode . modalka-mode)
   (conf-mode . modalka-mode)
   (diff-mode . modalka-mode)
   (gitignore-mode . modalka-mode)
   (help-mode . modalka-mode)
   (info-mode . modalka-mode)
   )

  :config
  ;; Translate regular keybindings
  ;; Number prefixes:
  (modalka-define-kbd "0" "C-0")
  (modalka-define-kbd "1" "C-1")
  (modalka-define-kbd "2" "C-2")
  (modalka-define-kbd "3" "C-3")
  (modalka-define-kbd "4" "C-4")
  (modalka-define-kbd "5" "C-5")
  (modalka-define-kbd "6" "C-6")
  (modalka-define-kbd "7" "C-7")
  (modalka-define-kbd "8" "C-8")
  (modalka-define-kbd "9" "C-9")

  ;; Special character behavior
  ;; ' (handy as self-inserting)
  (modalka-define-kbd "'" "C-'")        ; er/expand-region
  ;; " (handy as self-inserting)
  (modalka-define-kbd "," "C-,")
  ;; - (handy as self-inserting)
  (modalka-define-kbd "/" "C-/")        ; avy-goto-char-timer
  (modalka-define-kbd "." "C-.")
  (modalka-define-kbd ":" "M-:")        ; eval-expression
  (modalka-define-kbd ";" "C-;")        ; flyspell-correct-wrapper
  ;; (modalka-define-kbd "?" "C-?")        ; ewhd-eldoc-toggle
  (modalka-define-kbd "(" "C-x (")      ; kmacro-start-macro
  (modalka-define-kbd ")" "C-x )")      ; kmacro-end-macro
  (modalka-define-kbd "{" "M-{")
  (modalka-define-kbd "}" "M-}")
  (modalka-define-kbd "]" "M-e")
  (modalka-define-kbd "[" "M-a")
  (modalka-define-kbd ">" "C-p")
  (modalka-define-kbd "<" "C-n")
  (modalka-define-kbd "." "M-f")
  (modalka-define-kbd "," "M-b")
  (modalka-define-kbd "?" "M-e")

  (modalka-remove-kbd "q")
  (modalka-define-kbd "Q o" "C-q o")
  (modalka-define-kbd "Q C" "C-q C")     ; tabspaces-clear-buffers
  (modalka-define-kbd "Q x" "C-q x")     ; tabspaces-close-workspace
  (modalka-define-kbd "Q X" "C-q X")     ; tabspaces-kill-buffers-close-workspace
  (modalka-define-kbd "Q o" "C-q o")     ; tabspaces-open-or-create-project-and-workspace
  (modalka-define-kbd "Q w" "C-q w")     ; tabspaces-switch-or-create-workspace
  (modalka-define-kbd "Q r" "C-q r")     ; tabspaces-restore-session
  (modalka-define-kbd "Q s" "C-q s")     ; tabspaces-save-session
  (modalka-define-kbd "Q S" "C-q S")     ; tabspaces-save-current-project-session
  (modalka-define-kbd "Q k" "C-q k")     ; tabspaces-remove-selected-buffer
  (modalka-define-kbd "Q K" "C-q K")     ; tabspaces-remove-current-buffer
  (modalka-define-kbd "Q b" "C-q b")     ; tabspaces-switch-buffer-and-tab
  (modalka-define-kbd "Q q" "C-q q")     ; tabspaces-show-workspaces
  (modalka-define-kbd "Q n" "C-q n")     ; tab-next
  (modalka-define-kbd "Q p" "C-q p")     ; tab-previous
  (modalka-define-kbd "Q u" "C-q u")     ; tab-undo
  (modalka-define-kbd "Q N" "C-q N")     ; tab-new
  (modalka-define-kbd "Q m" "C-q m")     ; tab-move
  (modalka-define-kbd "Q <right>" "C-q <right>")  ; tab-next
  (modalka-define-kbd "Q <left>" "C-q <left>")  ; tab-previous
  (modalka-define-kbd "Q RET" "C-q RET")   ; tab-bar-select-tab-by-name
  (modalka-define-kbd "Q 1" "C-q 1")     ; tab-bar-select-tab 1
  (modalka-define-kbd "Q 2" "C-q 2")     ; tab-bar-select-tab 2
  (modalka-define-kbd "Q 3" "C-q 3")     ; tab-bar-select-tab 3
  (modalka-define-kbd "Q 4" "C-q 4")     ; tab-bar-select-tab 4
  (modalka-define-kbd "Q 5" "C-q 5")     ; tab-bar-select-tab 5
  (modalka-define-kbd "Q 6" "C-q 6")     ; tab-bar-select-tab 6
  (modalka-define-kbd "Q 7" "C-q 7")     ; tab-bar-select-tab 7
  (modalka-define-kbd "Q 8" "C-q 8")     ; tab-bar-select-tab 8
  (modalka-define-kbd "Q 9" "C-q 9")     ; tab-bar-select-tab 9
  (modalka-define-kbd "w" "M-w")
  (modalka-define-kbd "W" "C-w")
  (modalka-define-kbd "e" "C-e")
  (modalka-define-kbd "E" "M-e")
  (modalka-define-kbd "r" "C-r")
  (modalka-define-kbd "R" "M-%")
  (modalka-define-kbd "t" "C-t")        ; transpose-char
  (modalka-define-kbd "T" "M-t")        ; transpose-words
  (modalka-define-kbd "y" "C-y")
  (modalka-define-kbd "Y" "M-y")
  (modalka-define-kbd "u" "C-z")
  (modalka-define-kbd "U" "C-S-z")
  (modalka-define-kbd "i" "S-<return>") ; disable modalka-mode, aka "insert"
  (modalka-define-kbd "I" "S-<return>") ; disable modalka-mode, aka "insert"
  (modalka-define-kbd "o" "C-o")        ; open-line
  (modalka-define-kbd "O" "C-S-o")      ; other-window
  (modalka-define-kbd "p" "C-p")
  (modalka-define-kbd "P" "M-p")

  (modalka-define-kbd "a" "C-a")
  (modalka-define-kbd "A" "M-a")
  (modalka-define-kbd "s" "C-s")
  (modalka-define-kbd "S" "C-s")
  (modalka-define-kbd "d" "C-d")        ; delete-char
  (modalka-define-kbd "D" "M-d")        ; kill-word
  (modalka-define-kbd "f" "C-f")
  (modalka-define-kbd "F" "M-f")
  (modalka-define-kbd "g" "C-g")        ; quit
  (modalka-define-kbd "G" "C-g")        ; quit
  (modalka-define-kbd "h" "M-h")        ; mark-paragraph
  (modalka-define-kbd "H" "C-h")        ; mark-paragraph
  (modalka-define-kbd "j" "C-j")        ;
  (modalka-define-kbd "J" "C-j")        ; 
  (modalka-define-kbd "k" "C-k")        ; kill-visual-line
  (modalka-define-kbd "K" "M-k")        ; kill-sentence
  (modalka-define-kbd "l" "C-l")        ; recenter-top-bottom
  (modalka-define-kbd "L" "M-l")        ; downcase-word

  (modalka-define-kbd "z" "C-z")        ; undo
  (modalka-define-kbd "Z" "C-S-z")        ; redo
  ;; (modalka-define-kbd "Z" "M-z")        ; zap-to-character
  (modalka-define-kbd "x b" "C-x b")
  (modalka-define-kbd "x ;" "C-x C-;")  ; comment-line
  (modalka-define-kbd "x e" "C-x C-e")  ; eval-last-sexp
  (modalka-define-kbd "x E" "C-x e")    ; kmacro-end-and-call-macro
  (modalka-define-kbd "x s" "C-x C-s")
  (modalka-define-kbd "x SPC" "C-x C-SPC")
  (modalka-remove-kbd "X")
  (modalka-define-kbd "c" "M-c")        ; xah-toggle-letter-case
  (modalka-define-kbd "C" "M-c")        ; xah-toggle-letter-case
  (modalka-define-kbd "v" "C-v")
  (modalka-define-kbd "V" "C-v")
  (modalka-define-kbd "b" "C-b")
  (modalka-define-kbd "B" "M-b")
  (modalka-define-kbd "n" "C-n")
  (modalka-define-kbd "N" "M-n")
  (modalka-define-kbd "m" "C-SPC")
  (modalka-define-kbd "M" "M-a")




  ;; Bind a command directly
  ;; (define-key modalka-mode-map (kbd "Q") #'my-command)

  ;; Bind a prefix key for another keymap
  ;; (define-key modalka-mode-map "x" ctl-x-map)
  ;; (define-key modalka-mode-map "Q" tabspaces-mode-map)  
  ;; (define-key ctl-x-map (kbd "e") #'eval-last-sexp)
  ;; (define-key ctl-x-map (kbd "s") #'save-buffer)

  ;; Activate modalka-mode
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "S-<return>") nil))
  (global-set-key (kbd "S-<return>") #'modalka-mode)
  ;; (global-set-key (kbd "C-m") #'modalka-mode)

  ;; Enable modalka minor mode everywhere by default (except minibuffer)
  ;; (modalka-global-mode 1)

  ;; Disable modalka-mode in these major modes:
  ;; (add-to-list 'modalka-excluded-modes 'magit-status-mode)

  ;; Enable modalka-mode when other certain modes are called:
  ;; (add-hook 'text-mode-hook #'modalka-mode)
  ;; (add-hook 'prog-mode-hook #'modalka-mode)

  ;; Set cursor for visual feedback
  ;; (setq-default cursor-type '(bar . 1)) ; normal
  (setq-default cursor-type 'box) ; normal
  (setq modalka-cursor-type 'hollow)       ; modalka-mode
  )
