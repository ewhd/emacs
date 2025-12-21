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
