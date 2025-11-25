;; ewhd emacs work in progress -*- lexical-binding: t; -*-

(use-package casual

  :ensure t
  :bind (
         :map dired-mode-map
         ("C-c ?" . casual-dired-tmenu)
         ("s"     . casual-dired-sort-by-tmenu)
         ("/"     . casual-dired-search-replace-tmenu)
         :map emacs-lisp-mode-map
         ("C-c ?" . casual-elisp-tmenu)
         :map help-mode-map
         ("C-c ?" . casual-help-tmenu)
         ;; :map ibuffer-mode-map
         ;; ("C-c ?" . casual-ibuffer-tmenu)
         ;; ("F" . casual-ibuffer-filter-tmenu)
         ;; ("s" . casual-ibuffer-sortby-tmenu)
         :map Man-mode-map
         ("C-c ?" . casual-man-tmenu)
         ("n" . casual-lib-browse-forward-paragraph)
         ("p" . casual-lib-browse-backward-paragraph)
         ("[" . Man-previous-section)
         ("]" . Man-next-section)
         ("j" . next-line)
         ("k" . previous-line)
         ("K" . Man-kill)
         ("o" . casual-man-occur-options)
         )
  :hook
  (ibuffer-mode . ewhd-ibuffer-keys)
  (Man-mode . ewhd-Man-keys)
  :config
  ;; dired ediff config
  (casual-ediff-install) ; run this to enable Casual Ediff
  (add-hook 'ediff-keymap-setup-hook
            (lambda ()
              (keymap-set ediff-mode-map "C-c ?" #'casual-ediff-tmenu)))

  ;; keymap load hooked to ibuffer, since ibuffer-mode-map doesn't exist until
  ;; ibuffer is loaded for the first time
  (defun ewhd-ibuffer-keys ()
    (keymap-set ibuffer-mode-map "C-c ?" #'casual-ibuffer-tmenu)
    (keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu)
    (keymap-set ibuffer-mode-map "s" #'casual-ibuffer-sortby-tmenu)
    )
  )



(defun ewhd-window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))
