;; ewhd emacs work in progress -*- lexical-binding: t; -*-

(use-package casual
  :ensure t
  :config
  ;;dired casual config
  (keymap-set dired-mode-map "C-c ?" #'casual-dired-tmenu)
  (keymap-set dired-mode-map "s" #'casual-dired-sort-by-tmenu)
  (keymap-set dired-mode-map "/" #'casual-dired-search-replace-tmenu)

  ;; dired ediff config
  (casual-ediff-install) ; run this to enable Casual Ediff
  (add-hook 'ediff-keymap-setup-hook
            (lambda ()
              (keymap-set ediff-mode-map "C-c ?" #'casual-ediff-tmenu)))

  ;; elisp casual config
  (keymap-set emacs-lisp-mode-map "C-c ?" #'casual-elisp-tmenu)

  ;; eshell casual config
  ;; (keymap-set eshell-mode-map "C-c ?" #'casual-eshell-tmenu)

  ;; help casual config
  (keymap-set help-mode-map "C-c ?" #'casual-help-tmenu)

  ;;ibuffer casual config
  (keymap-set ibuffer-mode-map "C-c ?" #'casual-ibuffer-tmenu)
  (keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu)
  (keymap-set ibuffer-mode-map "s" #'casual-ibuffer-sortby-tmenu)

  ;; add to ibuffer block


  ;; man casual config
  (keymap-set Man-mode-map "C-c ?" #'casual-man-tmenu)
  (keymap-set Man-mode-map "n" #'casual-lib-browse-forward-paragraph)
  (keymap-set Man-mode-map "p" #'casual-lib-browse-backward-paragraph)
  (keymap-set Man-mode-map "[" #'Man-previous-section)
  (keymap-set Man-mode-map "]" #'Man-next-section)
  (keymap-set Man-mode-map "j" #'next-line)
  (keymap-set Man-mode-map "k" #'previous-line)
  (keymap-set Man-mode-map "K" #'Man-kill)
  (keymap-set Man-mode-map "o" #'casual-man-occur-options)
  )
