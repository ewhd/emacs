;; ewhd custom early-init.el -*- lexical-binding: t; -*-
;; This file should either be placed or symlinked into ~/.emacs.d
;; It should make emacs XDG compliant

;; Prerequisite for using elpaca
;; From https://github.com/progfolio/elpaca/blob/master/doc/early-init.el
(setq package-enable-at-startup nil)

;; Set user-home-directory
(defvar user-home-directory (concat (getenv "HOME") "/"))



;; Set XDG Base Directory Specification variables
(setq user-emacs-directory (expand-file-name (concat (getenv "XDG_CONFIG_HOME") "/emacs/") user-emacs-directory))
(setq user-emacs-cache-directory (expand-file-name (concat (getenv "XDG_CACHE_HOME") "/emacs/") user-emacs-cache-directory))
(setq user-emacs-data-directory (expand-file-name (concat (getenv "XDG_DATA_HOME") "/emacs/") user-emacs-data-directory))


;; Ensure the directories exist
(make-directory user-emacs-directory t)
(make-directory user-emacs-cache-directory t)
(make-directory user-emacs-data-directory t)


;; Alternatively, if running without XDG base directories, comment the above sections and uncomment the following line:
;; (setq user-emacs-directory (concat user-home-directory ".emacs.d/"))


;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t

;; End:
