;; ewhd custom early-init.el -*- lexical-binding: t; -*-


;; Prerequisite for using elpaca
;; From https://github.com/progfolio/elpaca/blob/master/doc/early-init.el
(setq package-enable-at-startup nil)

;; Set user-home-directory
(defvar user-home-directory (concat (getenv "HOME") "/"))


;; Set XDG Base Directory Specification variables
(setq user-emacs-directory (expand-file-name (concat (getenv "XDG_CONFIG_HOME") "/emacs/")))
(defvar user-emacs-cache-directory (expand-file-name (concat (getenv "XDG_CACHE_HOME") "/emacs/")))
(defvar user-emacs-data-directory (expand-file-name (concat (getenv "XDG_DATA_HOME") "/emacs/")))

;; Ensure the directories exist
(make-directory user-emacs-directory t)
(make-directory user-emacs-cache-directory t)
(make-directory user-emacs-data-directory t)


;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:


;;EOF
