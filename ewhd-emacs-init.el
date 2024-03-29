;; ewhd emacs configuration -*- lexical-binding: t; -*-

;; The purpose of this file is purely to call other config files
;; By commenting or uncommenting lines, I can choose which features to activate


;; General
;; Define identity, determine what kind of system it is on, etc.
(load (locate-user-emacs-file "ewhd-emacs-general.el") nil :nomessage)

;; Settings
;; Customize emacs behavior and keybindings
(load (locate-user-emacs-file "ewhd-emacs-settings.el") nil :nomessage)

;; Load the settings needed to make elpaca work
;; !!! This is required before loading any packages !!!
(load (locate-user-emacs-file "elpaca-init.el") nil :nomessage)

;; Universal Packages
;; Packages and their settings which I will always want
(load (locate-user-emacs-file "ewhd-emacs-universal-packages.el") nil :nomessage)
