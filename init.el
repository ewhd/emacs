;; ewhd basic emacs configuration -*- lexical-binding: t; -*-

;; The purpose of this file is to set a few things to optimize load time, but
;; mostly to source the file ewhd-emacs-init.el

;; Portions of the this file are borrowed from https://github.com/CSRaghunandan/.emacs.d/blob/master/init.el

;; Every file opened and loaded by Emacs will run through this list to check for
;; a proper handler for the file, but during startup, it won’t need any of them.
(defvar rag--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)



;; load ewhd-emacs-init.el, which will load the rest of the config
(load (locate-user-emacs-file "ewhd-emacs-init.el") nil :nomessage)



;; if gcmh exists, enable gcmh mode and garbage collect when moving out to other applications
(when (require 'gcmh nil t)
  (gcmh-mode 1)
  (add-function :after after-focus-change-function #'gcmh-idle-garbage-collect))


;; start emacs server only it has not already been started
(require 'server)
(unless (server-running-p) (server-start))

;; set gc-cons-threshold back to original value
(setq file-name-handler-alist rag--file-name-handler-alist
      gc-cons-threshold 16777216 ;; use 16MB
      gc-cons-percentage 0.1)




;;End
(put 'narrow-to-region 'disabled nil)
