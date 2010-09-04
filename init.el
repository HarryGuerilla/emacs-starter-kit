;; TONY PELAEZ
;; ===========
;; basic emacs configuration for ruby on rails development

;; CUSTOMIZED VARIABLES =================================================
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; APPEARANCE ===========================================================

(setq line-number-mode t)
(global-linum-mode 1) ; show line numbers
(display-time)        ; show clock

(setq inhibit-startup-screen t)
(setq inhibit-scratch-message nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(show-paren-mode t) ; highlight matching paren
(setq show-paren-style 'mixed)

(setq column-number-mode t)

(setq default-frame-alist '((font . "inconsolata")))
(push '(font-backend xft x) default-frame-alist)
(setq font-lock-maximum-decoration t)

;(require 'blank-mode)

;(require 'highline)
;(highline-mode 1) ; highlight current line

;; GENERAL CONFIGURATION ================================================
'(recentf-mode t)
'(transient-mark-mode t)
(setq make-backup-files nil)
(setq query-replace-highlight t)
(setq require-final-newline t)
(setq-default intent-tabs-mode nil)
(server-start) ; allow access from emacs client
(desktop-save-mode t) ; restore desktop on restart
(setq require-final-newline t)
(mouse-wheel-mode t)
(setq default-directory "~/")
(fset 'yes-or-no-p 'y-or-n-p)
(setq major-mode 'text-mode)

;; KEYBOARD SHORTCUTS ===================================================
(global-set-key "\M-z" 'undo)
(global-set-key "\M-s" 'save-buffer)
(global-set-key "\C-xt" 'term)

;; RUBY =================================================================
(setq auto-mode-alist (cons '("\\(?:\\.irbc\\|\\.rb\\)$" . ruby-mode)
			    auto-mode-alist))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process" t)
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode" t)

(setq load-path (cons (expand-file-name "~/.emacs.d/vendor/rails-mode") load-path))
(require 'rails-autoload)
(require 'inf-ruby)
