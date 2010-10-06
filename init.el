;; TONY PELAEZ
;; ===========
;; basic emacs configuration for ruby on rails development

;; ===================================================================
;; CUSTOMIZED VARIABLES
;; ===================================================================

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;; ===================================================================
;; APPEARANCE
;; ===================================================================

;; show line numbers in buffer
(setq line-number-mode t)
(global-linum-mode 1)

;; show fringe indicator for empty lines
(setq indicate-empty-lines 1)

;; show clock in status bar
(display-time)        

;; startup screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; menu's & scroll bars
(scroll-bar-mode -1)
(tool-bar-mode -1)

(show-paren-mode t) ; highlight matching paren
(setq show-paren-style 'mixed)

(setq column-number-mode t)

;; stop cursor from blinking
(blink-cursor-mode -1)

;; set color theme
(add-to-list 'load-path "~/.emacs.d/site-lisp/color-theme-6.6.0")
(load-file "~/.emacs.d/site-lisp/color-theme-6.6.0/color-theme-tangotango.el")
(add-to-list 'color-themes 
  '(color-theme-tangotango "Tango Tango" "http://blog.nozav.org/post/2010/07/12/Updated-tangotango-emacs-color-theme")
)
(load-file "~/.emacs.d/site-lisp/color-theme-6.6.0/color-theme-railscasts.el")
(add-to-list 'color-themes '(color-theme-railscasts "Railscasts" "<http://railscasts.org">))
(load-file "~/.emacs.d/site-lisp/color-theme-6.6.0/color-theme-vibrant-ink.el")
(add-to-list 'color-themes '(color-theme-vibrant-ink "Vibrant Ink" "<http://github.com/mig/color-theme-vibrant-ink"))

(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-railscasts)))

;; Set Font attributes
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-frame-alist '((font . "inconsolata-11")))
(push '(font-backend xft x) default-frame-alist)
(setq font-lock-maximum-decoration t)

;; Color for terminal
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;(require 'blank-mode)

;(require 'highline)
;(highline-mode 1) ; highlight current line



;; ===================================================================
;; GENERAL CONFIGURATION
;; ===================================================================
(setq load-path (cons (expand-file-name "~/.emacs.d/site-lisp") load-path))
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
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
(setq default-major-mode 'text-mode)
(desktop-save-mode 1) ;; Saves buffers between sessions

;; SET MAC SYSTEM FONTS FOR TERMINAL
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)



;; ===================================================================
;; KEYBOARD SHORTCUTS
;; ===================================================================

(global-set-key "\M-z" 'undo)
(global-set-key "\M-s" 'save-buffer)
(global-set-key "\C-xt" 'term)


;; ===================================================================
;; NAVIGATION
;; ===================================================================

(add-to-list 'load-path "~/.emacs.d/site-lisp/icicles/")
(require 'icicles)
(icy-mode 1)



;; ===================================================================
;; RUBY
;; ===================================================================

(setq auto-mode-alist (cons '("\\(?:\\.irbc\\|\\.rb\\)$" . ruby-mode)
			    auto-mode-alist))
;(autoload 'run-ruby "inf-ruby"
;  "Run an inferior Ruby process" t)
;(autoload 'inf-ruby-keys "inf-ruby"
;  "Set local key defs for inf-ruby in ruby-mode" t)

;(setq load-path (cons (expand-file-name "~/.emacs.d/site-lisp/rails-mode") load-path))
;(require 'rails-autoload)
;(require 'inf-ruby)


;; ===================================================================
;; Git
;; ===================================================================

(require 'magit)
