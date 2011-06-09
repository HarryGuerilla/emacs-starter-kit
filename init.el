;; TONY PELAEZ
;; ===========
;; basic emacs configuration for ruby on rails development

;; ===================================================================
;; CUSTOMIZED VARIABLES
;; ===================================================================

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; test to see if this computer is a mac
(setq is-a-mac nil)
(if (equal system-type 'darwin)
    (setq is-a-mac t))


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
(load-file "~/.emacs.d/site-lisp/color-theme-6.6.0/color-theme-vibrant-ink.el")
(load-file "~/.emacs.d/site-lisp/color-theme-6.6.0/zenburn.el")
(load-file "~/.emacs.d/site-lisp/color-theme-6.6.0/color-theme-solarized.el")
(load-file "~/.emacs.d/site-lisp/color-theme-6.6.0/color-theme-railscasts.el")
(add-to-list 'color-themes 
  '(color-theme-tangotango "Tango Tango" "http://blog.nozav.org/post/2010/07/12/Updated-tangotango-emacs-color-theme")
)
(add-to-list 'color-themes '(color-theme-vibrant-ink "Vibrant Ink" "<http://github.com/mig/color-theme-vibrant-ink"))
(add-to-list 'color-themes '(color-theme-zenburn "Zenburn" "<http://github.com/bbatsov/zenburn-emacs>"))
(add-to-list 'color-themes '(color-theme-railscasts "Railscasts" "Railscasts"))

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
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
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

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; mac keyboard keys
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; fullscreen
(load-file (expand-file-name "~/.emacs.d/site-lisp/maxframe.el"))
(require 'maxframe)
(defvar my-fullscreen-p t "Check if fullscreen is on or off")

(defun my-toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
	  (restore-frame)
	(maximize-frame)))

(global-set-key (kbd "M-RET") 'my-toggle-fullscreen)

;; ===================================================================
;; KEYBOARD SHORTCUTS
;; ===================================================================

(global-set-key "\M-z" 'undo)
(global-set-key "\M-s" 'save-buffer)
(global-set-key "\C-xt" 'term)
(global-set-key "\M-g" 'magit-status)


;; ===================================================================
;; NAVIGATION
;; ===================================================================

;;(add-to-list 'load-path "~/.emacs.d/site-lisp/icicles/")
;;(require 'icicles)
;;(icy-mode 1)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching



;; ===================================================================
;; RUBY
;; ===================================================================
(add-to-list 'load-path "~/.emacs.d/site-lisp/ruby-mode")

(setq auto-mode-alist (cons '("\\(?:\\.irbc\\|\\.rb\\)$" . ruby-mode)
			    auto-mode-alist))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process" t)
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode" t)

(add-hook 'ruby-mode-hook
      (lambda()
        (add-hook 'local-write-file-hooks
                  '(lambda()
                     (save-excursion
                       (untabify (point-min) (point-max))
                       (delete-trailing-whitespace)
                       )))
        (set (make-local-variable 'indent-tabs-mode) 'nil)
        (set (make-local-variable 'tab-width) 2)
        (imenu-add-to-menubar "IMENU")
;        (define-key ruby-mode-map "\C-m" 'newline-and-indent) ;Not sure if this line is 100% right!
        (require 'ruby-electric)
        (ruby-electric-mode t)
        ))

;(setq load-path (cons (expand-file-name "~/.emacs.d/site-lisp/rails-mode") load-path))
;(require 'rails-autoload)
;(require 'inf-ruby)

;; Rinari
(add-to-list 'load-path "~/.emacs.d/site-lisp/rinari")
(require 'rinari)

;haml
(require 'haml-mode)

;rspec
(add-to-list 'load-path "~/.emacs.d/site-lisp/rspec-mode")
(require 'rspec-mode)

;sass
(require 'sass-mode)

;coffeescript
(add-to-list 'load-path "~/.emacs.d/site-lisp/coffee-mode")
(require 'coffee-mode)


;; ===================================================================
;; Git
;; ===================================================================
(add-to-list 'load-path "~/.emacs.d/site-lisp/magit")
(require 'magit)


;; ===================================================================
;; CEDET
;; ===================================================================

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/cedet-1.0/common"))
(load-file (expand-file-name "~/.emacs.d/site-lisp/cedet-1.0/common/cedet.el"))
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
(global-srecode-minor-mode 1)            ; Enable template insertion menu


;; ===================================================================
;; ECB
;; ===================================================================
(add-to-list 'load-path "~/.emacs.d/site-lisp/ecb-2.40")
(require 'ecb)
(setq ecb-auto-activate t)
(setq ecb-layout-name "left15")
(setq ecb-layout-window-sizes (quote (("left15" (0.18435754189944134 . 0.75) (0.18435754189944134 . 0.25)))))
(setq ecb-options-version "2.40")
(setq ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
(setq ecb-source-path (quote ("~/Sites" "~/Code/Android")))
(setq ecb-tip-of-the-day nil)
(setq ecb-tree-buffer-style (quote ascii-guides))
(setq ecb-tree-indent 2)
(setq ecb-vc-enable-support t)
(setq ecb-windows-width 0.15)


;; ===================================================================
;; JDEE
;; ===================================================================

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/jdee-2.4.0.1/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/elib-1.0"))

;; Set the debug option to enable a backtrace when a
;; problem occurs.
(setq debug-on-error t)

;; If you want Emacs to defer loading the JDE until you open a 
;; Java file, edit the following line
;; (setq defer-loading-jde nil)
;; to read:
;; (setq defer-loading-jde t)
;;
(setq defer-loading-jde t)

(if defer-loading-jde
    (progn
      (autoload 'jde-mode "jde" "JDE mode." t)
      (setq auto-mode-alist
	    (append
	     '(("\\.java\\'" . jde-mode))
	     auto-mode-alist)))
  (require 'jde))

;; Sets the basic indentation for Java source files
;; to two spaces.
(defun my-jde-mode-hook ()
  (setq c-basic-offset 2))

(add-hook 'jde-mode-hook 'my-jde-mode-hook)

;; Include the following only if you want to run
;; bash as your shell.

;; Setup Emacs to run bash as its primary shell.
;(setq shell-file-name "bash")
(setq shell-file-name "zsh")
(setq shell-command-switch "-c")
(setq explicit-shell-file-name shell-file-name)
(setenv "SHELL" shell-file-name)
(setq explicit-sh-args '("-login" "-i"))
;;(if (boundp 'w32-quote-process-args)
;;  (setq w32-quote-process-args ?\")) ;; Include only for MS Windows.

(setq jde-jdk-registry
      (quote (("1.6.0_24" . "/System/Library/Frameworks/JavaVM.framework/Versions/CurrentJDK/")))
)
(setq jde-jdk 
      (quote ("1.6.0_24")))

(require 'flymake)

;; function does not exist in emacs 23.2
(defun semantic-parse())

(setq jde-complete-function (quote jde-complete-menu))
(setq jde-gen-k&r t)
(setq jde-global-classpath nil)


;; ===================================================================
;; ORG MODE
;; ===================================================================

(setq load-path (cons "~/.emacs.d/site-lisp/org-mode/lisp" load-path))
(setq load-path (cons "~/.emacs.d/site-lisp/org-mode/contrib/lisp" load-path))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(require 'org-install)

;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox/Org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/Org/inbox.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-agenda-files (quote ("~/Dropbox/Org/fancyhair.org" "~/Dropbox/Org/Personal.org" "~/Dropbox/Org/ilearnthings.org")))


;; ===================================================================
;; Android
;; ===================================================================

(add-to-list 'load-path "~/.emacs.d/site-lisp/android.el")
(require 'android)
(add-to-list 'load-path "~/.emacs.d/site-lisp/android-mode.el")
(require 'android-mode)
(setq android-mode-sdk-dir "/Developer/android-sdk-mac_x86")
(add-hook 'gud-mode-hook
     (lambda ()
            (add-to-list 'gud-jdb-classpath "/Developer/android-sdk-mac_x86/platforms/android-8/android.jar")
            ))
