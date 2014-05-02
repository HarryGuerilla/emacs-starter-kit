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

(setq load-path (cons (expand-file-name "~/.emacs.d/site-lisp") load-path))



;; ===================================================================
;; APPEARANCE
;; ===================================================================

;; show line numbers in buffer
(setq line-number-mode t)
(global-linum-mode 1)
(defun nolinum ()
  (global-linum-mode 0)
)

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
(menu-bar-mode -1)

(show-paren-mode t) ; highlight matching paren
(setq show-paren-style 'mixed)

(setq column-number-mode t)

;; stop cursor from blinking
 ; (blink-cursor-mode -1)

;; set color theme
(add-to-list 'load-path "~/.emacs.d/site-lisp/color-theme-6.6.0")
(load-file "~/.emacs.d/site-lisp/color-theme-6.6.0/color-theme-tangotango.el")
(load-file "~/.emacs.d/site-lisp/color-theme-6.6.0/color-theme-vibrant-ink.el")
(load-file "~/.emacs.d/site-lisp/color-theme-6.6.0/zenburn.el")
(load-file "~/.emacs.d/site-lisp/color-theme-6.6.0/color-theme-solarized.el")
(load-file "~/.emacs.d/site-lisp/color-theme-6.6.0/color-theme-railscasts.el")
(load-file "~/.emacs.d/site-lisp/color-theme-6.6.0/color-theme-tomorrow.el")
(load-file "~/.emacs.d/site-lisp/color-theme-6.6.0/tomorrow-night-theme.el")
(load-file "~/.emacs.d/site-lisp/color-theme-6.6.0/tomorrow-night-bright-theme.el")
(add-to-list 'color-themes
  '(color-theme-tangotango "Tango Tango" "http://blog.nozav.org/post/2010/07/12/Updated-tangotango-emacs-color-theme")
)
(add-to-list 'color-themes '(color-theme-vibrant-ink "Vibrant Ink" "<http://github.com/mig/color-theme-vibrant-ink"))
(add-to-list 'color-themes '(color-theme-zenburn "Zenburn" "<http://github.com/bbatsov/zenburn-emacs>"))
(add-to-list 'color-themes '(color-theme-railscasts "Railscasts" "Railscasts"))
(add-to-list 'color-themes '(color-theme-tomorrow "Tomorrow" "<Tomorrow theme>"))
(add-to-list 'color-themes '(color-theme-tomorrow "Tomorrow Night" "<Tomorrow theme>"))
(add-to-list 'color-themes '(color-theme-tomorrow "Tomorrow Night Bright" "<Tomorrow theme>"))

(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
;     (color-theme-railscasts)))
     (color-theme-tomorrow-night)))

;; Set Font attributes
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

; Setting font attributes was causing errors and slowdown
;;(setq default-frame-alist '((font . "inconsolata-11")))
;;(push '(font-backend xft x) default-frame-alist)
;;(setq font-lock-maximum-decoration t)

;; Make font smaller when using netbook
(if (string= system-name "caelum")
  (set-face-attribute 'default nil :height 90)
  (set-face-attribute 'default nil :height 110))

;; Color for terminal
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;(require 'blank-mode)

(require 'highline)
(highline-mode 1) ; highlight current line
(setq highline-face '((((class color) (background dark)) (:background "#343030"))))

(require 'lambda-mode)
(add-hook 'python-mode-hook #'lambda-mode 1)
(setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))



;; ===================================================================
;; GENERAL CONFIGURATION
;; ===================================================================
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
; saving desktop could prevent the daemon from starting due to conflicting pid
;;(desktop-save-mode t) ; restore desktop on restart
(setq require-final-newline t)
(mouse-wheel-mode t)
(setq default-directory "~/")
(fset 'yes-or-no-p 'y-or-n-p)
;;(setq default-major-mode 'text-mode)
(setq major-mode 'text-mode)
(global-set-key (kbd "C-x C-b") 'ibuffer) ; replace BufferMenu with ibuffer
    (autoload 'ibuffer "ibuffer" "List buffers." t)

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; mac keyboard keys
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; fullscreen
(require 'maxframe)
(defvar my-fullscreen-p t "Check if fullscreen is on or off")

(defun my-toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
	  (restore-frame)
	(maximize-frame)))

(global-set-key (kbd "M-RET") 'my-toggle-fullscreen)

;; Copy and Paste
(setq x-select-enable-clipboard t)

;; encryption
(require 'epa-file)
(epa-file-enable)
(setenv "GPG_AGENT_INFO" nil)



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
;        (require 'ruby-electric)
;        (ruby-electric-mode t)
        ))

;; as an alternative to Rinari you can use Rails-mode
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

;rhtml
(add-to-list 'load-path "~/.emacs.d/site-lisp/rhtml")
     (require 'rhtml-mode)
     (add-hook 'rhtml-mode-hook
     	  (lambda () (rinari-launch)))

;re-map tab for yas
(defun yas/advise-indent-function (function-symbol)
  (eval `(defadvice ,function-symbol (around yas/try-expand-first activate)
           ,(format
             "Try to expand a snippet before point, then call `%s' as usual"
             function-symbol)
           (let ((yas/fallback-behavior nil))
             (unless (and (interactive-p)
                          (yas/expand))
               ad-do-it)))))

(yas/advise-indent-function 'ruby-indent-line)

;; ===================================================================
;; Git
;; ===================================================================
(add-to-list 'load-path "~/.emacs.d/site-lisp/magit")
(require 'magit)


(if (equal is-a-mac t) ; on one of my macs git is installed in /usr/local/bin
    (push "/usr/local/bin" exec-path))



;; ===================================================================
;; CEDET
;; ===================================================================

;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/cedet-1.1/common"))
(load-file "~/.emacs.d/site-lisp/cedet-1.1/common/cedet.el")
;;(load-file (expand-file-name "~/.emacs.d/site-lisp/cedet-1.0/common/cedet.el"))
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
(global-srecode-minor-mode 1)            ; Enable template insertion menu



;; ===================================================================
;; ECB
;; ===================================================================
(add-to-list 'load-path "~/.emacs.d/site-lisp/ecb-2.40")
;;(require 'ecb)
(require 'ecb-autoloads)
(setq ecb-auto-activate t)
(setq ecb-layout-name "left15")
(setq ecb-layout-window-sizes (quote (("left15" (0.18435754189944134 . 0.75) (0.18435754189944134 . 0.25)))))
(setq ecb-options-version "2.40")
(setq ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
(setq ecb-source-path (quote ("~/Sites" "~/Code")))
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
(setq org-agenda-files (quote ("~/Dropbox/Org")))

(add-hook 'org-mode-hook 'nolinum)



;; ===================================================================
;; Android
;; ===================================================================

(require 'android)
(require 'android-mode)
(setq android-mode-sdk-dir "/Developer/android-sdk-mac_x86")
(add-hook 'gud-mode-hook
     (lambda ()
            (add-to-list 'gud-jdb-classpath "/Developer/android-sdk-mac_x86/platforms/android-8/android.jar")
            ))



;; ===================================================================
;; Autocomplete
;; ===================================================================
(add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete-1.3.1")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete-1.3.1/dict.enabled")

(ac-config-default)

;; ===================================================================
;; Python
;; ===================================================================

(add-to-list 'load-path "~/.emacs.d/site-lisp/python-mode.el-6.0.8/")
(autoload 'python-mode "python-mode.el" "Python mode." t)
(setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) auto-mode-alist))
(setq py-shell-name "/usr/bin/python3")

; look up python documentation
; to update the  pylookup db run:
; ./pylookup.py -u file:///usr/share/doc/python/html
(add-to-list 'load-path "~/.emacs.d/site-lisp/pylookup/")
(autoload 'pylookup-lookup "pylookup")
(autoload 'pylookup-update "pylookup")
(setq pylookup-program "~/.emacs.d/site-lisp/pylookup/pylookup.py")
(setq pylookup-db-file "~/.emacs.d/site-lisp/pylookup/pylookup.db")
(global-set-key "\C-ch" 'pylookup-lookup)

; autopair
(autoload 'autopair-global-mode "autopair" nil t)
(autopair-global-mode)
(add-hook 'lisp-mode-hook
          #'(lambda () (setq autopair-dont-activate t)))
(add-hook 'python-mode-hook
          #'(lambda ()
              (push '(?' . ?')
                    (getf autopair-extra-pairs :code))
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))

(require 'python-pep8)
(require 'python-pylint)

; remove trailing new lines and whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; ===================================================================
;; YASnippet
;; ===================================================================

;(add-to-list 'load-path "~/.emacs.d/site-lisp/yasnippet-0.6.1c")
;(require 'yasnippet)

;;  Yasnippets in this directory are compiled to improve load speed
;(yas/load-directory "~/.emacs.d/site-lisp/yasnippet-0.6.1c/snippets")
;(setq yas/prompt-functions '(yas/dropdown-prompt))

(require 'yas-custom) ;compiled from snippets directory
(yas/initialize)
(setq yas/global-mode t)


;; ===================================================================
;; Javascript
;; ===================================================================

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(eval-after-load 'js2-mode
  '(progn
     (require 'js2-imenu-extras)
     (js2-imenu-extras-setup)))


;; ===================================================================
;; Haskell
;; ===================================================================

(load "~/.emacs.d/site-lisp/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)


;; ===================================================================
;; Ledger
;; ===================================================================

(add-to-list 'load-path
              (expand-file-name "~/.emacs.d/site-lisp/ledger/"))
(load "ledger-mode")
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
(add-hook 'ledger-mode-hook 'nolinum)



;; ===================================================================
;; Markdown
;; ===================================================================

(load "~/.emacs.d/site-lisp/markdown-mode/markdown-mode.el")
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
