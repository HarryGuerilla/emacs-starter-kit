;; DECIDE WHICH FUNCTIONALITY TO ENABLE
(setq *macbook-support-enabled* t)
(setq *spell-check-support-enabled* t)
(setq *byte-code-cahce-enabled* t)
(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))

;; SET COMMAND KEY TO META KEY
(setq mac-command-modifier 'meta)

;; SET MAC SYSTEM FONTS FOR TERMINAL
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; ENABLE MENU-BAR FOR EMACS 23
(menu-bar-mode)

;; ENABLE EMACS SERVER
(server-start)