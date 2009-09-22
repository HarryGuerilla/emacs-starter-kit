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