;;; cssh-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (cssh-mode cssh-regexp-host-start cssh-term-remote-open)
;;;;;;  "cssh" "cssh.el" (19502 49240))
;;; Generated autoloads from cssh.el

(global-set-key (kbd "C-M-=") (quote cssh-regexp-host-start))

(autoload (quote cssh-term-remote-open) "cssh" "\
Opens a M-x term and type in ssh remotehost with given hostname

\(fn)" t nil)

(global-set-key (kbd "C-=") (quote cssh-term-remote-open))

(autoload (quote cssh-regexp-host-start) "cssh" "\
start ClusterSSH for all mathing hosts in  known_hosts

\(fn &optional CSSH-BUFFER-NAME)" t nil)

(autoload (quote cssh-mode) "cssh" "\
A major mode for controlling multiple terms at once.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("cssh-pkg.el") (19502 49240 657461))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; cssh-autoloads.el ends here
