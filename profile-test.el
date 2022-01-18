(advice-add #'x-apply-session-resources :override #'ignore)
(setq make-backup-files nil
      auto-save-default nil
      visible-bell t
      warning-minimum-level :debug
      warning-minimum-log-level :debug)
(toggle-debug-on-error)

(load "dracula-theme")
(load-theme 'dracula t)
