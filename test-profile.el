(advice-add #'x-apply-session-resources :override #'ignore)
(setq make-backup-files nil
      auto-save-default nil
      visible-bell t)

(load-file "dracula-theme.el")
(load-theme 'dracula t)
