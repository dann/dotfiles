;; no backup
(defun no-backup ()
  (interactive)
  (set (make-local-variable 'make-backup-files) nil)
  (auto-save-mode 0))

;; use directory name instead of <num>
(require 'uniquify nil 'noerror)
(setq uniquify-buffer-name-style 'forward)

;; Buffer-menu
(add-hook 'Buffer-menu-mode-hook
          #'(lambda ()
              (hl-line-mode 1)
              (setq show-trailing-whitespace nil)))
