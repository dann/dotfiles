(require 'auto-complete-config)
(ac-config-default)

;; objc-mode追加
(add-to-list 'ac-modes 'objc-mode)

;; メニュー専用のキーマップを使う (for C-p/C-n)
(setq ac-use-menu-map t)

;; auto completion like IntelliSense
(global-auto-complete-mode t)
(setq ac-auto-show-menu 0.5)
(define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
(define-key ac-complete-mode-map (kbd "TAB") nil)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
