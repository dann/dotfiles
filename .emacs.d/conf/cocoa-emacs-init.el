;; custom-file
(setq custom-file (concat user-emacs-directory "conf/cocoa-customize.el"))
(load custom-file)

;; tool-bar off
(tool-bar-mode 0)

;; scroll-bar off
(scroll-bar-mode 0)

;; menubar off
(menu-bar-mode 0)

;; metaキーを入れ替える
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;; full screen toggle
(define-key global-map (kbd "M-RET") 'ns-toggle-fullscreen)

;; 関連づけなどからファイルを開く場合に新規ウィンドウを開かない
(setq ns-pop-up-frames nil)

;; C-z で最小化するのうざいのでころす
(define-key global-map (kbd "C-z") nil)

