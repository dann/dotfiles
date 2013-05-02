(require 'anything-startup)
(require 'anything-config)
;; anything-grep
;; (auto-install-from-emacswiki "anything-grep.el")
(require 'anything-grep)
(require 'anything-match-plugin nil t)

(setq
   ;; 候補を表示するまでの時間。デフォルトは0.5
   anything-idle-delay 0.2
   ;; タイプして再描写するまでの時間。デフォルトは0.1
   anything-input-idle-delay 0.1
   ;; 候補の最大表示数。デフォルトは50
   anything-candidate-number-limit 50
   ;; 候補が多いときに体感速度を早くする
   anything-quick-update t
   ;; 候補選択ショートカットをアルファベットに
   anything-enable-shortcuts 'alphabet)

;; replace commands
(define-key global-map (kbd "C-x b") 'anything-for-me)
(define-key global-map (kbd "M-y")   'anything-show-kill-ring)
(define-key global-map (kbd "M-x")   'anything-M-x)
;; anything comamnd
(define-key global-map (kbd "C-z") 'anything-command-map)
(define-key anything-command-map (kbd "r") 'anything-resume)
(define-key anything-command-map (kbd "o") 'anything-occur)
(define-key anything-command-map (kbd "g") 'anything-grep)

(when (require 'descbinds-anything nil t)
;; describe-bindingsをAnythingに置き換える
  (descbinds-anything-install))

;; anything-for-me
(setq anything-for-me-sources
  (list anything-c-source-buffers
        anything-c-source-recentf
        anything-c-source-man-pages
        anything-c-source-emacs-commands
        anything-c-source-emacs-functions
        anything-c-source-files-in-current-dir))
(defun anything-for-me()
"Preconfigured `anything' for anything-for-me."
(interactive)
(anything anything-for-me-sources (thing-at-point 'symbol) nil nil nil "*anything for me"))

(define-key global-map (kbd "C-l") 'anything-for-me)

;;; manやinfoを調べるコマンドを作成してみる
;; anything-for-document 用のソースを定義
(setq anything-for-document-sources
  (list anything-c-source-man-pages
        anything-c-source-info-cl
        anything-c-source-info-pages
        anything-c-source-info-elisp
        anything-c-source-apropos-emacs-commands
        anything-c-source-apropos-emacs-functions
        anything-c-source-apropos-emacs-variables))
;; anything-for-document コマンドを作成
(defun anything-for-document ()
"Preconfigured `anything' for anything-for-document."
(interactive)
(anything anything-for-document-sources (thing-at-point 'symbol) nil nil nil "*anything for document*"))

;; C-hで一文字削除になるように
(define-key anything-map (kbd "C-h") 'delete-backward-char))
