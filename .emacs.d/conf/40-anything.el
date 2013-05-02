(require 'anything-startup)

(setq
 ;; 候補を表示するまでの時間。デフォルト0.5秒
 anything-idle-delay 0.3
 ;; 候補がおおいときに体感速度を早く
 anything-quick-update t)

(define-key global-map (kbd "C-x b") 'anything)

;; M-y で anything-show-kill-ring
(define-key global-map (kbd "M-y") 'anything-show-kill-ring)

;; anything-complete
(anything-read-string-mode 1)

(define-key emacs-lisp-mode-map "\C-\M-i" 'anything-lisp-complete-symbol-partial-match)
(define-key lisp-interaction-mode-map "\C-\M-i" 'anything-lisp-complete-symbol-partial-match)

;; filelist
(defun anything-custom-filelist ()
    (interactive)
    (anything-other-buffer
     (append
      '(anything-c-source-ffap-line
        anything-c-source-ffap-guesser
        anything-c-source-buffers+
        )
      (anything-c-sources-git-project-for)
      '(anything-c-source-recentf
        anything-c-source-bookmarks
        anything-c-source-file-cache
        anything-c-source-filelist
        ))
     "*anything file list*"))
