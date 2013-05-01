;; =================================================================
;; Emacs directory
;; =================================================================
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))
(add-to-list 'load-path user-emacs-directory)

;; =================================================================
;; el-get
;; =================================================================
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(setq-default el-get-dir (locate-user-emacs-file "el-get")
              el-get-emacswiki-base-url
              "http://raw.github.com/emacsmirror/emacswiki.org/master/")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "http://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
(add-to-list 'el-get-recipe-path (locate-user-emacs-file "recipes"))
(el-get 'sync
        '(el-get
          anything
          anything-project
          anything-c-moccur
          auto-save-buffers-enhanced
          auto-complete
          cperl-mode
          clmemo
          ee
          emacs-w3m
          git-gutter
          init-loader
          markdown-mode
          open-junk-file
          perl-completion
          switch-window
          shell-history 
          ))

;; =================================================================
;; Load init files
;; =================================================================
(require 'init-loader)
;; load
(setq-default init-loader-show-log-after-init nil
            init-loader-byte-compile t)
(init-loader-load (locate-user-emacs-file "conf"))

;; hide compilation results
(let ((win (get-buffer-window "*Compile-Log*")))
  (when win (delete-window win)))

;; =================================================================
;; site-lisp 
;; =================================================================
(when (fboundp 'normal-top-level-add-subdirs-to-load-path)
  (let* ((dir (locate-user-emacs-file "site-lisp"))
         (default-directory dir))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir)
      (normal-top-level-add-subdirs-to-load-path))))
