(setq clmemo-file-name "~/dev/sites/unknownplace.org/clmemo.txt")
(setq clmemo-time-string-with-weekday t)

(setq clmemo-subtitle-char "[")
(setq clmemo-subtitle-punctuation-char '(" [" . "]"))

(defadvice clmemo-get-title (after clmemo-get-title-with-time () activate)
  (setq ad-return-value (concat (format-time-string "%H:%M ") ad-return-value)))

(defun clmemo-with-file (memofile)
  (interactive "Fclmemo file: ")
  (let ((clmemo-file-name memofile))
    (call-interactively 'clmemo)))

(define-key global-map (kbd "C-x M") 'clmemo)
(define-key global-map (kbd "C-x C-M") (lambda ()
                                         (interactive)
                                         (clmemo-with-file "~/clmemo.txt")))
