(set-face-attribute 'default nil
                    :family "Menlo" :height 120)

(set-fontset-font nil 'unicode
                  (font-spec :family "Menlo"))

(set-fontset-font nil 'korean-ksc5601
                  (font-spec :family "Nanum Gothic"))

(dolist (code '(japanese-jisx0208 katakana-jisx0201))
  (set-fontset-font nil code
                    (font-spec :family "Hiragino Kaku Gothic ProN")))

(setq face-font-rescale-alist
      '((".*Menlo.*" . 1.0)
        (".*Hiragino.*" . 1.24)
        (".*Nanum.*" . 1.24)))
