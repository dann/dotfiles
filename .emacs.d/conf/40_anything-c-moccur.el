(require 'color-moccur)
(setq moccur-split-word t)

(require 'anything-c-moccur)
(setq anything-c-moccur-anything-idle-delay 0.2 ;`anything-idle-delay'
      anything-c-moccur-higligt-info-line-flag t ; `anything-c-moccur-dmoccur'などのコマンドでバッファの情報をハイライトする
      anything-c-moccur-enable-auto-look-flag t ; 現在選択中の候補の位置を他のwindowに表示する
      anything-c-moccur-enable-initial-pattern t) ; `anything-c-moccur-occur-by-moccur'の起動時にポイントの位置の単語を初期パターンにする

(define-key global-map (kbd "M-o") 'anything-c-moccur-occur-by-moccur) ;バッファ内検索
(define-key global-map (kbd "C-M-o") 'anything-c-moccur-dmoccur) ;ディレクトリ
