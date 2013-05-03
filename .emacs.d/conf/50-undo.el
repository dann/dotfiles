;;; undohist: 閉じたバッファも Undo できる
(when (require 'undohist nil t)
  (undohist-initialize))

;;; undo-tree: Undo の分岐を視覚化する
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))
