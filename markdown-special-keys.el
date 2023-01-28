;;; markdown-special-keys.el --- TODO: Frobnicate and bifurcate flanges

;; Copyright (C) 2023 Tomohisa Kuranari

;; Author: Tomohisa Kuranari <tomohisa.kuranari@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((markdown-mode "2.1") (evil "1.5") (mwim "0.4"))
;; Keywords: markdown
;; URL: TODO

;;; Commentary:

;; This package provides a minor mode to frobnicate and/or
;; bifurcate any flanges you desire.  To activate it, just type

;;; Code:
(require 'markdown-mode)
(require 'evil-core)

(defvar markdown-regex-header-atx-asynmetric "^#+[ \t]+")

(defun markdown-beginning-of-line (&optional n)
  (interactive "P") ;; org-beginning-of-line と合わせた

  (cond
   ((markdown-list-item-at-point-p)(markdown-beginning-of-line--list n))
   ((markdown-on-heading-p)(markdown-beginning-of-line--heading n))
   (t (mwim-beginning-of-code-or-line (if n (prefix-numeric-value n) n)))))

;; コードの良し悪しの判断ができない。mwimの内部構造を理解すると、もっと簡易な記述ができるのかもしれない
;; [nits] 行頭からコード行頭の間の空白文字の領域で関数を呼び出しした場合に、行頭ではなく、コード行頭にカーソルが移動してしまう挙動がやや気になる。
(defun markdown-beginning-of-line--list (&optional n)
  (cond
   ;; 1. リスト本文の先頭の場合は、bulletの前に移動
   ((looking-back markdown-regex-list)
    ;; 参考: mwim-beginning-of-code
    (progn
      (beginning-of-line)
      (skip-syntax-forward " " (line-end-position))))
   ;; 2. bulletの前の場合は、行頭に移動
   ((looking-back "^\s+") (beginning-of-line))
   ;; 3. それ以外の場合は、リスト本文の先頭に移動
   (t (progn
        (beginning-of-line)
        (re-search-forward markdown-regex-list)))))

(defun markdown-beginning-of-line--heading (&optional n)
  (cond
   ;; 1. 見出し本文の先頭の場合は、行頭に移動
   ;; 見出しの記号がinvisibleになっている可能性があるので markdown-move-heading-common を使って移動する
   ((looking-back markdown-regex-header-atx-asynmetric)
    (markdown-move-heading-common #'beginning-of-line n 'adjust))
   ;; 2. それ以外の場合は、リスト本文の先頭に移動
   (t (progn
        (beginning-of-line)
        (re-search-forward markdown-regex-header-atx-asynmetric)))))

;; 本当は 下記のような設定を加えて mwim のオプションに追加したい
;; (add-to-list 'mwim-beginning-of-line-function '(markdown-mode . markdown-beginning-of-line))
;; しかし list 以外の要素で Ctrl-a が使えなくなってしまう問題が残るので諦めた(調査時間の問題)
;; 代わりにキーバインドを直接割り当てている
(evil-define-key 'hybrid markdown-mode-map (kbd "C-a") 'markdown-beginning-of-line)

;; カーソルが非表示の文字列の上にある場合には、1文字前進させる。
;; evil-modeでinsert-modeからnormal-modeに戻る時に、カーソルが一文字後退する(normal-modeでiを押下し、直後にEscを押下するとその挙動がわかる)
;; markdown-hide-markupがtの場合、テキストプロパティ(display "")により非表示の文字にカーソルが移動することがあるためこれを防ぐ
;;
;; TODO: move-beginning-of-line も同じような処理をしているかも
(defun markdown-adjust-hidden-markup ()
  ;; (point) は normal-mode に戻って1文字後退した地点になる
  (when (and markdown-hide-markup
             (eq (get-char-property (point) 'display) ""))
    (forward-char)))

(add-hook 'evil-normal-state-entry-hook 'markdown-adjust-hidden-markup)

(provide 'markdown-special-keys)
;;; markdown-special-keys.el ends here
