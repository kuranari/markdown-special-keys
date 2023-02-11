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
  ;; mwim の mwim-beginning-of-code-or-line と引数の処理を合わせた
  (interactive
   (progn
     (handle-shift-selection)
     (when current-prefix-arg
       (list
        (prefix-numeric-value current-prefix-arg)))))

  ;; 前置引数がある場合は先に移動する
  (when (and (not (null n)) (/= n 0)) (forward-line n))
  (cond
   ((markdown-list-item-at-point-p)(markdown-beginning-of-line--list))
   ((markdown-on-heading-p)(markdown-beginning-of-line--heading))
   (t (mwim-beginning-of-code-or-line))))

;; コードの良し悪しの判断ができない。mwimの内部構造を理解すると、もっと簡易な記述ができるのかもしれない
;; [nits] 行頭からコード行頭の間の空白文字の領域で関数を呼び出しした場合に、行頭ではなく、コード行頭にカーソルが移動してしまう挙動がやや気になる。
(defun markdown-beginning-of-line--list ()
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

(defun markdown-beginning-of-line--heading ()
  (cond
   ;; 1. 見出し本文の先頭の場合は、行頭に移動
   ;; 見出しの記号がinvisibleになっている可能性があるので markdown-move-heading-common を使って移動する
   ((looking-back markdown-regex-header-atx-asynmetric)
    (markdown-move-heading-common #'beginning-of-line nil 'adjust))
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

(defun markdown-insert-space-context ()
  "カーソルが list の先頭にある場合にインデントする"
  (interactive)
  (cond
   ;; 1. 行頭ならリストを挿入する
   ((and (bolp) (not (markdown-code-block-at-point-p))) (insert "* "))
   ;; 2. リストの先頭ならインデントする
   ((looking-back markdown-regex-list) (markdown-demote-list-item))
   ;; 3. それ以外ならスペースを入力する
   (t (insert " "))
   ))

(defun markdown-backspace-context ()
  "カーソルが list の先頭にある場合にアウトデントする"
  (interactive)
  (cond
   ;; 1. トップレベルのリストの先頭なら、バレットを削除する
   ((looking-back "^[-*:+][[:blank:]]+") (kill-line 0))
   ;; 2. リストの先頭ならアウトデントする
   ((looking-back markdown-regex-list) (markdown-promote-list-item))
   ;; 3. それ以外ならデフォルトの挙動を行う
   (t (markdown-outdent-or-delete 1))))

(evil-define-key 'hybrid markdown-mode-map
  (kbd "SPC") 'markdown-insert-space-context
  (kbd "DEL") 'markdown-backspace-context
  (kbd "C-h") 'markdown-backspace-context)

(provide 'markdown-special-keys)
;;; markdown-special-keys.el ends here
