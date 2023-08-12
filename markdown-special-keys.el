;;; markdown-special-keys.el --- TODO: Frobnicate and bifurcate flanges

;; Copyright (C) 2023 Tomohisa Kuranari

;; Author: Tomohisa Kuranari <tomohisa.kuranari@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((markdown-mode "2.1") (evil "1.15"))
;; Keywords: markdown
;; URL: https://github.com/kuranari/markdown-special-keys

;;; Commentary:

;; This package provides a minor mode to frobnicate and/or
;; bifurcate any flanges you desire.  To activate it, just type

;;; Code:
(require 'markdown-mode)
(require 'evil)

;; markdown-modeのmarkdown-regex-listを改変して定義
;; * 全角スペースを空白として扱わないようにするために[[:blank]]を\sで置換
;; * リスト記号の後ろの空白は1つのみを許容する
;; * チェックボックスの後ろには1つ以上の空白を必要とする
(defvar markdown-regex-list-ascii-only
  "^\\(\s*\\)\\([#0-9]+\\.\\|[*+:-]\\)\\(\s\\)\\(\\(?:\\[[ Xx]]\\)\s+\\)?")

(defun markdown-beginning-of-line (&optional n)
  "Go to the beginning of the current line.

If this is a header or a list item, on the first attempt move to where the
text starts, and only move to beginning of line when the cursor is already
before the start of the text of the line.

With argument N not nil or 1, move forward N - 1 lines first."
  (interactive "^p")
  (let ((origin (point)))
    ;; First move to a line.
    (move-beginning-of-line n)
    (cond
     ((looking-at markdown-regex-header-atx)
      ;; At a header, special position is before the title.
      (let ((refpos (match-beginning 2)))
	      (if (or (> origin refpos)
                (= origin (line-beginning-position))
                (/= (line-number-at-pos origin) (line-number-at-pos))
                ;; Prevents the cursor from moving to invisible characters
                markdown-hide-markup)
	          (goto-char refpos))))
     ((looking-at markdown-regex-list)
      ;; At a list item, special position is after the list marker or checkbox.
      (let ((refpos (or (match-end 4) (match-end 3))))
	      (if (or (> origin refpos)
                (= origin (line-beginning-position))
                (/= (line-number-at-pos origin) (line-number-at-pos)))
	          (goto-char refpos))))
     ((looking-at "^\s+")
      ;; At an indented text line, special position is after the indentation.
      (let ((refpos (match-end 0)))
	      (if (or (> origin refpos)
                (= origin (line-beginning-position))
                (/= (line-number-at-pos origin) (line-number-at-pos)))
            (goto-char refpos))))

     ;; No special case, already at beginning of line.
     (t nil))))

;; カーソルが非表示の文字列の上にある場合には、1文字前進させる。
;; evil-modeでinsert-modeからnormal-modeに戻る時に、カーソルが一文字後退する(normal-modeでiを押下し、直後にEscを押下するとその挙動がわかる)
;; markdown-hide-markupがtの場合、テキストプロパティ(display "")により非表示の文字にカーソルが移動することがあるためこれを防ぐ
;;
;; 要調査: move-beginning-of-line も同じような処理をしているかもいレナい
(defun markdown-adjust-hidden-markup ()
  ;; (point) は normal-mode に戻って1文字後退した地点になる
  (when (and markdown-hide-markup
             (eq (get-char-property (point) 'display) ""))
    (forward-char)))

(add-hook 'evil-normal-state-entry-hook 'markdown-adjust-hidden-markup)

(defun markdown--current-indentation ()
  (save-excursion
    (back-to-indentation)
    (current-column)))

(defun markdown-insert-space-context ()
  "カーソルがリストの先頭にある場合にインデントする"
  (interactive)
  (if (markdown-code-block-at-point-p) (insert " ")
    (let ((start-of-indention (markdown--current-indentation)))
      (cond
       ;; 1. 行頭ならリストを挿入する
       ;; (markdown-insert-list-item)を使うと前の行とインデントレベルが合うが求めている挙動ではない
       ((bolp) (insert "* "))
       ;; 2. リストの先頭ならインデントする
       ((looking-back markdown-regex-list-ascii-only)
        (save-excursion
          (indent-line-to (+ start-of-indention markdown-list-indent-width))))
       ;; 3. それ以外ならスペースを入力する
       (t (insert " "))))))

(defun markdown-backspace-context ()
  "カーソルがリストの先頭にある場合にアウトデントする"
  (interactive)
  (let ((start-of-indention (markdown--current-indentation)))
    (cond
     ;; 1. リストの先頭の場合
     ((looking-back markdown-regex-list-ascii-only)
      (if (= start-of-indention 0)
          ;; 1-1. バレットを削除
          (kill-line 0)
        ;; 1-2. アウトデント
        (save-excursion
          (indent-line-to (- start-of-indention markdown-list-indent-width)))))
     ;; 2. 全角スペースの場合は文字する
     ;; (行頭の全角スペースを削除できるようにするため)
     ((string= (char-to-string (preceding-char)) "　") (delete-char -1))
     ;; 3. それ以外ならデフォルトの挙動を行う
     (t (markdown-outdent-or-delete 1)))))

;; 参考: evil-org-insert-line
(defun evil-markdown-insert-line (count)
  "Insert at beginning of line.
The insertion will be repeated COUNT times."
  (interactive "p")
  (if (or (markdown-list-item-at-point-p) (markdown-on-heading-p))
      (progn (beginning-of-line)
             (markdown-beginning-of-line nil)
             (evil-insert count))
    (evil-insert-line count)))

(defun markdown-cycle-advice (original &rest args)
  (if (or (not (bolp))
          (markdown-code-block-at-point-p)
          (markdown-list-item-at-point-p)
          (markdown-table-at-point-p)
          (markdown-on-heading-p))
      (apply original args)
    (insert "* ")))

(advice-add 'markdown-cycle :around #'markdown-cycle-advice)

(defun markdown-enter-key-advice (original &rest args)
  (cond
   ;; Listと本文の間に空行がない場合に、本文の次の行がリストになることを防ぐため
   ;; markdown-indent-on-enterが'indent-and-new-itemであってもList ItemのLazy continuation lineでは新規リストを追加しない
   ((and
    (memq markdown-indent-on-enter '(indent-and-new-item))
    (markdown-list-item-at-point-p)
    (save-excursion
      (beginning-of-line)
      (not (looking-at-p markdown-regex-list))))
    (newline)
    (markdown-indent-line))
   ;; markdown-hide-markupが有効で見出し本文の先頭の場合
   ;; 見出し記号を無視して改行を行う
   ((and
     markdown-hide-markup
     (save-excursion
       (beginning-of-line)
       (looking-at markdown-regex-header-atx))
     (= (match-beginning 2) (point)))
    (save-excursion
      (beginning-of-line)
      (newline)))
   (t
    (apply original args))))

(advice-add 'markdown-enter-key :around #'markdown-enter-key-advice)

(evil-define-key 'hybrid markdown-mode-map
  (kbd "SPC") 'markdown-insert-space-context
  (kbd "DEL") 'markdown-backspace-context
  (kbd "C-a") 'markdown-beginning-of-line
  (kbd "C-h") 'markdown-backspace-context)

(evil-define-key 'normal markdown-mode-map
  (kbd "I") 'evil-markdown-insert-line)

(provide 'markdown-special-keys)
;;; markdown-special-keys.el ends here
