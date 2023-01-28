(require 'ert)
(require 'markdown-special-keys)

(defvar markdown-test-list-sample "\
* List1
  * List1-1
* List2
  * List2-1
  * List2-2
* List3")

(defvar markdown-test-heading-sample "\
# Heading1
## Heading1-1
textbody

## Heading1-2
textbody")

(defvar markdown-test-code-sample "\
```
class Greeting
  def hello
    puts 'hello world'
  end
end
```
")

;; mwim のテストケースを参考にmacroを作成
;; markdown-beginning-of-line の内部処理では、テキストプロパティを使った判定を行なっている箇所があるため insert 後に markdown-mode を有効にしている。
;; 逆順にすると、正しくテストができないため注意
(defmacro mwim-test-with-sample (sample &rest body)
  (declare (debug t) (indent 1))
  `(with-temp-buffer
     (insert ,sample)
     (markdown-mode)
     (goto-char (point-min))
     ,@body))

(ert-deftest markdown-test-beginning-of-line/list ()
  (mwim-test-with-sample markdown-test-list-sample
    (markdown-beginning-of-line)
    (should (= (point) 3))
    (markdown-beginning-of-line)
    (should (= (point) 1))
    (next-line)
    (markdown-beginning-of-line)
    (should (= (point) 13))
    (markdown-beginning-of-line)
    (should (= (point) 11))
    (markdown-beginning-of-line)
    (should (= (point) 9))
    (markdown-beginning-of-line)
    (should (= (point) 13))
    ))

(ert-deftest markdown-test-beginning-of-line/heading ()
  (mwim-test-with-sample markdown-test-heading-sample
    (markdown-beginning-of-line)
    (should (= (point) 3))
    (markdown-beginning-of-line)
    (should (= (point) 1))
    (next-line)
    (markdown-beginning-of-line)
    (should (= (point) 15))
    (markdown-beginning-of-line)
    (should (= (point) 12))
    (markdown-beginning-of-line)
    (should (= (point) 15))
    ))

(ert-deftest markdown-test-beginning-of-line/code ()
  (mwim-test-with-sample markdown-test-code-sample
    (next-line 2)
    (markdown-beginning-of-line)
    (should (= (point) 22))
    (markdown-beginning-of-line)
    (should (= (point) 20))
    (markdown-beginning-of-line)
    (should (= (point) 22))
    ))

(ert-deftest markdown-test-beginning-of-line/code/prefix ()
  (mwim-test-with-sample markdown-test-code-sample
    (markdown-beginning-of-line 4)
    (should (= (point) 57))
    ))
