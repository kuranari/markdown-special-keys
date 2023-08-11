(require 'ert)
(require 'ert-x)
(require 'markdown-special-keys)

(defvar markdown-test-blank-buffer-sample "")

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
     (setq markdown-list-indent-width 2)
     (setq markdown-hide-markup nil)
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
    (should (= (point) 9))
    (markdown-beginning-of-line)
    (should (= (point) 13))
    ))

(ert-deftest markdown-test-beginning-of-line/list/prefix ()
  (mwim-test-with-sample markdown-test-list-sample
    (markdown-beginning-of-line 2)
    (should (= (point) 13))))

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

(ert-deftest markdown-test-beginning-of-line/heading/prefix ()
  (mwim-test-with-sample "# heading1\n## heading2"
                         (markdown-beginning-of-line 2)
                         (should (= (point) 15))))

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
    (should (= (point) 36))))

(ert-deftest markdown-test-insert-space-context/blank-line ()
  (mwim-test-with-sample
      ""
   (markdown-insert-space-context)
   (should (equal (buffer-substring-no-properties (point-min) (point-max)) "* "))))

(ert-deftest markdown-test-insert-space-context/text ()
  (mwim-test-with-sample
      "List1"
    (markdown-insert-space-context)
    (should (equal (buffer-substring-no-properties (point-min) (point-max)) "* List1"))))

(ert-deftest markdown-test-insert-space-context/list-level1 ()
  (mwim-test-with-sample
      "* List1"
    (forward-char 2)
    (markdown-insert-space-context)
    (should (equal (buffer-substring-no-properties (point-min) (point-max)) "  * List1"))))

(ert-deftest markdown-test-insert-space-context/list-level1-2 ()
  (mwim-test-with-sample "\
* List1
  * List1-2"
   (forward-char 2)
   (markdown-insert-space-context)
   (should (equal (buffer-substring-no-properties (point-min) (point-max)) "  * List1\n  * List1-2"))))

(ert-deftest markdown-test-insert-space-context/list-level1-body ()
  (mwim-test-with-sample
      "* List1"
    (forward-char 3)
    (markdown-insert-space-context)
    (should (equal (buffer-substring-no-properties (point-min) (point-max)) "* L ist1"))))

(ert-deftest markdown-test-insert-space-context/code-block ()
  (mwim-test-with-sample "```\n\n```"
    (next-line)
    (markdown-insert-space-context)
    (should (equal (buffer-substring-no-properties (point-min) (point-max)) "```\n \n```"))))


(ert-deftest markdown-test-backspace-context/list-level1 ()
  (mwim-test-with-sample
      "* List1"
    (forward-char 2)
    (markdown-backspace-context)
    (should (equal (buffer-substring-no-properties (point-min) (point-max)) "List1"))))

(ert-deftest markdown-test-backspace-context/list-level1-body ()
  (mwim-test-with-sample
      "* List1"
    (forward-char 3)
    (markdown-backspace-context)
    (should (equal (buffer-substring-no-properties (point-min) (point-max)) "* ist1"))))

(ert-deftest markdown-test-backspace-context/list-level1-body-whitespace ()
  (mwim-test-with-sample
   "*  List1"
   (forward-char 3)
   (markdown-backspace-context)
   (should (equal (buffer-substring-no-properties (point-min) (point-max)) "* List1"))))

(ert-deftest markdown-test-backspace-context/list-level2-head-of-bullet ()
  (mwim-test-with-sample
      "  * List1"
    (forward-char 2)
    (markdown-backspace-context)
    (should (equal (buffer-substring-no-properties (point-min) (point-max)) "* List1"))))

(ert-deftest markdown-test-backspace-context/list-level2-head-of-list ()
  (mwim-test-with-sample
      "  * List1"
    (forward-char 4)
    (markdown-backspace-context)
    (should (equal (buffer-substring-no-properties (point-min) (point-max)) "* List1"))))

(ert-deftest markdown-test-backspace-context/list-level2-3-head-of-list ()
  (mwim-test-with-sample "\
  * List1
    * List1-2"
   (forward-char 4)
   (markdown-backspace-context)
   (should (equal (buffer-substring-no-properties (point-min) (point-max)) "* List1\n    * List1-2"))))

(ert-deftest markdown-test-backspace-context/full-width-space ()
  (mwim-test-with-sample
   "　"
   (forward-char 1)
   (markdown-backspace-context)
   (should (equal (buffer-substring-no-properties (point-min) (point-max)) ""))))

(ert-deftest markdown-test-evil-markdown-insert-line/list ()
  (mwim-test-with-sample
   markdown-test-list-sample
   (evil-markdown-insert-line 1)
   (should (= (point) 3))))

(ert-deftest markdown-test-evil-markdown-insert-line/heading ()
  (mwim-test-with-sample
   markdown-test-heading-sample
   (evil-markdown-insert-line 1)
   (should (= (point) 3))))

(ert-deftest markdown-test-markdown-cycle-advice/plain-text ()
  (mwim-test-with-sample ""
   (ert-simulate-command '(markdown-cycle))
   (should (equal (buffer-substring-no-properties (point-min) (point-max)) "* ")))

  (mwim-test-with-sample "body"
   (ert-simulate-command '(markdown-cycle))

   (should (equal (buffer-substring-no-properties (point-min) (point-max)) "* body")))

  (mwim-test-with-sample "body"
   (let ((indent-tabs-mode nil))
     (forward-char 1)
     ;; markdownで呼び出されるmarkdown-indent-lineがthis-commandに依存しているため
     ;; ert-x.elのert-simulate-commandを使う
     (ert-simulate-command '(markdown-cycle))
     (should (equal (buffer-substring-no-properties (point-min) (point-max)) "    body")))))

(ert-deftest markdown-test-markdown-cycle-advice/heading ()
  (mwim-test-with-sample "## Heading"
   (ert-simulate-command '(markdown-cycle))
   (should (equal (buffer-substring-no-properties (point-min) (point-max)) "## Heading"))))

(ert-deftest markdown-test-markdown-cycle-advice/list ()
  (mwim-test-with-sample "* List"
   (ert-simulate-command '(markdown-cycle))
   (should (equal (buffer-substring-no-properties (point-min) (point-max)) "  * List"))))

(ert-deftest markdown-test-markdown-cycle-advice/table ()
  (mwim-test-with-sample "| aaa | bbb |"
   (ert-simulate-command '(markdown-cycle))
   (should (= (point) 3))
   (ert-simulate-command '(markdown-cycle))
   (should (= (point) 9))
   (should (equal (buffer-substring-no-properties (point-min) (point-max)) "| aaa | bbb |"))))

(ert-deftest markdown-test-enter-key-advice ()
  (let ((markdown-indent-on-enter 'indent-and-new-item))
    (mwim-test-with-sample
     "* List\nlazy body"
     (next-line)
     (end-of-line)
     (markdown-enter-key)
     (should (equal (buffer-substring-no-properties (point-min) (point-max)) "* List\nlazy body\n")))))
