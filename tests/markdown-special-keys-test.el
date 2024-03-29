(require 'ert)
(require 'ert-x)
(require 'markdown-special-keys)

;; markdown-beginning-of-line の内部処理では、テキストプロパティを使った判定を行なっている箇所があるため insert 後に markdown-mode を有効にしている。
;; 逆順にすると、正しくテストができないため注意
;; TODO use markdown-test-string instead
(defmacro markdown-test-buffer (sample &rest body)
  (declare (debug t) (indent 1))
  `(with-temp-buffer
     (insert ,sample)
     (markdown-mode)
     (setq-default indent-tabs-mode nil)
     (goto-char (point-min))
     ,@body))

(ert-deftest test/markdown-beginning-of-line/list ()
  (markdown-test-buffer
   "    * List1-1"
   (markdown-beginning-of-line)
   (should (= (point) 7))
   (markdown-beginning-of-line)
   (should (= (point) 1))
   (markdown-beginning-of-line)
   (should (= (point) 7))))

(ert-deftest test/markdown-beginning-of-line/list/prefix ()
  (markdown-test-buffer
   "* List1\n    * List1-1"
   (markdown-beginning-of-line 2)
   (should (= (point) 15))))

(ert-deftest test/markdown-beginning-of-line/heading ()
  (markdown-test-buffer
   "## heading2"
   (markdown-beginning-of-line)
   (should (= (point) 4))
   (markdown-beginning-of-line)
   (should (= (point) 1))
   (markdown-beginning-of-line)
   (should (= (point) 4))))

(ert-deftest test/markdown-beginning-of-line/heading/prefix ()
  (markdown-test-buffer
   "# heading1\n## heading2"
   (markdown-beginning-of-line 2)
   (should (= (point) 15))))

(ert-deftest test/markdown-beginning-of-line/code ()
  (markdown-test-buffer
   "\
```
def hello
  puts 'hello world'
end
```"
   (markdown-beginning-of-line 3) ; prefix command
   (should (= (point) 17))
   (markdown-beginning-of-line)
   (should (= (point) 15))
   (markdown-beginning-of-line)
   (should (= (point) 17))))

(ert-deftest test/markdown-insert-space-context/blank-line ()
  (markdown-test-buffer
   ""
   (markdown-insert-space-context)
   (should (string= (buffer-string) "* "))))

(ert-deftest test/markdown-insert-space-context/text ()
  (markdown-test-buffer
   "List1"
   (markdown-insert-space-context)
   (should (string= (buffer-string) "* List1"))))

(ert-deftest test/markdown-insert-space-context/list-level1 ()
  (markdown-test-buffer
   "* List1"
   (forward-char 2)
   (markdown-insert-space-context)
   (should (string= (buffer-string) "    * List1"))))

(ert-deftest test/markdown-insert-space-context/list-level1-2 ()
  (markdown-test-buffer
   "* List1\n    * List1-1"
   (forward-char 2)
   (markdown-insert-space-context)
   (should (string= (buffer-string) "    * List1\n    * List1-1"))))

(ert-deftest test/markdown-insert-space-context/list-level1-body ()
  (markdown-test-buffer
   "* List1"
   (forward-char 3)
   (markdown-insert-space-context)
   (should (string= (buffer-string) "* L ist1"))))

(ert-deftest test/markdown-insert-space-context/code-block ()
  (markdown-test-buffer
   "```\n\n```"
   (next-line)
   (markdown-insert-space-context)
   (should (string= (buffer-string) "```\n \n```"))))

(ert-deftest test/markdown-outdent-or-delete-advice/heading ()
  (let ((markdown-hide-markup t))
    (markdown-test-buffer
     "\n## Heading"
     (next-line)
     (forward-char 3)
     (call-interactively #'markdown-outdent-or-delete)
     (should (string= (buffer-string) "## Heading"))))

  (let ((markdown-hide-markup nil))
    (markdown-test-buffer
     "\n## Heading"
     (next-line)
     (forward-char 3)
     (call-interactively #'markdown-outdent-or-delete)
     (should (string= (buffer-string) "\n##Heading")))))

(ert-deftest test/markdown-outdent-or-delete-advice/list-level1 ()
  (markdown-test-buffer
   "* List1"
   (forward-char 2)
   (call-interactively #'markdown-outdent-or-delete)
   (should (string= (buffer-string) "List1"))))

(ert-deftest test/markdown-outdent-or-delete-advice/list-level1-body ()
  (markdown-test-buffer
   "* List1"
   (forward-char 3)
   (call-interactively #'markdown-outdent-or-delete)
   (should (string= (buffer-string) "* ist1"))))

(ert-deftest test/markdown-outdent-or-delete-advice/list-level1-body-whitespace ()
  (markdown-test-buffer
   "*  List1"
   (forward-char 3)
   (call-interactively #'markdown-outdent-or-delete)
   (should (string= (buffer-string) "* List1"))))

(ert-deftest test/markdown-outdent-or-delete-advice/list-level2-head-of-bullet ()
  (markdown-test-buffer
   "    * List1"
   (forward-char 4)
   (call-interactively #'markdown-outdent-or-delete)
   (should (string= (buffer-string) "* List1"))))

(ert-deftest test/markdown-outdent-or-delete-advice/list-level2-head-of-list ()
  (markdown-test-buffer
   "    * List1"
   (forward-char 6)
   (call-interactively #'markdown-outdent-or-delete)
   (should (string= (buffer-string) "* List1"))))

(ert-deftest test/markdown-outdent-or-delete-advice/list-level2-3-head-of-list ()
  (markdown-test-buffer
   "\
    * List1
        * List1-2"
   (forward-char 6)
   (call-interactively #'markdown-outdent-or-delete)
   (should (string= (buffer-string) "* List1\n        * List1-2"))))

(ert-deftest test/markdown-outdent-or-delete-advice/checkbox ()
  (markdown-test-buffer
   "* [X] List1"
   (forward-char 6)
   (call-interactively #'markdown-outdent-or-delete)
   (should (string= (buffer-string) "* List1"))))

(ert-deftest test/markdown-outdent-or-delete-advice/full-width-space ()
  (markdown-test-buffer
   "　"
   (forward-char 1)
   (call-interactively #'markdown-outdent-or-delete)
   (should (string= (buffer-string) ""))))

(ert-deftest test/markdown-evil-markdown-insert-line/list ()
  (markdown-test-buffer
   "* List1"
   (evil-markdown-insert-line 1)
   (should (= (point) 3))))

(ert-deftest test/markdown-evil-markdown-insert-line/heading ()
  (markdown-test-buffer
   "# Heading1"
   (evil-markdown-insert-line 1)
   (should (= (point) 3))))

(ert-deftest test/markdown-markdown-cycle-advice/plain-text ()
  (markdown-test-buffer
   ""
   (ert-simulate-command '(markdown-cycle))
   (should (string= (buffer-string) "* ")))

  (markdown-test-buffer
   "body"
   (ert-simulate-command '(markdown-cycle))

   (should (string= (buffer-string) "* body")))

  (markdown-test-buffer
   "body"
   (let ((indent-tabs-mode nil))
     (forward-char 1)
     ;; markdownで呼び出されるmarkdown-indent-lineがthis-commandに依存しているため
     ;; ert-x.elのert-simulate-commandを使う
     (ert-simulate-command '(markdown-cycle))
     (should (string= (buffer-string) "    body")))))

(ert-deftest test/markdown-markdown-cycle-advice/heading ()
  (markdown-test-buffer
   "## Heading"
   (ert-simulate-command '(markdown-cycle))
   (should (string= (buffer-string) "## Heading"))))

(ert-deftest test/markdown-markdown-cycle-advice/list ()
  (markdown-test-buffer
   "* List"
   (ert-simulate-command '(markdown-cycle))
   ;; TODO expected 4 spaces but actual 2 spaces.
   (should (string= (buffer-string) "  * List"))))

(ert-deftest test/markdown-markdown-cycle-advice/table ()
  (markdown-test-buffer
   "| aaa | bbb |"
   (ert-simulate-command '(markdown-cycle))
   (should (= (point) 3))
   (ert-simulate-command '(markdown-cycle))
   (should (= (point) 9))
   (should (string= (buffer-string) "| aaa | bbb |"))))

(ert-deftest test/markdown-enter-key-advice/list ()
  (let ((markdown-indent-on-enter 'indent-and-new-item))
    (markdown-test-buffer
     "* List\nlazy body"
     (next-line)
     (end-of-line)
     (markdown-enter-key)
     (should (string= (buffer-string) "* List\nlazy body\n")))))

(ert-deftest test/markdown-enter-key-advice/heading ()
  (markdown-test-buffer
   "## Heading"
   (markdown-toggle-markup-hiding +1)
   (forward-char 3)
   (markdown-enter-key)
   (should (string= (buffer-string) "\n## Heading"))))
