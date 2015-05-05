(require 'ac-html)

;;; Core test

(ert-deftest test-ac-html--flatten-returns-nil ()
  "`ac-html--flatten' should return nil when argument is nil."
  (should (null (ac-html--flatten nil))))

(ert-deftest test-ac-html--flatten-returns-list ()
  "`ac-html--flatten' should return list of argument when argument is atom."
  (should (equal (ac-html--flatten "wtf") '("wtf"))))

(ert-deftest test-ac-html--flatten-returns-flatten-list ()
  "`ac-html--flatten' should return list of argument when argument is atom."
  (should (equal (ac-html--flatten
                  '(1 2 (3 (((4))) 5 ((6)) 7) 8)) '(1 2 3 4 5 6 7 8))))

(setq ac-html-source-dirs (list (cons "html01" (expand-file-name "test/fixtures/01" ac-html-package-dir))
				(cons "html02" (expand-file-name "test/fixtures/02" ac-html-package-dir))))

(defun test-suite-summary (item items)
  "Helper. Popup summary of ITEM in list ITEMS"
  (popup-item-summary (car (member item items))))

(defun test-suite-documentaion (item items)
  "Helper. Popup document of ITEM in list ITEMS"
  (popup-item-documentation (car (member item items))))

;; tags

(ert-deftest test-ac-html-tags ()
  "Test `ac-html--tags'.
It should return correctly."
  (let* ((tag-list (ac-html--tags)))
    (should-not (equal tag-list '()))
    (should-not (member "div123" tag-list))
    (should (member "div" tag-list))
    (should (member "a" tag-list))))

(ert-deftest test-ac-html-tags-summary ()
  "Summary of tags test"
  (let* ((tag-list (ac-html--tags)))
    (should (string= "html01" (test-suite-summary "uniq01tag" tag-list)))
    (should (string= "html02" (test-suite-summary "uniq02tag" tag-list)))))

(ert-deftest test-ac-html-tags-summary-override ()
  "There are divs is in both html source dirs,
 test/fixtures/01/html-tag-list and
 test/fixtures/02/html-tag-list 

But fixtures/02 is second in list `web-completion-data-sources' and summary must be html02.
Other \"XXXX-override\" test are same thing.
"
  (let* ((tag-list (ac-html--tags)))
    (should (string= "html02" (test-suite-summary "div" tag-list)))))

(ert-deftest test-ac-html-tags-doc-inline ()
  "Inline documentation test"
  (let* ((tag-list (ac-html--tags)))
    (should (string= "inline <p> doc" (test-suite-documentaion "body" tag-list)))))

(ert-deftest test-ac-html-tags-doc-nodoc ()
  "If tag have no documantaion, nil returned"
  (let* ((tag-list (ac-html--tags)))
    (should (eq nil (test-suite-documentaion "fake_tag" tag-list)))))

(ert-deftest test-ac-html-tags-doc-override ()
  "There tag <p> have inline document in in both html source dirs,

Must be documentation of second fixture directory"
  (let* ((tag-list (ac-html--tags)))
    (should (string= "inline <p> doc, 02 fixture" (test-suite-documentaion "p" tag-list)))))

(ert-deftest test-ac-html-tags-doc-in-external-file ()
"Documentation about tag may be defined in file html-tag-short-docs/TAGNAME

But inline documentaion (inside \"html-tag-list\" file) have first priority"
  (let* ((tag-list (ac-html--tags)))
    ;; inline doc vs  external file priority test. Inline have priority
    (should (string= "inline <p> doc" (test-suite-documentaion "body" tag-list)))
    ;; external file
    (should (string= "article doc" (test-suite-documentaion "article" tag-list)))))

;; attribute

(ert-deftest test-ac-html-attribute ()
  "Global and concrete attributes of tag <body>"
  (should-not (member "onload123" (ac-html--attribute-candidates "body" "")))
  (should (member "onload" (ac-html--attribute-candidates "body" "")))
  ;; global
  (should (member "class" (ac-html--attribute-candidates "body" "")))
  (should (member "id" (ac-html--attribute-candidates "body" ""))))

(ert-deftest test-ac-html-attribute-global ()
  "Global attribute for any tag"
  (should (member "class" (ac-html--attribute-candidates "fake_tag" ""))))

(ert-deftest test-ac-html-attribute-summary ()
  "Summary of attributes"
  (should (string= "html01, G" (test-suite-summary "class" (ac-html--attribute-candidates "body" ""))))
  (should (string= "html01" (test-suite-summary "onload" (ac-html--attribute-candidates "body" ""))))
  (should (string= "html02, G" (test-suite-summary "id" (ac-html--attribute-candidates "body" "")))))

(ert-deftest test-ac-html-attribute-summary-ovveride ()
  (should (string= "html02, G" (test-suite-summary "id" (ac-html--attribute-candidates "body" "")))))

(ert-deftest test-ac-html-attribute-doc-inline ()
  "Documentaion of attributes"
  (should (string= "global inline class doc" (popup-item-documentation
				  (car (member "class" (ac-html--attribute-candidates "fake_tag" "no-doc"))))))
  ;; default test
  (should (string= "no-doc" (popup-item-documentation
				  (car (member "id" (ac-html--attribute-candidates "fake_tag" "no-doc"))))))
  )

(ert-deftest test-ac-html-attribute-doc-override ()
  "Documentaion of attributes may be overrided in external file or inline, but inline have priority

Use `ac-html--attribute-documentation' fun"
  ;; inline priority test
  (should (string= "inline doc for onstorage" (popup-item-documentation
				  (car (member "onstorage" (ac-html--attribute-candidates "body" "no-doc"))))))
  ;; we have 2 documentaion for body onredo in both sources, secondary must be ok
  (should (string= "external doc for onredo, fixture 02" (popup-item-documentation
				  (car (member "onredo" (ac-html--attribute-candidates "body"
										       #'(lambda (symbol)
											   (ac-html--attribute-documentation "onredo" "body")))))))))

;; attribute values

(ert-deftest test-ac-html-value ()
  "Values of attributes. `ac-source--html-values-internal'"
  (should (equal '() (ac-source--html-values-internal "body" "fake_attribute")))
  (should (member "ltr" (ac-source--html-values-internal "body" "dir"))))

(ert-deftest test-ac-html-value-summary ()
  (should (string= "html01, G" (test-suite-summary "ltr" (ac-source--html-values-internal "body" "dir")))))

(ert-deftest test-ac-html-value-doc ()
  "Documentaion for value"
  ;; undocument
  (should (equal nil (test-suite-documentaion "rtr" (ac-source--html-values-internal "body" "dir"))))
  ;; inline doc
  (should (string= "left to right" (test-suite-documentaion "ltr" (ac-source--html-values-internal "body" "dir")))))



;;; HTML test

(defun test-suite-html-current-thing-scan (body)
  "This test suite is for
`ac-html--current-html-tag'
`ac-html--current-html-attribute'."
  (unwind-protect
      (progn
        (with-temp-buffer
          (insert "<html><head lang=\"\"\n\n lang=\"\"></head></html>")
          (funcall body)))))

(ert-deftest test-ac-html--current-html-tag ()
  "Test `ac-html--current-html-tag' correctly scan the tag user is typing on."
  (test-suite-html-current-thing-scan
   (lambda ()
     (goto-char 20)
     (should (equal (ac-html--current-html-tag) "head")))))

(ert-deftest test-ac-html--current-html-attribute ()
  "Test `ac-html--current-html-attribute' correctly scan the attribute."
  (test-suite-html-current-thing-scan
   (lambda ()
     (goto-char 29)
     (should (equal (ac-html--current-html-attribute) "lang")))))
