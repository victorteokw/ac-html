(require 'ac-html)

;;; Core test

(defun test-suite-summary (item items)
  "Helper. Popup summary of ITEM in list ITEMS"
  (popup-item-summary (car (member item items))))

(defun test-suite-documentaion (item items)
  "Helper. Popup document of ITEM in list ITEMS"
  (popup-item-documentation (car (member item items))))

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
     (should (equal (ac-html-current-tag) "head")))))
