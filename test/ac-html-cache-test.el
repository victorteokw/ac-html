;;; Cache, id and class test.

(require 'projectile)

(require 'ac-html-cache)

(ert-deftest test-ac-html-ensure-project ()
  "Test `ac-html-ensure-project' invoke function body or not."
  (should (equal 3 (ac-html-ensure-project (+ 1 2)))))

(ert-deftest test-ac-html-user-cache-dir ()
  "`ac-html-user-cache-dir' should return user's cache dir."
  (should (string= (ac-html-user-cache-dir)
                   (expand-file-name ".ac-html" user-emacs-directory)))
  (should (string-match "\.emacs\.d" (ac-html-user-cache-dir)))
  (should (string-match "\.ac-html" (ac-html-user-cache-dir)))
  (should (file-directory-p (ac-html-user-cache-dir))))

(ert-deftest test-ac-html-user-project-cache-dir ()
  "Test `ac-html-user-project-cache-dir' return correctly and auto create."
  (let ((path "/home/some/nice/path"))
    (should (string= (expand-file-name (md5 path) (ac-html-user-cache-dir))
                     (ac-html-user-project-cache-dir path)))
    (should (file-directory-p (ac-html-user-project-cache-dir path)))))

(ert-deftest test-ac-html-user-current-project-dir ()
  "This test is pending.")

(ert-deftest test-ac-html-user-current-project-cache-dir ()
  "This test is pending.")

(ert-deftest test-ac-html-html-modes ()
  "`ac-html-html-modes' variable should contain some modes."
  (should (memq 'web-mode ac-html-html-modes))
  (should (memq 'html-mode ac-html-html-modes))
  (should (memq 'haml-mode ac-html-html-modes))
  (should (memq 'jade-mode ac-html-html-modes))
  (should (memq 'slim-mode ac-html-html-modes))
  (should-not (memq 'css-mode ac-html-html-modes)))

(ert-deftest test-ac-html-css-modes ()
  "`ac-html-html-modes' variable should contain some modes."
  (should (memq 'css-mode ac-html-css-modes))
  (should (memq 'scss-mode ac-html-css-modes))
  (should (memq 'sass-mode ac-html-css-modes))
  (should (memq 'less-mode ac-html-css-modes))
  (should-not (memq 'web-mode ac-html-css-modes)))

;; Dummy project needed.
;; (ert-deftest test-ac-html-mode-index-list ()
;;   "`test-ac-html-mode-index-list' should return file list."
;;   (let ((index-list (ac-html-mode-index-list
;;                      '(ruby-mode python-mode)
;;                      dummy-project-dir)))
;;     (should (listp index-list))
;;     (should (consp index-list))
;; ))

(ert-deftest test-ac-html-user-html-files-index ()
  "pending.")

(ert-deftest test-ac-html-user-css-files-index ()
  "pending.")

(ert-deftest test-ac-html-cache-bundle-location ()
  "`ac-html-cache-bundle-location' should return correctly and auto
make directories."
  (let ((location (ac-html-cache-bundle-location
                   "index.html"
                   (expand-file-name "test" (ac-html-user-cache-dir)))))
    (message "I Love")
    (should (equal (expand-file-name "test/index.html"
                                     (ac-html-user-cache-dir))
                   location))
    (should (file-directory-p location))))

(ert-deftest test-ac-html-class-file-location ()
  "not needed.")

(ert-deftest test-ac-html-id-file-location ()
  "not needed.")

(ert-deftest test-ac-html-md5-file-location ()
  "not needed.")

(ert-deftest test-ac-html-file-full-name ()
  "`ac-html-file-full-name' should concat file name and proj dir."
  (should (string= (ac-html-file-full-name "index.html" "/pro")
                   "/pro/index.html")))

(ert-deftest test-ac-html-md5-of-file ()
  "pending.")

(ert-deftest test-ac-html-cached-md5-of-file ()
  "pending.")

(ert-deftest test-ac-html-hash-and-save-md5-file ()
  "pending.")

(ert-deftest test-ac-html-file-need-index ()
  "pending.")

(ert-deftest test-ac-html-hash-and-save-md5-file-if-needed ()
  "pending.")
