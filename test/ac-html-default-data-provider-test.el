(require 'f)
(require 'ac-html-default-data-provider)
(expectations
 (desc "web-completion-data-extension"
       (desc "web-completion-data-tag-list-file"
             (expect web-completion-data-tag-list-file
                     (f-expand "html-tag-list" web-completion-data-html-source-dir)))
       (desc "web-completion-data-tag-doc-dir"
             (expect web-completion-data-tag-doc-dir
                     (f-expand "html-tag-short-docs" web-completion-data-html-source-dir)))
       (desc "web-completion-data-tag-doc-file"
             (expect (web-completion-data-tag-doc-file "div")
                     (f-expand "div" web-completion-data-tag-doc-dir)))
       (desc "web-completion-data-attr-list-dir"
             (expect web-completion-data-attr-list-dir
                     (f-expand "html-attributes-list" web-completion-data-html-source-dir)))
       (desc "web-completion-data-attr-global-list-file"
             (expect web-completion-data-attr-global-list-file
                     (f-expand "global" web-completion-data-attr-list-dir)))
       (desc "web-completion-data-attr-list-file"
             (expect (web-completion-data-attr-list-file "meta")
                     (f-expand "meta" web-completion-data-attr-list-dir)))
       (desc "web-completion-data-attr-doc-dir"
             (expect web-completion-data-attr-doc-dir
                     (f-expand "html-attributes-short-docs"
                               web-completion-data-html-source-dir)))
       (desc "web-completion-data-attr-global-doc-file"
             (expect (web-completion-data-attr-global-doc-file "id")
                     (f-expand "global-id" web-completion-data-attr-doc-dir)))
       (desc "web-completion-data-attr-doc-file"
             (expect (web-completion-data-attr-doc-file "div" "class")
                     (f-expand "div-class" web-completion-data-attr-doc-dir)))
       (desc "web-completion-data-attrv-list-dir"
             (expect web-completion-data-attrv-list-dir
                     (f-expand "html-attrv-list" web-completion-data-html-source-dir)))
       (desc "web-completion-data-attrv-list-file"
             (expect (web-completion-data-attrv-list-file "a" "target")
                     (f-expand "a-target" web-completion-data-attrv-list-dir)))
       (desc "web-completion-data-attrv-global-list-file"
             (expect (web-completion-data-attrv-global-list-file "lang")
                     (f-expand "global-lang" web-completion-data-attrv-list-dir)))
       (desc "web-completion-data-attrv-doc-dir"
             (expect web-completion-data-attrv-doc-dir
                     (f-expand "html-attrv-docs" web-completion-data-html-source-dir)))
       (desc "web-completion-data-attrv-global-doc-file"
             (expect (web-completion-data-attrv-global-doc-file "lang" "en")
                     (f-expand "global-lang-en" web-completion-data-attrv-doc-dir)))
       (desc "web-completion-data-attrv-doc-file"
             (expect (web-completion-data-attrv-doc-file "script" "type" "text/javascript")
                     (f-expand (url-hexify-string "script-type-text/javascript")
                               web-completion-data-attrv-doc-dir))))
 (desc "tags completion"
       (desc "ac-html-default-tags"
             (expect (car (ac-html-default-tags))
                     "a")
             (expect (length (ac-html-default-tags))
                     147)
             (expect (nth 146 (ac-html-default-tags))
                     "xmp"))
       (desc "ac-html-default-attrs"
             (expect (car (ac-html-default-attrs "a"))
                     "download")
             (expect (length (ac-html-default-attrs "a"))
                     35)
             (expect (nth 34 (ac-html-default-attrs "a"))
                     "title"))
       (desc "ac-html-default-attrvs"
             (expect (car (ac-html-default-attrvs "a" "target"))
                     "_blank")
             (expect (length (ac-html-default-attrvs "a" "target"))
                     4))

       )

 )
