ac-html
=======

Emacs auto complete source for html tag and attributes.

Vision
------
Provide accurate and intelligent auto completion to HTML and css.
Support haml and slim.
Add boolean attribute recognition.
For non-boolean attribute, auto complete ``=""`` and posit cursor in the middle of quotes.
Add class source and auto complete class names.
Add id source and auto complete id names.

Issues
------
It may contain some bugs.

Usage
-----

### If you are using html-mode:

Add these lines
``` elisp
(add-to-list 'ac-sources 'ac-source-html-tag)
(add-to-list 'ac-sources 'ac-source-html-attribute)
(add-to-list 'ac-sources 'ac-source-html-attribute-2)
```

### If you are using web-mode:
Additionally you need to add these lines:
``` elisp
(add-to-list 'web-mode-ac-sources-alist
  '("html" . (ac-source-html-tag
		          ac-source-html-attribute
		          ac-source-html-attribute-2)))
```
