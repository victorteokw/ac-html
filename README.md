ac-html
=======

Emacs auto complete source for html tag and attributes.

Watch [Screencast](https://www.youtube.com/watch?v=UrXNgrN4d5Y).

Vision
------
Provide accurate and intelligent auto completion to HTML and css.

Support haml and slim.

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
```

### If you are using web-mode:
Additionally you need to add these lines:
``` elisp
(add-to-list 'web-mode-ac-sources-alist
  '("html" . (ac-source-html-tag
		          ac-source-html-attribute)))
```
