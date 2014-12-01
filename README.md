ac-html
=======

Emacs auto complete source for html tag and attributes.

Watch [Screencast](https://www.youtube.com/watch?v=UrXNgrN4d5Y).

Vision
------
Provide accurate and intelligent auto completion to HTML and css.

Support slim.

Add class source and auto complete class names.

Add id source and auto complete id names.

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

### If you are using haml-mode:
use \`ac-source-haml-tag' and \`ac-source-haml-attrubute'

Issues
------
It may contain some bugs.

Contribution
------
Any contribution is welcome.
Requested features and bad bugs are listed in issues.
Also, setting up test is needed.
