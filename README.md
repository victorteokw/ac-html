ac-html [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link] [![Build Status](https://travis-ci.org/cheunghy/ac-html.png?branch=master)](https://travis-ci.org/cheunghy/ac-html)
=======

Emacs auto complete source for html and css.

Watch [Screencast](https://www.youtube.com/watch?v=UrXNgrN4d5Y).

Vision
------
Provide accurate and intelligent auto completion to HTML and css.

Configuration
-----

### If you are using html-mode:

Add these lines
``` elisp
(add-hook 'html-mode-hook 'ac-html-enable)
```

### If you are using web-mode:
Additionally you need to add these lines:
``` elisp
(add-to-list 'web-mode-ac-sources-alist
             '("html" . (ac-source-html-attribute-value
                         ac-source-html-tag
                         ac-source-html-attribute)))
```

### Support for template languages:
You may selectively add:
``` elisp
(add-hook 'haml-mode-hook 'ac-haml-enable)
(add-hook 'jade-mode-hook 'ac-jade-enable)
(add-hook 'slim-mode-hook 'ac-slim-enable)
```

Issues
------
It may contain some bugs.

If you find some bug, please report it via issues.


Contribution
------
Any contribution is welcome.

A list of contributors is listed [here](https://github.com/cheunghy/ac-html/graphs/contributors). Many thanks!

Requested features and bad bugs are listed in issues.

[melpa-link]: http://melpa.org/#/ac-html
[melpa-badge]: http://melpa.org/packages/ac-html-badge.svg
[melpa-stable-link]: http://stable.melpa.org/#/ac-html
[melpa-stable-badge]: http://stable.melpa.org/packages/ac-html-badge.svg
