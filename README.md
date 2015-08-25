ac-html [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link] [![Build Status](https://travis-ci.org/cheunghy/ac-html.png?branch=master)](https://travis-ci.org/cheunghy/ac-html)
=======

Emacs auto complete source for html, slim, haml, and jade.

Watch [Screencast](https://www.youtube.com/watch?v=UrXNgrN4d5Y).

Upgrade Guide
------

### About Version 0.4

Version 0.4 is a big advance, the code are refactored to improve extensibility and flexibility.

In the future, per framework completion data is easy to integrate into the big completion engine.

Since it's a big advance forward, it causes compatibility issue.

See configuration for how to setup.

Data Provider
------

ac-html now has data provider mechanism. You can define your own data provider, and hook the data into ac-html engine.

A data provider can provide data for html tag, attribute, attribute value, id and class.

Language (Adaptor)
------

An adaptor is hooked into a specific language mode, a adaptor need to provide how to find current tag and attribute, when to trigger tag or attribute completion.

Current available adators are ac-source-html, ac-source-jade, ac-source-haml, ac-source-slim.

Configuration
-----

### web-mode:

``` elisp
(add-to-list 'web-mode-ac-sources-alist
             '("html" . (ac-source-html-tag
                         ac-source-html-attr
                         ac-source-html-attrv)))
```

### Support for template languages:
You may selectively use:
``` elisp
(add-hook 'haml-mode-hook 'ac-haml-enable)
(add-hook 'jade-mode-hook 'ac-jade-enable)
(add-hook 'slim-mode-hook 'ac-slim-enable)
```

Related projects
-----

You may be interested in next projects:
- [ac-html-bootstrap](https://github.com/osv/ac-html-bootstrap) - Twitter:Bootstrap support for ac-html.
- [ac-html-csswatcher](https://github.com/osv/ac-html-csswatcher) - CSS/Less support for ac-html

Issues
------

There are some bugs.

If you find, please report or fix it via issues.


Contributors
------

This package is originally authored and maintained by Zhang Kai Yu.

The biggest contributors are:

- Olexandr Sydorchuk

Many thanks to you.


[melpa-link]: http://melpa.org/#/ac-html
[melpa-badge]: http://melpa.org/packages/ac-html-badge.svg
[melpa-stable-link]: http://stable.melpa.org/#/ac-html
[melpa-stable-badge]: http://stable.melpa.org/packages/ac-html-badge.svg
