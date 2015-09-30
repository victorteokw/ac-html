ac-html [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link] [![Build Status](https://travis-ci.org/cheunghy/ac-html.svg?branch=master)](https://travis-ci.org/cheunghy/ac-html)
=======

Emacs auto complete source for html, slim, haml, and jade.

Watch [Screencast](https://www.youtube.com/watch?v=UrXNgrN4d5Y).

Upgrade Guide
------

### About Version 0.4.alpha

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

This example setup completion for haml, same way should work for slim, html, jade, too.

``` elisp
(defun setup-ac-for-haml ()
  ;; Require ac-haml since we are setup haml auto completion
  (require 'ac-haml)
  ;; Require default data provider if you want to use
  (require 'ac-html-default-data-provider)
  ;; Enable data providers,
  ;; currently only default data provider available
  (ac-html-enable-data-provider 'ac-html-default-data-provider)
  ;; Let ac-haml do some setup
  (ac-haml-setup)
  ;; Set your ac-source
  (setq ac-sources '(ac-source-haml-tag
                     ac-source-haml-attr
                     ac-source-haml-attrv))
  ;; Enable auto complete mode
  (auto-complete-mode))

(add-hook 'haml-mode-hook 'setup-ac-for-haml)
```

For web mode you need additional

``` elisp
(add-to-list 'web-mode-ac-sources-alist
             '("html" . (ac-source-html-tag
                         ac-source-html-attr
                         ac-source-html-attrv)))
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
