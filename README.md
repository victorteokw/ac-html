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
(add-to-list 'ac-sources 'ac-source-html-attribute-value)
(add-to-list 'ac-sources 'ac-source-html-tag)
(add-to-list 'ac-sources 'ac-source-html-attribute)
```

### If you are using web-mode:
Additionally you need to add these lines:
``` elisp
(add-to-list 'web-mode-ac-sources-alist
             '("html" . (ac-source-html-attribute-value
                         ac-source-html-tag
                         ac-source-html-attribute)))
```

### If you are using haml-mode:
use \`ac-source-haml-tag' and \`ac-source-haml-attrubute'

### More customization

If you want fire completion as fast as it possible only for these ac sources
and for example change popup face, below hint help you:
``` elisp
(setq ac-source-html-tag
      (append
       '(
         (candidate-face . your-cool-face)
         (symbol . "Tag")
         (requires . 0)) ac-source-html-tag))

(setq ac-source-html-attribute
      (append
       '(
         (candidate-face . your-cool-face)
         (symbol . "ATR")
         (requires . 0)) ac-source-html-attribute))

(setq ac-source-html-attribute-value
      (append
       '(
         (candidate-face . your-cool-face)
         (symbol . "VAL")
         (requires . 0)) ac-source-html-attribute-value))
         
;; if you use `web-mode` and set `(requires . 0)`, you may want disable
;; auto-quotinng of web-mode because of bug popup.
;; `smartparens-mode` as example of auto-pair quote may be used.
(setq web-mode-enable-auto-quoting nil)
```

#### Event attribute

For event attribute completion you can use tern.js. Install tern via node.js `sudo npm install -g tern`,
and package `tern-auto-complete` 
Create `.tern-project` file in your project directory with `browser`:

```json
{
  "libs": [
    "browser",
  ],
    "plugins": {
    }
}
```

in your web-mode-hook add `ac-source-tern-completion` and enable tern-mode:


```lisp
(defun my-web-mode-hook ()
  "Hook for Web mode."
  (setq web-mode-ac-sources-alist
        '(("css" . (
                    ac-source-css-property))
          ("html" . (
                     ac-source-html-attribute-value
                     ac-source-html-tag
                     ac-source-html-attribute
                     ac-source-tern-completion))
          ("jsx" . (
                     ac-source-tern-completion))
          ("javascript" . (
                     ac-source-tern-completion))))
  (auto-complete-mode t)
  (tern-mode t))
```

Issues
------
It may contain some bugs.

Contribution
------
Any contribution is welcome.
Requested features and bad bugs are listed in issues.
