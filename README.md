ac-html
=======

Emacs auto complete source for html tag and attributes.

Watch [Screencast](https://www.youtube.com/watch?v=UrXNgrN4d5Y).

Vision
------
Provide accurate and intelligent auto completion to HTML and css.

Add class source and auto complete class names.

Add id source and auto complete id names.

Usage
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

### If you are using haml-mode and/or jade-mode:
Additionally you need to add these lines:
``` elisp
(add-hook 'haml-mode-hook 'ac-haml-enable)
(add-hook 'jade-mode-hook 'ac-jade-enable)
(add-hook 'slim-mode-hook 'ac-slim-enable)
```

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

;; additionally you may want configure
;; ac-source-haml-tag ac-source-haml-attribute ac-source-haml-attribute-value
;; and same for jade nad slim sources.

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
