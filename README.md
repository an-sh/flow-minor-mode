# Flow mode

Major mode for [flow](http://flowtype.org), derived from
`web-mode`. Essentially a rewrite of an official
[flow-for-emacs](https://github.com/flowtype/flow-for-emacs) snippet
into a standalone mode with an improved usability.

## Additional setup

Auto mode for `.flow.js` files:

```
(add-to-list 'auto-mode-alist '("\\.flow.js\\'" . flow-mode))
```

### flycheck-flow

```lisp
(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-flow 'flow-mode)
  (flycheck-add-mode 'javascript-eslint 'flow-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))
```

### company-flow

```lisp
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-flow))
```

### xref (build-in)

Use standard Emacs 25.1 xref keys (`M-.` and `M-,`) for jumping to
definitions.
