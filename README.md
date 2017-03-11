# flow-minor-mode

Major mode for [flow](http://flowtype.org), derived from
`web-mode`. Essentially a rewrite of an official
[flow-for-emacs](https://github.com/flowtype/flow-for-emacs) snippet
into a standalone mode with an improved usability.

## Setup

To enable this mode, enable it in your preferred JavaScript mode's
hooks:

```lisp
(add-hook 'js2-mode-hook 'flow-enable-automatically)
````

This will enable `flow-minor-mode` for a file only when there is a
`//@flow` declaration at the first line. If you wish to enable
flow-minor-mode for all javascript files, use this instead:

```lisp
(add-hook 'js2-hook 'flow-minor-mode)
```

## Additional integrations

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
