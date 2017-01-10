# helm-flx

[![MELPA](http://melpa.org/packages/helm-flx-badge.svg)](http://melpa.org/#/helm-flx)

This package implements intelligent helm fuzzy sorting, provided by [`flx`](https://github.com/lewang/flx).

You can install the package by either cloning it yourself, or by doing <kbd>M-x</kbd> `package-install` <kbd>RET</kbd> `helm-flx` <kbd>RET</kbd>.

After that, you can enable it by putting the following in your init file:

```emacs
(helm-flx-mode +1)
```

In addition, there are two important configuration variables:

```emacs
(setq helm-flx-for-helm-find-files t ;; t by default
      helm-flx-for-helm-locate t) ;; nil by default
```

Some History
============

Crazy stuff simmers in my [`emacs-config`](https://github.com/PythonNut/emacs-config), and every once and a while, it's worth sharing. [This was no exception](https://github.com/PythonNut/emacs-config/blob/f1df3ac16410bfa72d88855325bd6c2de56f587b/modules/config-helm.el#L33#L89). It's been patiently evolving in my config for the better part of a year, and I think it's useful enough to benefit other people.

Note that `flx`'s author [**@lewang**](https://github.com/lewang) did start work on his own port of `helm-flx`, but it appears to have been abandoned. (Left unfinished without modification for nine months.) If, at any time, lewang can find time to contribute again, (his implementation is significantly more efficient than mine) I would be glad.

Caveats
=======

 * Doesn't work for sources that don't already support fuzzy matching
