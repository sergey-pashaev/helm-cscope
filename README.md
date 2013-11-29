# helm-cscope
Use xcscope with helm! This is pretty straightforward port of anything-cscope.el from [here](https://github.com/vexus2/dotfiles/blob/master/.elisp/anything-cscope.el)

# dependencies
* [cscope](http://cscope.sourceforge.net/)
* `helm` (available on MELPA)
* `xcscope` (available on MELPA)

# install
1. Install all dependencies
2. Put helm-cscope.el in your load path: `(add-to-list 'load-path "/<path to helm-cscope dir here>/")`
3. Add `(require 'helm-cscope)` to your .emacs

# interactive functions
* helm-cscope-find-symbol
* helm-cscope-find-global-definition
* helm-cscope-find-called-function
* helm-cscope-find-calling-this-funtcion
* helm-cscope-select (uses all of above sources)

# example configuration
```elisp
;; Enable helm-cscope-mode
(add-hook 'c-mode-hook 'helm-cscope-mode)
(add-hook 'c++-mode-hook 'helm-cscope-mode)
;; Set key bindings
(eval-after-load "helm-cscope"
  '(progn
     (define-key helm-cscope-mode-map (kbd "M-t") 'helm-cscope-find-symbol)
     (define-key helm-cscope-mode-map (kbd "M-r") 'helm-cscope-find-global-definition)
     (define-key helm-cscope-mode-map (kbd "M-g M-c") 'helm-cscope-find-called-function)
     (define-key helm-cscope-mode-map (kbd "M-g M-p") 'helm-cscope-find-calling-this-funtcion)
     (define-key helm-cscope-mode-map (kbd "M-s") 'helm-cscope-select)))
```
