(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


(add-to-list 'load-path "/home/jan/gits/stack-ide/stack-mode/")
(require 'stack-mode)
(add-hook 'haskell-mode-hook 'stack-mode)
