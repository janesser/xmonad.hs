;; If you don't have MELPA in your package archives:
(require 'package)
(add-to-list
  'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

;; Install Intero
(package-install 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)

;; Install vue-mode
;; http://wikemacs.org/wiki/Vuejs
(package-install 'vue-mode)
(package-install 'vue-html-mode)

(setq js-indent-level 2)
