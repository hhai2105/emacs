(add-to-list 'package-archives '("ORG" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("MELPA STABLE" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("MELPA" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("GNU ELPA" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package evil
	     :ensure t
	     :init
	     (setq evil-want-integration t)
	     (setq evil-want-keybinding nil)
	     (setq evil-vsplit-window-right t)
	     (setq evil-split-window-below t)
	     (evil-mode))
(use-package evil-collection
	     :after evil
	     :ensure t
	     :config
	     (evil-collection-init))

(use-package 'all-the-icons-dired
:ensure t)
(use-package 'all-the-icons-ibuffer
:ensure t)
(use-package 'all-the-icons-dired
:ensure t)
(use-package 'all-the-icons-dired
:ensure t)
(use-package 'all-the-icons-dired
:ensure t)

(use-package which-key
    :ensure t)
(which-key-mode)

(menu-bar-mode -1)
     (tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package doom-themes
	:ensure t)
(setq doom-themes-enable-bold t
    doom-themes-enable-italics t)
(load-theme 'doom-outrun-electric t)

(use-package doom-modeline
 :ensure t)
(doom-modeline-mode 1)

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

(set-face-attribute 'default nil
	:font "Noto Sans Mono 15"
      :weight 'medium)
    (set-face-attribute 'variable-pitch nil
	:font "Noto Sans Mono 15"
      :weight 'medium)
    (set-face-attribute 'fixed-pitch nil
	:font "Noto Sans Mono 15"
      :weight 'medium)
  ;;(setq-default line-spacing 0.10)
(add-to-list 'default-frame-alist '(font . "Noto Sans Mono 15"))

(use-package general
:ensure t
:config
(general-evil-setup t))
;; (nvmap :prefix "SPC"

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
