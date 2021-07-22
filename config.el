(require 'package)
(add-to-list 'package-archives '("ORG" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("MELPA STABLE" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("MELPA" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("GNU ELPA" . "http://elpa.gnu.org/packages/"))
(package-refresh-contents)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Using garbage magic hack.
(use-package gcmh
    :ensure t
    :config
    (gcmh-mode 1))
;; Setting garbage collection threshold
(setq gc-cons-threshold 402653184
    gc-cons-percentage 0.6)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
	(lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		    (format "%.2f seconds"
			    (float-time
			    (time-subtract after-init-time before-init-time)))
		    gcs-done)))

  ;; Silence compiler warnings as they can be pretty disruptive (setq comp-async-report-warnings-errors nil)

(setq backup-directory-alist '(("." . "~/.emacs.d/saves/")))

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

(use-package which-key
	:ensure t)
(which-key-mode)

(use-package general
     :ensure t
     :config
(general-evil-setup t))
;; (nvmap :prefix "SPC"

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package doom-themes
    :ensure t)
(setq doom-themes-enable-bold t
    doom-themes-enable-italics t)
(load-theme 'doom-outrun-electric t)

(use-package all-the-icons
    :ensure t)

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

(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-edit-src-content-indentation 0
    org-src-tab-acts-natively t
    org-src-preserve-indentation nil)

(use-package org-bullets
  :ensure t)
(add-hook 'org-mode-hook (lambda() (org-bullets-mode 1)))

(setq org-startup-folded t)

(use-package toc-org
    :ensure t
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package yasnippet
    :ensure t)

(use-package yasnippet-snippets

(use-package emojify
    :ensure t
    :hook (after-init . global-emojify-mode))

(setq gc-cons-threshold (* 2 1000 1000))
