(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(setq use-package-always-ensure t)

;; Using garbage magic hack.
(use-package gcmh
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

(setq backup-directory-alist '(("." . "~/.config/emacs/saves/")))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package doom-themes)
(setq doom-themes-enable-bold t
    doom-themes-enable-italics t)
(load-theme 'doom-outrun-electric t)

(use-package doom-modeline)
(doom-modeline-mode 1)

(column-number-mode)
(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

(dolist (mode '(term-mode-hook
        eshell-mode-hook))
    (add-hook mode (lambda() (display-line-numbers-mode 0))))

(use-package rainbow-delimiters
:hook (prog-mode . rainbow-delimiters-mode))

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

(use-package evil
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
    (evil-mode))
(use-package evil-collection
    :after evil
    :config
    (evil-collection-init))

(use-package which-key)
(which-key-mode)

(use-package general
     :config
(general-evil-setup t))

(use-package ivy
    :diminish
    :bind (("C-s" . swiper)
            :map ivy-minibuffer-map
            ("TAB" . ivy-alt-done)
            ("C-l" . ivy-alt-done)
            ("C-j" . ivy-next-line)
            ("C-k" . ivy-previous-line)
            :map ivy-switch-buffer-map
            ("C-k" . ivy-previous-line)
            ("C-l" . ivy-done)
            ("C-d" . ivy-switch-buffer-kill)
            :map ivy-reverse-i-search-map
            ("C-k" . ivy-previous-line)
            ("C-d" . ivy-reverse-i-search-kill))
    :config
    (ivy-mode 1))

(use-package counsel)

(use-package projectile)

(use-package all-the-icons)

(use-package sudo-edit)

(use-package dashboard)

(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
    "."     '(find-file :which-key "Find file")
    "f f"   '(find-file :which-key "Find file")
    "f r"   '(counsel-recentf :which-key "Recent files")
    "f s"   '(save-buffer :which-key "Save file")
    "f u"   '(sudo-edit-find-file :which-key "Sudo find file")
    "f y"   '(dt/show-and-copy-buffer-path :which-key "Yank file path")
    "f C"   '(copy-file :which-key "Copy file")
    "f D"   '(delete-file :which-key "Delete file")
    "f R"   '(rename-file :which-key "Rename file")
    "f S"   '(write-file :which-key "Save file as...")
    "f U"   '(sudo-edit :which-key "Sudo edit file"))

(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-src-tab-acts-natively t
    org-src-preserve-indentation nil
    org-edit-src-content-indentation 0)

(use-package org-bullets)
(add-hook 'org-mode-hook (lambda() (org-bullets-mode 1)))

(setq org-startup-folded t)

(setq org-src-fontify-natively t
    org-confirm-babel-evaluate nil)

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package yasnippet)
(yas-global-mode 1)

(use-package yasnippet-snippets)

(use-package haskell-mode)

(use-package emojify
    :hook (after-init . global-emojify-mode))

(setq gc-cons-threshold (* 2 1000 1000))
