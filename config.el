(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package) (package-install 'use-package))

(require 'use-package-ensure)
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

(setq backup-directory-alist '(("." . "~/.config/emacs/.saves/")))

(setq auto-save-file-name-transforms
`((".*" "~/.config/emacs/.saves" t)))

(setq recentf-save-file (expand-file-name "~/.config/emacs/.saves/recentf"))

(setq ad-redefinition-action 'accept)

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

(setq scroll-conservatively 10000)
(setq scroll-margin 10)

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

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package key-chord)

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

(setq key-chord-two-keys-delay 0.3)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)

(use-package which-key)
(which-key-mode)

(use-package general
     :config
(general-evil-setup t))

(general-create-definer space-leader
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC"
)

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

(use-package ivy-rich
    :init
    (ivy-rich-mode 1))

(use-package counsel
:bind (("M-x" . counsel-M-x)
        ("C-x b" . counsel-ibuffer)
        ("C-x C-f" . counsel-find-file)
        :map minibuffer-local-map
        ("C-r" . 'counsel-minibuffer-history)))

(use-package company)
(add-hook 'after-init-hook 'global-company-mode)

(add-hook 'after-init-hook 'electric-indent-mode)

(add-hook 'after-init-hook 'electric-pair-mode)
(setq electric-pair-preserve t)
(show-paren-mode 1)
(setq show-paren-delay 0)

(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as bannerj
  (setq dashboard-startup-banner "~/.config/emacs/emacs-dash.png")  ;; use custom image as banner
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
			      (bookmarks . "book"))))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(use-package perspective
:bind
("C-x C-b" . persp-list-buffers)
:config
(persp-mode)
)

(add-hook 'dired-mode-hook 'auto-revert-mode)

(use-package pdf-tools
    :defer t
    :config
    (pdf-tools-install)
    (setq-default pdf-view-display-size 'fit-page)
)



(use-package projectile)

(use-package all-the-icons)

(use-package emacs-everywhere)

(use-package sudo-edit)

(space-leader
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

(space-leader
  "- a" '(lambda () (interactive)(find-file "~/orgfiles/agenda.org") :which-key "Org agenda")
  "- e" '(lambda () (interactive)(find-file "~/.config/emacs/config.org") :which-key "Emacs Configuration")
  "- p" '(lambda () (interactive)(find-file "~/Documents/Projects") :which-key "Project Folder")
)

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

(use-package haskell-mode)

(use-package emojify
    :hook (after-init . global-emojify-mode))

(setq split-height-threshold nil)
(setq split-width-threshold 0)

(winner-mode 1)
(space-leader 
       ;; Window splits
       "w c"   '(evil-window-delete :which-key "Close window")
       "w d"   '(evil-window-delete :which-key "Close window")
       "w o"   '(delete-other-windows :which-key "Delete other windows")
       "w n"   '(evil-window-new :which-key "New window")
       "w s"   '(evil-window-split :which-key "Horizontal split window")
       "w v"   '(evil-window-vsplit :which-key "Vertical split window")
       "w _"   '(evil-window-set-height :which-key "evil-window-set-height")
       "w |"   '(evil-window-set-width :which-key "evil-window-set-width")

       ;; Window motions
       "w h"   '(evil-window-left :which-key "Window left")
       "w j"   '(evil-window-down :which-key "Window down")
       "w k"   '(evil-window-up :which-key "Window up")
       "w l"   '(evil-window-right :which-key "Window right")
       "w w"   '(evil-window-next :which-key "Goto next window")
       ;; winner mode
       "w <left>"  '(winner-undo :which-key "Winner undo")
       "w <right>" '(winner-redo :which-key "Winner redo"))

(setq gc-cons-threshold (* 2 1000 1000))
