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

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package doom-themes)
(setq doom-themes-enable-bold t
    doom-themes-enable-italics t)
(load-theme 'doom-dracula t)

(use-package doom-modeline)
(doom-modeline-mode 1)
(setq find-file-visit-truename t)

(global-display-line-numbers-mode)
;; (setq display-line-numbers-type 'relative)

(dolist (mode '(term-mode-hook
        eshell-mode-hook))
    (add-hook mode (lambda() (display-line-numbers-mode 0))))

(set-default 'truncate-lines t)

;; Create a variable for our preferred tab width
(setq custom-tab-width 4)

;; Two callable functions for enabling/disabling tabs in Emacs
(defun disable-tabs () (setq indent-tabs-mode nil))
(defun enable-tabs  ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

;; Hooks to Enable Tabs
(add-hook 'prog-mode-hook 'disable-tabs)
;; Hooks to Disable Tabs
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'disable-tabs)

;; Making electric-indent behave sanely
(setq-default electric-indent-inhibit t)

;; Make the backspace properly erase the tab instead of
;; removing 1 space at a time.
(setq backward-delete-char-untabify-method 'hungry)

;; (OPTIONAL) Shift width for evil-mode users
;; For the vim-like motions of ">>" and "<<".
(setq-default evil-shift-width custom-tab-width)

;; WARNING: This will change your life
;; (OPTIONAL) Visualize tabs as a pipe character - "|"
;; This will also show trailing characters as they are useful to spot.
(setq whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces
 '(whitespace-tab ((t (:foreground "#636363")))))
(setq whitespace-display-mappings
  ;; '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
  '((tab-mark 9 [9] [92 9]))) ; 124 is the ascii ID for '\|'
;; (global-whitespace-mode) ; Enable whitespace mode everywhere
; END TABS CONFIG

;; Language-Specific Tweaks
(setq-default python-indent-offset custom-tab-width) ;; Python
(setq-default python-indent-guess-indent-offset nil) ;; Python
(setq-default js-indent-level custom-tab-width)      ;; Javascript
(setq-default c-basic-offset 4)                      ;; C/C++/Java
(setq LaTeX-item-indent 0)                           ;; LaTeX
(setq LaTeX-indent-level 4)                          ;; LaTeX
(add-hook 'html-mode-hook                            ;; html
    (lambda ()
    ;; Default indentation is usually 2 spaces, changing to 4.
        (set (make-local-variable 'sgml-basic-offset) 4)))
(add-hook 'sgml-mode-hook                            ;; html
    (lambda ()
        ;; Default indentation to 2, but let SGML mode guess, too.
        (set (make-local-variable 'sgml-basic-offset) 2)
        (sgml-guess-indent)))

(use-package aggressive-indent)
(global-aggressive-indent-mode)

(use-package undo-fu)
(use-package undo-fu-session)
(setq evil-undo-system 'undo-fu)

(global-hl-line-mode +1)
(use-package hlinum)
(hlinum-activate)

(use-package rainbow-delimiters
:hook (prog-mode . rainbow-delimiters-mode))

(setq scroll-step 1)
(setq scroll-conservatively 10000)

(set-face-attribute 'default nil
    :font "Noto Sans Mono 11"
    :weight 'medium)
(set-face-attribute 'variable-pitch nil
	:font "Noto Sans Mono 11"
    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
    :font "Noto Sans Mono 11"
    :weight 'medium)
;;(setq-default line-spacing 0.10)
(add-to-list 'default-frame-alist '(font . "Noto Sans Mono 11"))

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(setq org-pretty-entities t)

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
  (setq dashboard-startup-banner "~/.config/emacs/splash.png")  ;; use custom image as banner
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

(use-package all-the-icons-dired
  :init (setq all-the-icons-dired-monochrome nil)
)

(use-package dired-open)
(use-package peep-dired)

(space-leader
    "d d" '(dired :which-key "Open dired")
    "d j" '(dired-jump :which-key "Dired jump to current")
    "d p" '(peep-dired :which-key "Peep-dired"))

(defun fix-peep-dired-next-file()
(interactive)
(delete-other-windows)
(peep-dired-next-file))

(defun fix-peep-dired-prev-file()
(interactive)
(delete-other-windows)
(peep-dired-prev-file))

(with-eval-after-load 'dired
(general-define-key
:states '(normal, visual)
:keymaps 'dired-mode-map
"h" 'dired-up-directory
"l" 'dired-find-file)

(general-define-key
:states '(normal, visual)
:keymaps 'peep-dired-mode-map
"j" 'fix-peep-dired-next-file
"k" 'fix-peep-dired-prev-file))

(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("jpeg" . "sxiv")
                              ("png" . "sxiv")
                              ("svg" . "sxiv")
                              ("mkv" . "mpv")
                              ("pdf" . "zathura")
                              ("mp4" . "mpv")))

(eval-after-load  "dired-x" '(defun dired-clean-up-after-deletion (fn)
  "My Clean up after a deleted file or directory FN.
Remove expanded subdir of deleted dir, if any."
  (save-excursion (and (cdr dired-subdir-alist)
                       (dired-goto-subdir fn)
                       (dired-kill-subdir)))

  ;; Offer to kill buffer of deleted file FN.
  (if dired-clean-up-buffers-too
      (progn
        (let ((buf (get-file-buffer fn)))
          (and buf
               (save-excursion ; you never know where kill-buffer leaves you
                 (kill-buffer buf))))
        (let ((buf-list (dired-buffers-for-dir (expand-file-name fn)))
              (buf nil))
          (and buf-list
               (while buf-list
                 (save-excursion (kill-buffer (car buf-list)))
                 (setq buf-list (cdr buf-list)))))))
  ;; Anything else?
  ))

(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header 
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (mydired-sort))

(use-package flycheck)
(use-package flycheck-haskell)
(global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(mode-enabled save))

(use-package rainbow-mode)

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
  "- c" '(lambda () (interactive)(find-file "~/Documents/Class/2022/spring/") :which-key "current class folder")
)

(use-package evil-anzu)
(global-anzu-mode)

(use-package diff-hl)
(global-diff-hl-mode)

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

(space-leader
       "m *"   '(org-ctrl-c-star :which-key "Org-ctrl-c-star")
       "m +"   '(org-ctrl-c-minus :which-key "Org-ctrl-c-minus")
       "m ."   '(counsel-org-goto :which-key "Counsel org goto")
       "m e"   '(org-export-dispatch :which-key "Org export dispatch")
       "m f"   '(org-footnote-new :which-key "Org footnote new")
       "m h"   '(org-toggle-heading :which-key "Org toggle heading") "m i"   '(org-toggle-item :which-key "Org toggle item")
       "m n"   '(org-store-link :which-key "Org store link")
       "m o"   '(org-set-property :which-key "Org set property")
       "m t"   '(org-todo :which-key "Org todo")
       "m x"   '(org-toggle-checkbox :which-key "Org toggle checkbox")
       "m B"   '(org-babel-tangle :which-key "Org babel tangle")
       "m I"   '(org-toggle-inline-images :which-key "Org toggle inline imager")
       "m T"   '(org-todo-list :which-key "Org todo list")
       "o a"   '(org-agenda :which-key "Org agenda")
       )

(require 'org)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

(setq org-startup-with-inline-images t)

(use-package evil-org
  :ensure t
  :after org
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
(add-hook 'org-mode-hook 'evil-org-mode)



(use-package yasnippet)
(yas-global-mode 1)

(setq yas-indent-line nil)

(use-package haskell-mode)
(use-package typescript-mode)

(use-package ac-html)
(use-package ac-html-angular)
(use-package ac-html-csswatcher)
(use-package ac-html-bootstrap)

(use-package auctex
:defer t)

(use-package emojify)

(use-package atomic-chrome)
(atomic-chrome-start-server)

(use-package quickrun)
(space-leader
       "x x"   '(quickrun :which-key "quickrun")
)

(use-package eyebrowse)
(eyebrowse-mode t) 
(eyebrowse-setup-opinionated-keys)

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

(space-leader
       "r c"   '(copy-to-register :which-key "Copy to register")
       "r f"   '(frameset-to-register :which-key "Frameset to register")
       "r i"   '(insert-register :which-key "Insert register")
       "r j"   '(jump-to-register :which-key "Jump to register")
       "r l"   '(list-registers :which-key "List registers")
       "r n"   '(number-to-register :which-key "Number to register")
       "r r"   '(counsel-register :which-key "Choose a register")
       "r v"   '(view-register :which-key "View a register")
       "r w"   '(window-configuration-to-register :which-key "Window configuration to register")
       "r +"   '(increment-register :which-key "Increment register")
       "r SPC" '(point-to-register :which-key "Point to register"))

(space-leader
  "SPC"   '(counsel-M-x :which-key "M-x")
  "c c"   '(compile :which-key "Compile")
  "c C"   '(recompile :which-key "Recompile")
  "h r r" '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :which-key "Reload emacs config")
  "h t t" '(load-theme :which-key "Reload emacs config")
  "t t"   '(toggle-truncate-lines :which-key "Toggle truncate lines"))

(defun my/dired-copy-dirname-as-kill ()
  "Copy the current directory into the kill ring."
  (interactive)
  (kill-new default-directory))
(space-leader
"y y" 'my/dired-copy-dirname-as-kill)

(setq gc-cons-threshold (* 2 1000 1000))
