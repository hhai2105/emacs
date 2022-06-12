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

(setq use-dialog-box nil)

(setq use-short-answers t)
(setq confirm-nonexistent-file-or-buffer nil)

(setq ido-create-new-buffer 'always)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

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
    :font "JetBrains  Mono Medium 13")
(set-face-attribute 'variable-pitch nil
	:font "JetBrains Mono Medium 13")
(set-face-attribute 'fixed-pitch nil
    :font "JetBrains Mono Medium 13")
;;(setq-default line-spacing 0.10)
(add-to-list 'default-frame-alist '(font . "JetBrains Mono Medium 13"))
(add-to-list 'default-frame-alist '(line-spacing . 0.2))

;;(no-leader
;;"C-=" '(text-scale-increase :which-key "increase text size")
;;"C--" '(text-scale-decrease :which-key "decrease text size"))

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
(with-eval-after-load 'evil-maps
(define-key evil-motion-state-map (kbd "RET") nil))

(use-package evil-numbers)
(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

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

(general-create-definer no-leader
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix ""
)

(general-create-definer dap-leader
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "C-c d"
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
  (setq dashboard-startup-banner "~/.config/emacs/emacs.txt") ;; use standard emacs logo as bannerj
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

(use-package helm)

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
                              ("ttf" . "sxiv")
                              ("mkv" . "mpv")
                              ("pdf" . "zathura")
                              ("pptx" . "zathura")
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
;; (setq flycheck-check-syntax-automatically '(mode-enabled save))

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
  "- a" '(lambda () (interactive)(find-file "~/orgfiles/agenda.org") :which-key "Emacs Configuration")
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
    org-return-follows-link t
    org-src-preserve-indentation nil
    org-edit-src-content-indentation 0
    org-src-fontify-natively t
    org-confirm-babel-evaluate nil)

(use-package org-bullets)
(add-hook 'org-mode-hook (lambda() (org-bullets-mode 1)))

(setq org-startup-folded t)

(setq org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
        '((sequence
           "TODO(t)"           ; A task that is ready to be tackled
           "[ ](T)"           ; A checkbox
           "|"                 ; The pipe necessary to separate "active" states and "inactive" states
           "[X](D)"           ; A checkbox
           "DONE(d)"           ; Task has been completed
           "CANCELLED(c)" )))  ; Task has been cancelled

(defun org-toggle-todo ()
  (interactive)
  (save-excursion
    (org-back-to-heading t) ;; Make sure command works even if point is
                            ;; below target heading
    (cond ((looking-at "\*+ TODO")
           (org-todo "DONE"))
          ((looking-at "\*+ DONE")
           (org-todo "TODO"))
          ((looking-at "\*+ \\[ \\]")
           (org-todo "[X]"))
          ((looking-at "\*+ \\[X\\]")
           (org-todo "[ ]"))
          (t (message "org toggle")))))

;; (define-key org-mode-map (kbd "C-c C-d") 'org-toggle-todo)

(define-key org-read-date-minibuffer-local-map (kbd "C-h") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
(define-key org-read-date-minibuffer-local-map (kbd "C-l") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
(define-key org-read-date-minibuffer-local-map (kbd "C-k") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
(define-key org-read-date-minibuffer-local-map (kbd "C-j") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))
(define-key org-read-date-minibuffer-local-map (kbd "C-.") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-month 1))))
(define-key org-read-date-minibuffer-local-map (kbd "C-,") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-month 1))))

(use-package org-make-toc)
(add-hook 'org-mode-hook #'org-make-toc-mode)

(space-leader
       "m *"   '(org-ctrl-c-star :which-key "Org-ctrl-c-star")
       "m +"   '(org-ctrl-c-minus :which-key "Org-ctrl-c-minus")
       "m ."   '(counsel-org-goto :which-key "Counsel org goto")
       "m e"   '(org-export-dispatch :which-key "Org export dispatch")
       "m f"   '(org-footnote-new :which-key "Org footnote new")
       "m h"   '(org-toggle-heading :which-key "Org toggle heading") 
       "m i"   '(org-toggle-item :which-key "Org toggle item")
       "m n"   '(org-store-link :which-key "Org store link")
       "m o"   '(org-set-property :which-key "Org set property")
       "m t"   '(org-todo :which-key "Org todo")
       "m x"   '(org-toggle-todo :which-key "Org toggle checkbox")
       "m B"   '(org-babel-tangle :which-key "Org babel tangle")
       "m I"   '(org-toggle-inline-images :which-key "Org toggle inline imager")
       "m T"   '(org-todo-list :which-key "Org todo list")
       "o a"   '(org-agenda :which-key "Org agenda")
       "m s"   '(org-schedule :which-key "Org schedule")
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

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package org-roam
:init
(setq org-roam-v2-ack t)
:custom
(org-roam-directory "~/orgfiles/roam")
:config
(org-roam-setup)
(org-roam-db-autosync-mode))
(setq org-agenda-files '("~/orgfiles/roam/daily/"))

(setq org-roam-dailies-capture-templates
'(("d" "default" entry "* TODO %<%I:%M %p>: %? \nSCHEDULED: \<%<%Y-%m-%d  %a>\> "
:if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")))
)

(space-leader
"n f" '(org-roam-node-find :which-key "find node")
"n i" '(org-roam-node-insert :which-key "insert node")
"n l" '(org-roam-buffer-toggle :which-key "toggle buffer")
"n d n" '(org-roam-dailies-capture-today :which-keyh "capture today")
"n d T" '(org-roam-dailies-capture-tomorrow :which-keyh "capture tomorrow")
"n d Y" '(org-roam-dailies-capture-yesterday :which-keyh "capture yesterday")
"n d c" '(org-roam-dailies-goto-today :which-keyh "go to today")
"n d t" '(org-roam-dailies-goto-tomorrow :which-keyh "go to tomorrow")
"n d y" '(org-roam-dailies-goto-yesterday :which-keyh "go to yesterday")
)



(use-package yasnippet)
(yas-global-mode 1)

(setq yas-indent-line nil)

(use-package auctex
:defer t)

(use-package emojify)

(use-package atomic-chrome)
(atomic-chrome-start-server)

(use-package quickrun)
(space-leader
       "x x"   '(quickrun :which-key "quickrun")
)

;; (use-package eyebrowse)
;; (eyebrowse-mode t) 
;; (eyebrowse-setup-opinionated-keys)

(use-package persp-mode
  :init
  (add-hook 'after-init-hook #'persp-mode)
  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-reset-windows-on-nil-window-conf nil
        persp-nil-hidden t
        persp-auto-save-fname "autosave"
        persp-save-dir (concat user-emacs-directory "workspaces/")
        persp-set-last-persp-for-new-frames t
        persp-switch-to-added-buffer nil
        persp-kill-foreign-buffer-behaviour 'kill
        persp-remove-buffers-from-nil-persp-behaviour nil
        persp-auto-resume-time -1 ; Don't auto-load on startup
        persp-auto-save-opt (if noninteractive 0 1)) ; auto-save on kill
)

(load
(expand-file-name
  "packages/workspaces.el"
  user-emacs-directory))

(no-leader 
"M-0" '(+workspace/switch-to-9 :which-key "workspace 0")
"M-1" '(+workspace/switch-to-0 :which-key "workspace 1")
"M-2" '(+workspace/switch-to-1 :which-key "workspace 2")
"M-3" '(+workspace/switch-to-2 :which-key "workspace 3")
"M-4" '(+workspace/switch-to-3 :which-key "workspace 4")
"M-5" '(+workspace/switch-to-4 :which-key "workspace 5")
"M-6" '(+workspace/switch-to-5 :which-key "workspace 6")
"M-7" '(+workspace/switch-to-6 :which-key "workspace 7")
"M-8" '(+workspace/switch-to-7 :which-key "workspace 8")
"M-9" '(+workspace/switch-to-8 :which-key "workspace 9")
)
(space-leader 
"TAB 0" '(+workspace/switch-to-9 :which-key "workspace 0")
"TAB 1" '(+workspace/switch-to-0 :which-key "workspace 1")
"TAB 2" '(+workspace/switch-to-1 :which-key "workspace 2")
"TAB 3" '(+workspace/switch-to-2 :which-key "workspace 3")
"TAB 4" '(+workspace/switch-to-3 :which-key "workspace 4")
"TAB 5" '(+workspace/switch-to-4 :which-key "workspace 5")
"TAB 6" '(+workspace/switch-to-5 :which-key "workspace 6")
"TAB 7" '(+workspace/switch-to-6 :which-key "workspace 7")
"TAB 8" '(+workspace/switch-to-7 :which-key "workspace 8")
"TAB 9" '(+workspace/switch-to-8 :which-key "workspace 9")
"TAB n" '(+workspace/new :which-key "new workspace")
"TAB d" '(+workspace/delete :which-key "delete workspace")
"TAB r" '(+workspace/rename :which-key "rename workspace")
"TAB TAB" '(+workspace/display :which-key "display workspaces")

)

(use-package haskell-mode)
(use-package typescript-mode)

(use-package ac-html)
(use-package ac-html-angular)
(use-package ac-html-csswatcher)
(use-package ac-html-bootstrap)

(use-package lsp-mode
:init
(setq lsp-keymap-prefix "C-l")
:config
(lsp-enable-which-key-integration t)
:commands
(lsp lsp-deferred)
)
(setq lsp-enable-indentation nil)
(setq lsp-enable-on-type-formatting nil)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-java)
(add-hook 'java-mode-hook 'lsp-deferred)

(add-hook 'c++-mode-hook 'lsp-deferred)
(add-hook 'c-mode-hook 'lsp-deferred)

(use-package lsp-pyright)
(add-hook 'python-mode-hook 'lsp-deferred)

(add-hook 'javascript-mode-hook 'lsp-deferred)
(add-hook 'js-mode-hook 'lsp-deferred)
(add-hook 'typescript-mode-hook 'lsp-deferred)
(add-hook 'ts-mode-hook 'lsp-deferred)
(add-hook 'js-jsx-mode-hook 'lsp-deferred)

(use-package lsp-latex)  
(use-package lsp-ltex)  
(add-hook 'latex-mode-hook 'lsp-deferred)

(use-package dap-mode)  
(setq dap-auto-configure-features '(sessions locals controls tooltip))
(no-leader
"<f5>" '(dap-debug :which-key "debug mode"))
(dap-leader
"d" '(dap-debug :which-key "debug-mode")
"b a" '(dap-breakpoint-add :which-key "add breakpoint")
"b d" '(dap-breakpoint-delete :which-key "delete breakpoint")
"h" '(dap-hydra :which-key "dap hydra")
)

(add-hook 'dap-stopped-hook
    (lambda (arg) (call-interactively #'dap-hydra)))

;; java
(require 'dap-java)
;; python 
(require 'dap-python)
;; c/c++
(require 'dap-gdb-lldb)
(require 'dap-lldb)
(require 'dap-cpptools)
;; remeber to run dap-gdb-lldb-setup
;; remeber to run dap-cpptools-setup

(setq tramp-default-method "ssh")

(setq shell-prompt-pattern '"^[^#$%>\n]*~?[#$%>] *")

(use-package docker)
(load
(expand-file-name
"packages/dockerfile-mode.el"
user-emacs-directory))
(use-package docker-compose-mode)

(add-hook 'dockerfile-mode-hook
      (lambda ()
          (setq-local indent-line-function #'sh-indent-line)))

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
