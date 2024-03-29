(org-babel-load-file
 (expand-file-name
  "README.org"
  user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application eshell)
	  eshell-connection-default-profile)
	 ((:application tramp)
	  tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile
	  (eshell-path-env-list))
	 (tramp-connection-local-darwin-ps-profile
	  (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
	  (tramp-process-attributes-ps-format
	   (pid . number)
	   (euid . number)
	   (user . string)
	   (egid . number)
	   (comm . 52)
	   (state . 5)
	   (ppid . number)
	   (pgrp . number)
	   (sess . number)
	   (ttname . string)
	   (tpgid . number)
	   (minflt . number)
	   (majflt . number)
	   (time . tramp-ps-time)
	   (pri . number)
	   (nice . number)
	   (vsize . number)
	   (rss . number)
	   (etime . tramp-ps-time)
	   (pcpu . number)
	   (pmem . number)
	   (args)))
	 (tramp-connection-local-busybox-ps-profile
	  (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
	  (tramp-process-attributes-ps-format
	   (pid . number)
	   (user . string)
	   (group . string)
	   (comm . 52)
	   (state . 5)
	   (ppid . number)
	   (pgrp . number)
	   (ttname . string)
	   (time . tramp-ps-time)
	   (nice . number)
	   (etime . tramp-ps-time)
	   (args)))
	 (tramp-connection-local-bsd-ps-profile
	  (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
	  (tramp-process-attributes-ps-format
	   (pid . number)
	   (euid . number)
	   (user . string)
	   (egid . number)
	   (group . string)
	   (comm . 52)
	   (state . string)
	   (ppid . number)
	   (pgrp . number)
	   (sess . number)
	   (ttname . string)
	   (tpgid . number)
	   (minflt . number)
	   (majflt . number)
	   (time . tramp-ps-time)
	   (pri . number)
	   (nice . number)
	   (vsize . number)
	   (rss . number)
	   (etime . number)
	   (pcpu . number)
	   (pmem . number)
	   (args)))
	 (tramp-connection-local-default-shell-profile
	  (shell-file-name . "/bin/sh")
	  (shell-command-switch . "-c"))
	 (tramp-connection-local-default-system-profile
	  (path-separator . ":")
	  (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "b54bf2fa7c33a63a009f249958312c73ec5b368b1094e18e5953adb95ad2ec3a" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" "8b6506330d63e7bc5fb940e7c177a010842ecdda6e1d1941ac5a81b13191020e" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "1aa4243143f6c9f2a51ff173221f4fd23a1719f4194df6cef8878e75d349613d" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "da186cce19b5aed3f6a2316845583dbee76aea9255ea0da857d1c058ff003546" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "6ebdb33507c7db94b28d7787f802f38ac8d2b8cd08506797b3af6cdfd80632e0" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "6f4421bf31387397f6710b6f6381c448d1a71944d9e9da4e0057b3fe5d6f2fad" "b5803dfb0e4b6b71f309606587dd88651efe0972a5be16ece6a958b197caeed8" "266ecb1511fa3513ed7992e6cd461756a895dcc5fef2d378f165fed1c894a78c" "23c806e34594a583ea5bbf5adf9a964afe4f28b4467d28777bcba0d35aa0872e" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "cbdf8c2e1b2b5c15b34ddb5063f1b21514c7169ff20e081d39cf57ffee89bc1e" "028c226411a386abc7f7a0fba1a2ebfae5fe69e2a816f54898df41a6a3412bb5" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "5784d048e5a985627520beb8a101561b502a191b52fa401139f4dd20acb07607" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "1f1b545575c81b967879a5dddc878783e6ebcca764e4916a270f9474215289e5" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "c2aeb1bd4aa80f1e4f95746bda040aafb78b1808de07d340007ba898efa484f5" "d47f868fd34613bd1fc11721fe055f26fd163426a299d45ce69bef1f109e1e71" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "da53441eb1a2a6c50217ee685a850c259e9974a8fa60e899d393040b4b8cc922" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" default))
 '(electric-indent-mode nil)
 '(global-aggressive-indent-mode t)
 '(global-hl-line-mode t)
 '(helm-minibuffer-history-key "M-p")
 '(ignored-local-variable-values '((eval add-hook 'before-save-hook 'time-stamp)))
 '(package-selected-packages
   '(magit writeroom-mode git-ps1-mode no-littering aggressive-indent dumb-jump go-complete go-mode ein sql-indent highlight-indent-guides xah-fly-keys helm org-make-toc lsp-tailwindcss docker-compose-mode dockerfile-compose-mode dockerfile-mode pretty-mode tramp-theme eyebrowse persp-mode lsp-java lsp-ui lsp-ivy lsp-mode emojify pdf-tools peep-dired dired-open counsel ivy general which-key evil-collection doom-themes gcmh use-package spacemacs-theme org-roam evil-org svg-clock undo-fu-session undo-fu quickrun cuda-mode highlight-indentation markdown-mode flymake-eslint eslint-fix atomic-chrome ssh-agency ssh-deploy ssh-tunnels ssh leetcode ac-html-bootstrap ac-html-csswatcher ac-html-angular ac-html typescript-mode diff-hl evil-anzu phps-mode hlinum auctex-latexmk flycheck frontside-javascript flycheck-haskell openwith visual-fill-column org-noter-pdftools org-noter rainbow-mode perspective company emacs-everywhere smooth-scroll key-chord ivy-rich xwwp-follow-link-ivy dashboard sudo-edit rainbow-delimiters projectile auctex yasnippet org-bullets all-the-icons-dired ## doom-modeline doom-modelines evil))
 '(pixel-scroll-mode nil)
 '(safe-local-variable-values '((git-commit-major-mode . git-commit-elisp-text-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-buffer-modified ((t :inherit modified-buffer)))
 '(whitespace-tab ((t (:foreground "#636363")))))
(put 'dired-find-alternate-file 'disabled nil)
