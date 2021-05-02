(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-refresh-contents)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq-default
 delete-by-moving-to-trash t
 tab-width 4
 uniquify-buffer-name-style 'forward
 window-combination-resize t
 x-stretch-cursor t
 indent-tabs-mode t)

(setq undo-limit 80000000
      frame-resize-pixelwise t
      evil-want-fine-undo t
      inhivit-compacting-font-caches t
      truncate-string-ellipsis "...")

(delete-selection-mode 1)
(global-subword-mode 1)
(global-visual-line-mode t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq custom-file (concat user-emacs-directory "/custom.el"))

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t
	evil-split-window-below t)
  (evil-mode))
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (setq evil-collection-mode-list '(dired ibuffer))
  (evil-collection-init))

(use-package general
  :ensure t
  :config
  (general-evil-setup t))

(use-package popwin :ensure t)
(popwin-mode 1)

(nvmap :keymaps 'override :prefix "SPC"
  "."   '(find-file :which-key "Find file")
  "SPC" '(counsel-M-x :which-key "M-x")
  "RET" '(bookmark-jump :which-key "Bookmarks")
  "c"   '(compile :which-key "Compile")
  "l"   '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :which-key "Reload config")
  "t"   '(toggle-truncate-lines :which-key "Toggle truncate lines")
  "p"   '(popwin:messages :which-key "Popup window")
  "a"   '(org-agenda :which-key "Open org-agenda"))

(nvmap :prefix "SPC"
  "b b" '(ibuffer :which-key "Ibuffer")
  "b k" '(kill-current-buffer :which-key "kill current buffer")
  "b n" '(next-buffer :which-key "Next buffer")
  "b p" '(previous-buffer :which-key "Previous buffer"))

(use-package all-the-icons :ensure t)
(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode))

(use-package gruvbox-theme :ensure t)
(load-theme 'gruvbox t)

(set-face-attribute 'default nil
					:font "JetBrains Mono 10"
					:weight 'medium)
(set-face-attribute 'variable-pitch nil
					:font "JetBrains Mono 10"
					:weight 'medium)
(set-face-attribute 'fixed-pitch nil
					:font "JetBrains Mono 10"
					:weight 'medium)

(set-face-attribute 'font-lock-comment-face nil
					:slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
					:weight 'medium)

(setq-default line-spacing 0.10)
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-10"))
(setq global-prettify-symbols-mode t)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-search-feed-face ":foreground #fff :weight bold"
		elfeed-feeds (quote
					  (("https://www.reddit.com/r/linux.rss" reddit linux)
					   ("https://www.reddit.com/r/commandline.rss" reddit commandline)
					   ("https://www.reddit.com/r/emacs.rss" reddit emacs)
					   ("https://opensource.com/feed/" linux opensouce)
					   ("https://linux.softpedia.com/backend.xml" softpedia)
					   ("https://www.computerworld.com/index.rss" computerworld)
					   ("https://betanews.com/feed" betanews linux)
					   ("https://distowatch.com/news/dwd.xml" distrowatch linux)))))

(use-package elfeed-goodies
  :ensure t
  :init
  (elfeed-goodies/setup)
  :config
  (setq elfeed-goodies/entry-pane-size 0.5))

(add-hook 'elfeed-show-mode-hook 'visual-line-mode)
(evil-define-key 'normal elfeed-show-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev)
(evil-define-key 'normal elfeed-search-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev)

(use-package all-the-icons-dired :ensure t)
(use-package dired-open :ensure t)
(use-package peep-dired :ensure t)

(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
  "d d" '(dired :which-key "Open dired")
  "d j" '(dired-jump :which-key "Dired jump to current")
  "d p" '(peep-dired :which-key "Peep dired"))

(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file)
  (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
  (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file))

(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(setq dired-open-extentions '(("gif" . "sxiv")
							  ("jpg" . "sxiv")
							  ("png" . "sxiv")
							  ("mkv" . "vlc")
							  ("mp4" . "vlc")))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode 1))

(use-package recentf
  :ensure t
  :config
  (recentf-mode))
(use-package sudo-edit :ensure t)

(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
  "f f" '(find-file :which-key "Find file")
  "f r" '(counsel-recentf :which-key "Recent files")
  "f s" '(save-buffer :find-file "Save file")
  "f S" '(sudo-edit :find-file "Sudo edit file"))

(use-package which-key
  :ensure t
  :init
  (setq which-key-sort-uppercase-first nil
		which-key-add-column-padding 1
		which-key-max-display-columns nil
		which-key-min-display-lines 4
		which-key-idle-delay 0.2
		which-key-seperator " → " ))
(which-key-mode)

(use-package company :ensure t)
(add-hook 'after-init-hook 'global-company-mode)

(use-package writeroom-mode :ensure t)

(use-package magit :ensure t)
(use-package git-gutter+ :ensure t)
(use-package git-gutter-fringe+ :ensure t)

(git-gutter-fr+-minimal)
(global-git-gutter+-mode)

(use-package telephone-line :ensure t)
(setq telephone-line-lhs
	  '((evil    . (telephone-line-evil-tag-segment))
		(accent  . (telephone-line-vc-segment
					telephone-line-erc-modified-channels-segment
					telephone-line-process-segment))
		(nil     . (telephone-line-buffer-segment))))
(setq telephone-line-rhs
	  '((nil     . (telephone-line-misc-info-segment))
		(accent  . (telephone-line-major-mode-segment))
		(evil    . (telephone-line-airline-position-segment))))

(setq telephone-line-primary-right-separator 'telephone-line-abs-left
	  telephone-line-secondary-right-separator 'telephone-line-abs-hollow-left)
(setq telephone-line-evil-use-short-tag t)

(set-face-attribute 'telephone-line-evil-normal
					nil
					:background
					"#cc241d")
(set-face-attribute 'telephone-line-evil-insert
					nil
					:background
					"#98971a")
(set-face-attribute 'telephone-line-evil-visual
					nil
					:background
					"#d79921")
(set-face-attribute 'telephone-line-evil-emacs
					nil
					:background
					"#b16286")
(set-face-attribute 'telephone-line-evil-motion
					nil
					:background
					"#458588")
(set-face-attribute 'telephone-line-evil-god
					nil
					:background
					"#458588")
(set-face-attribute 'telephone-line-evil-operator
					nil
					:background
					"#458588")

(display-time-mode 1)
(telephone-line-mode 1)

(use-package counsel
  :after ivy
  :ensure t
  :config (counsel-mode))
(use-package ivy
  :ensure t
  :defer 0.1
  :diminish
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (setq ivy-count-format "(%d/%d) "
		ivy-use-virtual-buffers t
		enable-recursive-minibuffers t)
  :config
  (ivy-mode))
(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
							   'ivy-rich-switch-buffer-transformer)
  (ivy-rich-mode 1))
(use-package swiper
  :after ivy
  :ensure t
  :bind (("C-s" . swiper)
		 ("C-r" . swiper)))

(setq ivy-initial-inputs-alist nil)

(use-package smex :ensure t)
(smex-initialize)

(use-package winum
  :ensure t)
(winum-mode)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
	(define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
	(setq treemacs-with 30
		  treemacs-indentation 3)
	(treemacs-follow-mode t)
	(treemacs-filewatch-mode t)
	(treemacs-fringe-indicator-mode 'always)
	(pcase (cons (not (null (executable-find "git")))
				 (not (null treemacs-python-executable)))
	  ('(t . t)
	   (treemacs-git-mode 'deferred))
	  ('(t . _)
	   (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
		("M-0"       . treemacs-select-window)
		("C-x t 1"   . treemacs-delete-other-windows)
		("C-x t t"   . treemacs)
		("C-x t B"   . treemacs-bookmark)
		("C-x t C-t" . treemacs-find-file)
		("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs-evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp
  :after (treemacs persp -mode)
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-all-the-icons :ensure t)
(treemacs-load-theme "all-the-icons")

(nvmap :prefix "SPC"
  "e h" '(counsel-esh-history :which-key "Eshell history")
  "e s" '(eshell :which-key "Eshell"))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :ensure t
  :config
  (eshell-syntax-highlighting-global-mode +1))

(setq eshell-history-size 5000
	  eshell-buffer-maximum-lines 5000
	  eshell-hist-ignoredups t
	  eshell-scroll-to-bottom-on-input t
	  eshell-destroy-buffer-when-process-dies t
	  eshell-visual-commands '("bash" "fish" "htop" "ssh" "top" "zsh"))

(use-package vterm :ensure t)
(setq shell-file-name "/bin/bash"
	  vterm-max-scrollback 5000)

(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-directory "~/Org/"
	  org-agenda-files '("~/Org/agenda.org")
	  org-ellipsis " ▼ "
	  org-hide-emphasis-markers t)
(setq org-src-preserve-indentation nil
	  org-src-tab-acts-natively t
	  org-src-fontify-natively t
	  org-confirm-babel-evaluate nil
	  org-edit-src-content-indentation 0)

(use-package org-bullets :ensure t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-link-abbrev-alist
	  '(("google" . "https://google.com/search?q=")
		("arch-wiki" . "https://wiki.archlinux.org/index.php/")
		("duckduckgo" . "https://duckduckgo.com/?q=")
		("wiki" . "https://en.wikipedia.org/wiki/")))

(setq org-todo-keywords
	  '((sequence
		 "TODO(t)"
		 "BLOG(b)"
		 "PROJ(p)"
		 "BOOK(b)"
		 "WAIT(w)"
		 "|"
		 "DONE(d)"
		 "CANCELLED(c)" )))

(use-package org-tempo
  :ensure nil)

(use-package smooth-scroll :ensure t)
(smooth-scroll-mode t)

(setq scroll-conservatively 101
	  mouse-wheel-scroll-amount '(3 ((shift) . 3))
	  mouse-wheel-progressive-speed t
	  mouse-wheel-follow-mouse 't)

(nvmap :prefix "SPC"
  ;; Window splits
  "w c" '(evil-window-delete :which-key "Close window")
  "w n" '(evil-window-new :which-key "New window")
  "w s" '(evil-window-split :which-key "Split window")
  "w v" '(evil-window-vsplit :which-key "Vsplit window")
  ;; Window Selections
  "w h" '(evil-window-left :which-key "Window left")
  "w j" '(evil-window-down :which-key "Window down")
  "w k" '(evil-window-up :which-key "Window up")
  "w l" '(evil-window-right :which-key "Window right")
  "w w" '(evil-window-next :which-key "Goto next window")
  ;; Window movement
  "w H" '(evil-window-move-far-left :which-key "Move window left")
  "w J" '(evil-window-move-very-bottom :which-key "Move window down")
  "w K" '(evil-window-move-very-top :which-key "Move window up")
  "w L" '(evil-window-move-far-right :which-key "Move window right"))

(use-package markdown-mode :ensure t)

(use-package rust-mode :ensure t)
(add-hook 'rust-mode-hook
		  (lambda () (setq indent-tabs-mode nil)))
(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)

(use-package go-mode :ensure t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(use-package haskell-mode :ensure t)

(use-package lua-mode :ensure t)

(use-package python-mode
  :ensure t
  :custom
  (python-shell-interpreter "python3"))

(use-package smartparens :ensure t)
(add-hook 'prog-mode-hook #'smartparens-mode)

(nvmap :prefix "SPC"
  "o e" '(eshell :which-key "Open Eshell")
  "o v" '(vterm :which-key "Open Vterm")
  "o t" '(treemacs :which-key "Open Treemacs")
  "o b" '(eww :which-key "Open Web Wowser")
  "o l" '(elfeed :which-key "Open elfeed")
  "o d" '(dired-jump :which-key "Open dired"))

(use-package linum-relative :ensure t)
(linum-relative-global-mode)
