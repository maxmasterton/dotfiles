;; Max's configuration for the GNU/Emacs Text Editor? (init.el)

;; Setting up package.el to work with Melpa, Org and Gnu Repositories.
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-install 'use-package))

;; Installing Use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Enabling ':ensure t' globally
(require 'use-package)
(setq use-package-always-ensure t)

;; Rudimentary Settings
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
(display-time-mode 1)
(global-subword-mode 1)
(global-visual-line-mode t)

;; Line numbers on all buffers (expect otherwise specified)
(global-display-line-numbers-mode t)

;; Disabling line numbers on terminal and shell buffers
(dolist (mode '(org-mode-hook
				term-mode-hook
				treemacs-mode-hook
				vterm-mode-hook
				eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Disabling useless GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode -1)

;; Allocating ~/.emacs.d/custom.el file for custom set vairables
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; Installing Ivy and Ivy-rich.
;; Ivy is a completion mechanisim for emacs, ivy-rich gives descriptions alongside the commands
(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
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

;; Installing Swiper and Counsel, these build up on Ivy and provide unqiue functions
(use-package counsel
  :after ivy
  :config (counsel-mode))
(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
		 ("C-r" . swiper)))

;; Ivy posframe is an ivy-extention, which lets ivy use posframe to show it's candiate menu
(use-package ivy-posframe)
(setq ivy-posframe-display-functions-alist
	  '((swiper				. ivy-posframe-display-at-window-center)
		(complete-symbol	. ivy-posframe-display-at-point)
		(counsel-M-x		. ivy-posframe-display-at-window-center)
		(t					. ivy-posframe-display)))
(ivy-posframe-mode 1)

;; Improving Ivy
(setq ivy-initial-inputs-alist nil) ;; Removes annoying initial inputs, e.g. ("^")

;; Smex is a package that remembers M-x history
(use-package smex)
(smex-initialize)

;; Defining Fonts
(set-face-attribute 'default nil
					:font "JetBrains Mono 10"
					:weight 'medium)
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 110)
(set-face-attribute 'fixed-pitch nil
					:font "JetBrains Mono 10"
					:weight 'medium)

;; Some color schemes change characterisitcs of keywords and fonts. This overrides that config.
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :weight 'medium)

(setq-default line-spacing 0.10) ;; Configure line spacing
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-10")) ;; Only neccesary if using emacsclient
(setq global-prettify-symbols-mode t) ;; Allow glphys

;; Setting a nice theme
(use-package gruvbox-theme)
(load-theme 'gruvbox t)

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95. 95)))

;; installing some nice icons, dependency for later on
(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
	(all-the-icons-install-fonts t)))

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

;; Installing and selecting components of telephone-line
(use-package winum)
(winum-mode)

(use-package telephone-line)
(setq telephone-line-lhs
	  '((evil	. (telephone-line-evil-tag-segment))
		(accent	. (telephone-line-vc-segment
				   telephone-line-erc-modified-channels-segment
				   telephone-line-process-segment))
		(nil	. (telephone-line-buffer-segment))))
(setq telephone-line-rhs
	  '((nil	. (telephone-line-misc-info-segment))
		(accent	. (telephone-line-major-mode-segment))
		(evil   . (telephone-line-airline-position-segment))))

;; Changing the Telephone-line seperators to be always right-pointing
(setq telephone-line-primary-right-separator 'telephone-line-abs-left
	  telephone-line-secondary-right-separator 'telephone-line-abs-hollow-left)
(setq telephone-line-evil-use-short-tag t) ;; Use the short evil-tag (e.g "NO" instead of "NORMAL")

;; Telephone-line evil colors do not match gruvbox color scheme so I manually changed them.
(set-face-attribute 'telephone-line-evil-normal nil
					:background "#fb4934")
(set-face-attribute 'telephone-line-evil-insert nil
					:background "#98971a")
(set-face-attribute 'telephone-line-evil-visual nil
					:background "#d79921")
(set-face-attribute 'telephone-line-evil-emacs nil
					:background "#b16286")
(set-face-attribute 'telephone-line-evil-motion nil
					:background "#689d6a")
(set-face-attribute 'telephone-line-evil-god nil
					:background "#689d6a")
(set-face-attribute 'telephone-line-evil-operator nil
					:background "#689d6a")

;; Changing modeline to Telephone-line, must be after all configuration
(telephone-line-mode 1)

;; When programming, Use colored brakcets.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Installing Which-key
;; Which-key is a minor mode that displays key bindings follow the currently enetered incomplete commands in a popup window.
(use-package which-key
  :init (setq which-key-min-display-lines 4
			  which-key-idle-display 0.2
			  which-key-column-padding 1)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))
(which-key-mode)

;; Binding keys with general.el
(use-package general
  :config
  (general-evil-setup t))

;; Buffers
(nvmap :prefix "SPC"
  "b b" '(ibuffer :which-key "Ibuffer")
  "b k" '(kill-current-buffer :which-key "Kill current buffer")
  "b c" '(hydra-cycle-buffers/body :which-key "Cycle buffers"))

;; Elisp Evaluation and File Managment
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
  "e b" '(eval-buffer :which-key "Evaluate elisp in buffer")
  "e d" '(eval-expression :which-key "Evaluate elisp expression")
  "e r" '(eval-region :which-key "Evaluate reigon")

  "f f" '(find-file :which-key "Find file")
  "f r" '(counsel-recentf :which-key "Recent files")
  "f s" '(save-buffer :which-key "Save file")
  "f u" '(sudo-edit :which-key "Sudo edit file"))

;; Base Keybindings
(nvmap :keymaps 'override :prefix "SPC"
  "."   '(find-file :which-key "Find file")
  "SPC" '(counsel-M-x :which-key "M-x")
  "RET" '(bookmark-jump :which-key "Bookmarks")
  "l"   '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :which-key "Reload emacs")
  "t"   '(toggle-truncate-lines :which-key "Toggle truncate lines")
  "p"   '(counsel-projectile-rg :which-key "Ripgrep across Project") ;; Ripgrep Must be Installed
  "s"   '(hydra-text-scale/body :which-key "Scale text")
  "r"   '(writeroom-mode :which-key "Writeroom mode"))

;; Window Control 
(nvmap :prefix "SPC"
  ;; Window splits
  "w c" '(evil-window-delete :which-key "Close window")
  "w n" '(evil-window-new :which-key "New window")
  "w s" '(evil-window-split :which-key "Split window")
  "w v" '(evil-window-vsplit :which-key "Vsplit window")
  ;; Window selection
  "w h" '(evil-window-left :which-key "Window left")
  "w j" '(evil-window-down :which-key "Window down")
  "w k" '(evil-window-up :which-key "Window up")
  "w l" '(evil-window-right :which-key "Window right")
  "w w" '(evil-window-next :which-key "Goto next window")
  ;; Window movement
  "w H" '(evil-window-move-far-left :which-key "Window move left")
  "w J" '(evil-window-move-very-bottom :which-key "Window move down")
  "w K" '(evil-window-move-very-top :which-key "Window move up")
  "w L" '(evil-window-move-far-right :which-key "Window move right"))

;; Installing recentf to find recent files
(use-package recentf
  :config
  (recentf-mode))

;; Installing sudo edit for opening files with root privalibes
(use-package sudo-edit)

;; Evil Mode
;; Emulation of Vim within Emacs
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t
		evil-vsplit-window-below t)
  (evil-mode))

;; Collection brings evil support for packages like dired and ibuffer
(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dired ibuffer))
  (evil-collection-init))

;; Hydra is a package which allows for persistant keybindings for repative tasks
(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(defhydra hydra-cycle-buffers (:timeout 4)
  "cycle buffers"
  ("j" next-buffer "next")
  ("k" previous-buffer "last")
  ("q" nil "quit" :exit t))

;; Projectile plugin, Project managment within emacs
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/Code")
	(setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

;; Counsel support for projectile, I mainly use this for fd wich allows for serarching within a project.
(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

;; Magit, A Git Porceline inside Emacs
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;;; Org Mode
(defun org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . org-mode-setup)
  :config
  (setq org-ellipsis " ▼ "
		org-hide-emphasis-markers t))
(setq org-directory "~/org/"
	  org-agenda-files '("~/org/agenda.org")
	  org-deadline-warning-days 2
	  org-agenda-start-with-log-mode t
	  org-log-done 'note
	  org-log-into-drawer t)

;; Org Agenda Tags
(setq org-tag-alist
	  '((:startgroup)
		;; Mutually exclusive tags go here!
		(:endgroup)
		("School" . ?e)
		("Home"   . ?h)
		("Book"   . ?b)
		("Idea"   . ?i)
		("Notes"  . ?n)
		("Plan"   . ?p)))
		
;; Setting up org-src blocks to work as intended
(setq org-src-preserve-indentation t
	  org-src-tab-acts-natively t
	  org-src-fontify-natively t
	  org-confirm-babel-evaluate t
	  org-edit-src-content-indentation 0)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(setq org-confirm-babel-evaluate nil)

;; Proper org-mode indentation
(require 'org-indent)
(add-hook 'org-mode-hook 'org-indent-mode)

;; More usefull and diffrenciated bullets
(use-package org-bullets
  :after org)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Notated links to websites listed below:
(setq org-link-abbrev-alist
	  '(("duckduckgo" . "https://duckduckgo.com/?q=")
		("arch-wiki" . "https://wiki.archlinux.org/index.php/")
		("wikipedia" . "https://en.wikipedia.org/wiki/")))

;; Org tempo to increase efficency when creating source blocks
(use-package org-tempo
  :ensure nil)

;; Automatically create a Table of contents when :TOC: is used.
(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

;; Use Variable-pitch font for headlines in org-mode, diffrenciate them by size.
(dolist (face '((org-level-1 . 1.2)
				(org-level-2 . 1.1)
				(org-level-3 . 1.05)
				(org-level-4 . 1.0)
				(org-level-5 . 1.1)
				(org-level-6 . 1.1)
				(org-level-7 . 1.1)
				(org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

;; Sets attributes for what should and should not be variable pitch within emacs
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; Center documents in the center of the screen when editing an org mode document
(defun org-mode-visual-fill ()
  (setq visual-fill-column-width 75
		visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

;; This is simmilar to writeroom mode, which is simmilar to zen mode, I have this so I can use it when programming
(use-package writeroom-mode)

;; Smooth Scrolling
(setq scroll-conservatively 1000
	  scroll-margin 4
	  scroll-tep 1
	  mouse-wheel-scroll-amount '(6 ((shift) . 1))
	  mouse-wheel-progressive-speed nil)

(setq redisplay-dont-pause t)

(setq fast-but-imprecise-scrolling nil
	  jit-lock-defer-time 0)

(use-package smooth-scrolling)
(smooth-scrolling-mode 1)

;; Treemacs
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
	(define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
	(setq treemacs-collapse-dirs (if treemacs-python-executable 3 0)
		  treemacs-width 35
		  treemacs-position 'left)

	(treemacs-follow-mode t)
	(treemacs-filewatch-mode t)
	(treemacs-fringe-indicator-mode 'always)
	(pcase (cons (not (null (executable-find "git")))
				 (not (null treemacs-python-executable)))
	  ('(t . t)
	   (treemacs-git-mode 'deferred))
	  ('(t . t)
	   (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
		("M-0"			. treemacs-select-window)
		("C-x t 1" 		. treemacs-delete-other-windows)
		("C-x t B"		. treemacs-bookmark)
		("C-x t C-t"	. treemacs-find-file)
		("C-x t M-t"	. treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config (treemacs-icons-dired-mode))

(require 'treemacs-all-the-icons)
(treemacs-load-theme "all-the-icons")

;; Language-Server Protocol
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-intergration t)
  :hook (lsp-mode . lsp-mode-setup))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-ui-sideline-enable nil
		lsp-ui-sideline-show-hover nil)
  (lsp-ui-focus-frame))

(use-package lsp-treemacs
  :after lsp)

;; Header Breadcrumb
(defun lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headline-breadcrumb-mode))

;; Better code completions with the company package
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
			  ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
		("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))
;(add-hook 'prog-mode-hook 'company-mode)

(use-package company-box
  :hook (company-mode . company-box-mode))

;;; Programming language support and LSP-mode Intergration

;; Python Support
(use-package python-mode
  :custom
  (python-shell-interpreter "python3"))

;; Python keybindings
(nvmap :prefix "SPC"
  "p p" '(run-python :which-key "Run python")
  "p r" '(python-shell-send-reigon :which-key "Interpret Reigon")
  "p b" '(python-shell-send-buffer :which-key "Interpret Buffer"))
