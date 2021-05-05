;; Max's configuration for the GNU/Emacs Text Editor? (init.el)
;; Full "~/.emacs.d/" can be found on github.com/maxmasterton/emacs/

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
				vterm-mode-hook
				eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

(setq ivy-initial-inputs-alist nil) ;; Removes annoying initial inputs, e.g. ("^")

;; Smex is a package that remembers M-x history
(use-package smex)
(smex-initialize)

;; Defining Fonts
(set-face-attribute 'default nil
					:font "JetBrains Mono 10"
					:weight 'medium)
(set-face-attribute 'variable-pitch nil
					:font "JetBrains Mono 10"
					:weight 'medium)
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

;; Installing and selecting components of telephone-line
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
					:background "#cc241d")
(set-face-attribute 'telephone-line-evil-insert nil
					:background "#98971a")
(set-face-attribute 'telephone-line-evil-visual nil
					:background "#d79921")
(set-face-attribute 'telephone-line-evil-emacs nil
					:background "#b16286")
(set-face-attribute 'telephone-line-evil-motion nil
					:background "#458588")
(set-face-attribute 'telephone-line-evil-god nil
					:background "#458588")
(set-face-attribute 'telephone-line-evil-operator nil
					:background "#458588")

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
