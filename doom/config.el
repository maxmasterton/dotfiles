;   ___ _ __ ___   __ _  ___ ___
;  / _ \ '_ ` _ \ / _` |/ __/ __|
; |  __/ | | | | | (_| | (__\__ \
;  \___|_| |_| |_|\__,_|\___|___/
;

(setq user-full-name "Max Masterton"
      user-mail-address "me@maxmasterton.tech"
      ;user-login-name "MaxTheMasterton"
      )

(setq-default
 delete-by-moving-to-trash t ; Delete files to trash
 tab-width 4 ; set tab width
 uniquify-buffer-name-style 'forward ; Uniquify buffer names
 window-combination-resize t ; Take window sapce from all existing windows
 x-stretch-cursor t ; Stretch cursor over long glyphs
 )

(setq undo-limit 80000000 ; Set undo limit to 80mb
      evil-want-fine-undo t ; More granular changes in insert mode
      auto-save-default t ; Auto save by default
      inhibit-compacting-font-caches t ; Load glyphs into memory
      truncate-string-ellipsis "..." ; Use unicode ellispis instead of "..."
      )

(delete-selection-mode 1) ; Replace selection when inerting text
(global-subword-mode 1) ; Iterate through CamelCase Words with evil bindings

(define-key evil-normal-state-map (kbd "SPC o i") 'eww)
(define-key evil-normal-state-map (kbd "SPC o l") 'elfeed)

(setq browse-url-browser-function 'eww-browse-url)

(setq doom-font (font-spec :family "JetBrains Mono" :size 13)
      doom-big-font (font-spec :family "JetBrains Mono" :size 30)
      ;doom-variable-pitch-font
      ;doom-serif-font
      )

(defun my-buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in the current buffer"
  (interactive)
  (setq buffer-face-mode-face '(family: "JetBrains Mono" :height 100))
  (buffer-face-mode))
(add-hook 'org-mode-hook 'my-buffer-face-mode-variable)

(setq +doom-dashboard-menu-sections
    '(("Reload last session"
      :icon (all-the-icons-octicon "history" :face 'doom-dashboard-menu-title)
      :when (cond ((require 'persp-mode nil t)
                    (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
                  ((require 'desktop nil t)
                    (file-exists-p (desktop-full-file-name))))
      :face (:inherit (doom-dashboard-menu-title bold))
      :action doom/quickload-session)
      ("Open File Manager"
       :icon (all-the-icons-octicon "file-directory" :face 'doom-dashboard-menu-title)
       :action dired-jump)
      ("Read Emails"
       :icon (all-the-icons-octicon "mail" :face 'doom-dashboard-menu-title)
       :action =mu4e)
      ("Open org-agenda"
      :icon (all-the-icons-octicon "calendar" :face 'doom-dashboard-menu-title)
      :when (fboundp 'org-agenda)
      :action org-agenda)
      ("Read the News"
       :icon (all-the-icons-octicon "globe" :face 'doom-dashboard-menu-title)
       :action elfeed)
      ("Recently opened files"
      :icon (all-the-icons-octicon "file-text" :face 'doom-dashboard-menu-title)
      :action recentf-open-files)
      ("Jump to a Bookmark"
      :icon (all-the-icons-octicon "file-symlink-file" :face 'doom-dashboard-menu-title)
      :action bookmark-jump)
      ("Edit Config"
      :icon (all-the-icons-octicon "gear" :face 'doom-dashboard-menu-title)
      :action doom/open-private-config)
      ("Open Terminal"
      :icon (all-the-icons-octicon "terminal" :face 'doom-dashboard-menu-title)
      :action +vterm/here)
      ("Browse the Web"
       :icon (all-the-icons-octicon "rss" :face 'doom-dashboard-menu-title)
       :action eww)
      ("Quit Emacs"
       :icon (all-the-icons-octicon "circle-slash" :face 'doom-dashboard-menu-title)
       :action evil-quit)
      ))

(setq doom-theme 'doom-dracula)

;; Flagship themes
;(setq doom-theme 'doom-one
;      doom-theme 'doom-vibrant)

(delq! t custom-theme-load-path)

(setq fancy-splash-image "~/Images/Emacs-logo.svg")

(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

(set-frame-parameter (selected-frame) 'alpha '(95 . 90))
(add-to-list 'default-frame-alist '(alpha . (95 . 90)))

(setq doom-modeline-height 25
      doom-modeline-bar-width 3
      )

(setq doom-modeline-icon (display-graphic-p)
      doom-modeline-buffer-notification-icon t
      ;doom-modeline-enable-word-count t ; Enables word count for a selected area
      doom-modeline-indent-info t
      doom-modeline-workspace-name t
      )

(setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

(display-time-mode 1)
(global-display-line-numbers-mode)

(defun doom-modeline-conditional-buffer-encoding ()
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding
          )

(after! org
  (setq org-directory "~/org/"
        ;org-agenda-files "~/org/agenda.org"
        org-log-done 'note
        )
  (require 'org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
)

(require 'sublimity)
(require 'sublimity-scroll)
;(require 'sublimity-map) ; Doesn't interact well with minimap
(sublimity-mode 1)

(setq sublimity-scroll-weight1 0.8
      sublimity-scroll-weight2 0.8
      )

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed t
      )

(setq evil-vsplit-window-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))
(setq +ivy-buffer-preview t)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(require 'smtpmail)
(setq mu4e-get-mail-command "mbsyncrc -c"
      mu4e-update-interval 300
      message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("outlook.office365.com" 587 nil nil))
      smtpmail-auth-credentials '(("outlook.office365.com" 587 "m.masterton19@st-eds.co.uk" nil))
      smtpmail-default-smtp-server "outlook.office365.com"
      smtpmail-smtp-server "outlook.office365.com"
      smtpmail-smtp-service 587
      mu4e-sent-folder "/Sent"
      mu4e-drafts-folder "/Drafts"
      mu4e-trash-folder "/Trash"
      mu4e-refile-folder "/All Mail"
      mu4e-maildir-shortcuts
      '(("/max-school/Inbox"       . ?i)
        ("/max-school/Sent"        . ?s)
        ("/max-school/All Mail"    . ?a)
        ("/max-school/Trash"       . ?t))
      )

(require 'elfeed-goodies)
(elfeed-goodies/setup)
(setq elfeed-goodies/entry-pane-size 0.5)
(evil-define-key 'normal elfeed-show-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev)
(evil-define-key 'normal elfeed-search-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev)

(setq elfeed-feeds (quote
                    (("https://www.reddit.com/r/linux.rss" reddit linux)
                     ("https://www.reddit.com/r/emacs.rss" reddit emacs)
                     ("https://distrowatch.com/news/dwd.xml" distrowatcg linux)
                     ("https://betanews.com/feed" betanews linux)
                     ("https://www.zdnet.com/topic/linux/rss.xml" zdnet linux)
                     ("https://cms.qz.com/re/rss" quartz news))))
