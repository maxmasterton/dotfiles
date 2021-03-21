;   ___ _ __ ___   __ _  ___ ___
;  / _ \ '_ ` _ \ / _` |/ __/ __|
; |  __/ | | | | | (_| | (__\-- \
;  \___|_| |_| |_|\__,_|\___|___/
;

(setq user-full-name "Max Masterton"
      user-mail-address "me@maxmasterton.tech"
      ;user-login-name "MaxTheMasterton"
      )

(setq-default
 delete-by-moving-to-trash t ; Delete files to trash
 tab-width 4 ; Set tab width
 uniquify-buffer-name-style 'forward ; Uniquify buffer names
 window-combination-resize t ; Take new window space from all existing windows
 x-stretch-cursor t ; Stretch cursor to cover long glyphs
 )

(setq undo-limit 80000000 ; Set undo limit to 80mb
      evil-want-fine-undo t ; More granular changes in insert mode
      auto-save-default t ; Auto save by default
      inhibit-compacting-font-caches t ; Load glyphs into memory
      truncate-string-ellipsis "..." ; Use unicode eliipis instead of "..."
      )

(delete-selection-mode 1) ; Replace selection when inserting text
(global-subword-mode 1) ; Iterate through CamelCase Words with evil bindings

(setq doom-font (font-spec :family "JetBrains Mono" :size 13)
      doom-big-font (font-spec :family "JetBrains Mono" :size 30)
      ;doom-variable-pitch font
      ;doom-serif-font
      )

(defun my-buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in the current buffer"
  (interactive)
  (setq buffer-face-mode-face '(family: "JetBrains Mono" :height 100))
  (buffer-face-mode))
(add-hook 'org-mode-hook 'my-buffer-face-mode-variable)

(setq doom-theme 'doom-dracula)

;; Flagship themes
;(setq doom-theme 'doom-one
;      doom-theme 'doom-vibrant)

;(set-frame-parameter (selected-frame) 'alpha '(85 . 85))
;(add-to-list 'default-frame-alist '(alpha . (85 . 85)))

(setq doom-modeline-height 25
      doom-modeline-bar-width 3)
;
(setq doom-modeline-icon (display-graphic-p)
      doom-modeline-buffer-notification-icon t
;      doom-modeline-enable-word-count t ; Enables word count for selected area
      doom-modeline-indent-info t
      doom-modeline-workspace-name t
      )
;
(setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

(display-time-mode 1)
(global-display-line-numbers-mode)

(defun doom-modeline-conditional-buffer-encoding ()
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(setq org-directory "~/org/")

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(setq mu4e-get-mail-command "mbsync -c ~/.config/mu4e/mbsyncrc -a"
      mu4e-update-insterval 300
      mu4e-main-buffer-hide-personal-addresses t
      message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.maxmasterton.tech" 587 nil nil))
      mu4e-sent-folder "/account-1/Sent"
      mu4e-drafts-folder "/account-1/Drafts"
      mu4e-trash-folder "/account-1/Trash"
      mu4e-maildir-shortcuts
      '(("/account-1/Inbox"      . ?i)
        ("/account-1/Sent Items" . ?s)
        ("/account-1/Drafts"     . ?d)
        ("/account-1/Trash"      . ?t)))
