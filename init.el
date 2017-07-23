;; ABOUT ME
(setq user-full-name "William Bolton")
(setq user-mail-address "william.ellet@gmail.com")
;; PACKAGE MANAGEMENT
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(require 'js-comint)
(setq inferior-js-program-command "/path/to/executable &lt;args&gt;")
(add-hook 'js2-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
            (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
            (local-set-key (kbd "C-c b") 'js-send-buffer)
            (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
            (local-set-key (kbd "C-c l") 'js-load-file-and-go)))

;; THEMES
 ;; (add-to-list 'load-path "~/.emacs.d/tomorrow-theme")
 ;; (require 'tomorrow-night-bright-theme)

(load-theme 'monokai t)
(set-face-attribute 'default nil
                    :height 150
                    :weight 'normal
                    :width 'normal)


(add-to-list 'load-path "~/.emacs.d/ws-trim")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(global-git-gutter+-mode)

;; WHITESPACE
(require 'ws-trim)
(global-ws-trim-mode t)
(set-default 'ws-trim-level 0)
(set-default 'ws-trim-method-hook '(ws-trim-tabs ws-trim-trailing))
(require 'whitespace)

(progn
  (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark )))

  ;; Make whitespace-mode and whitespace-newline-mode use “¶” for end of line char and “▷” for tab.
  (setq whitespace-display-mappings
        ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
        '(
          (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
          (newline-mark 10 [182 10]) ; LINE FEED,
          (tab-mark 9 [9655 9] [92 9]) ; tab
          )))

(custom-set-faces
 ;; ORG MODE STYLES
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:foreground "#7aa6da" :weight bold))))
 '(org-level-2 ((t (:foreground "#70c0b1" :weight bold))))
 '(org-level-3 ((t (:foreground "#b9ca4a" :weight bold))))
 '(org-level-4 ((t (:foreground "#e7c547" :weight bold))))
 '(org-level-5 ((t (:foreground "#e78c45" :weight bold))))
 '(org-level-6 ((t (:foreground "#d54e53" :weight bold))))
 '(org-level-7 ((t (:foreground "#c397d8" :weight bold))))
 '(org-level-8 ((t (:foreground "#4d5057" :weight bold))))
 '(org-link ((t (:foreground "#c397d8"))))
 '(org-todo ((t (:foreground "#d54e53" :weight bold))))
 '(whitespace-space ((t (:foreground "#75715E" :slant italic :weight light :width normal)))))


;; SAVE CURRENT DESKTOP
(require 'desktop)
(desktop-save-mode 1)
(defun my-desktop-save ()
  (interactive)
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)
(setq desktop-path '("."))

;; (add-hook 'after-init-hook #'global-flycheck-mode)
;; (eval-after-load 'flycheck '(progn
;;   (set-face-attribute 'flycheck-warning nil :underline nil :foreground "orange")
;;   (set-face-attribute 'flycheck-error nil :underline nil :foreground "red")))

(require 'autopair)
(autopair-global-mode)

(require 'auto-complete-config)
(ac-config-default)

;; RAILS DEVELOPMENT
;;(require 'flymake-ruby)
;;(add-hook 'ruby-mode-hook 'flymake-ruby-load)
;; RAILS CONSOLE SHORTCUT
;;(global-set-key (kbd "C-c i r b") 'inf-ruby)

;;(require 'rvm)
;;(rvm-use-default) ;; use rvm's default ruby for the current Emacs session
;;(global-set-key (kbd "C-c r m") 'rvm-activate-corresponding-ruby)
(setq ruby-deep-indent-paren nil)
(setq ruby-indent-level 2)
(setq ruby-insert-encoding-magic-comment nil)

(require 'yaml-mode)

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

(scroll-bar-mode -1)
(tool-bar-mode -1)


;; (menu-bar-mode -1)

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(setq echo-keystrokes 0.1
      use-dialog-box nil)
(setq ring-bell-function 'ignore)
(show-paren-mode t)
;; auto-complete config
(ac-config-default)
;; Line-wrap mode
(global-visual-line-mode t)


(setq
 indent-tabs-mode nil
 kill-whole-line t
 make-backup-files nil
 tab-width 2
 vc-handled-backends nil
 read-file-name-completion-ignore-case nil
 sentence-end-double-space nil
 )
(global-linum-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)

(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

(require 'direx)
(global-set-key (kbd "C-x C-g") 'direx-project:jump-to-project-root)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(direx:closed-icon " ▸")
 '(direx:leaf-icon "  ")
 '(direx:open-icon " ▾"))

(setq gofmt-command "goimports")
(add-hook 'before-save-hook #'gofmt-before-save)

(setq js-indent-level 2)

(setq org-startup-indented t)
(setq org-agenda-files '("~/Dropbox/work"))
(setq org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo 'done))
(setq org-agenda-window-setup 'current-window)
(global-set-key (kbd "C-c a #") 'org-agenda-list-stuck-projects)
(global-set-key (kbd "C-c a a") 'org-agenda-list)
(global-set-key (kbd "C-c a t") 'org-todo-list)
(setq org-todo-keywords '((sequence "WAIT" "TODO" "DONE")))
(setq auto-revert-interval 2)
(setq auto-revert-verbose nil)
(global-auto-revert-mode)

(add-to-list 'load-path "~/.emacs.d/org-bullets")
(require 'org-bullets)
(setq org-bullets-face-name (quote org-bullet-face))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-bullets-bullet-list '("￮"))


(define-key input-decode-map "\e[1;10A" [S-M-up])
(define-key input-decode-map "\e[1;10B" [S-M-down])
(define-key input-decode-map "\e[1;10C" [S-M-right])
(define-key input-decode-map "\e[1;10D" [S-M-left])
(put 'set-goal-column 'disabled nil)
