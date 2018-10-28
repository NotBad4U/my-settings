(package-initialize)

;; PACKAGES ==============================================
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(put 'set-goal-column 'disabled nil)

;; EDITOR ==============================================

;; LAYOUT
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode t)
(setq-default truncate-lines -1)

(global-linum-mode 1)
(setq linum-format "%4d \u2503 ")

;; Spaces > Tabs
(setq c-basic-offset 4)
(setq-default tab-width 10)
(setq indent-tabs-mode nil)

(require 'hlinum)
(hlinum-activate)

(global-hl-todo-mode)

;; WINDOW
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(global-set-key (kbd "M-d") 'split-window-below)
(global-set-key (kbd "M-r") 'split-window-right)

;; PROJECTILE
(projectile-mode t)
(setq projectile-completion-system 'grizzl)

;; ICONS
(require 'all-the-icons) ;; Seem's to doesn't work yet

;; NEOTREE
(require 'neotree)
(global-set-key (kbd "C-b") 'neotree-toggle)
(setq neo-theme 'icons)

;; Auto Pairs
(setq skeleton-pair t)
  (global-set-key "(" 'skeleton-pair-insert-maybe)
  (global-set-key "[" 'skeleton-pair-insert-maybe)
  (global-set-key "{" 'skeleton-pair-insert-maybe)
  (global-set-key "<" 'skeleton-pair-insert-maybe)
  (global-set-key "\"" 'skeleton-pair-insert-maybe)
  (global-set-key "'" 'skeleton-pair-insert-maybe)

;; THEMES ==============================================

;; NORD
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
(load-theme 'nord t)
(setq nord-comment-brightness 15)
;;(load-theme 'rebecca t)

;; EMACS SETTINGS =====================================
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b06e5e671032cb2d215e502bd88a5e4c6aa9f5b992bd8738800052a13df9e45c" "72a097f48e588eaa08b17027ac20304dd3b3ea8ceaca4ca553fb2577b64f4d09" default)))
 '(package-selected-packages
   (quote
    (typescript-mode company-tern dockerfile-mode docker heroku-theme rebecca-theme dumb-jump airline-themes powerline markdown-mode auto-complete-c-headers go-mode minimap sublimity multiple-cursors hl-todo hlinum grizzl git-gutter doom-themes toml-mode toml all-the-icons projectile neotree racer company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



;; LANGS ==============================================

;; RUST
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'company-mode)

(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)



;; OTHERS ============================================

;; GIT
(global-git-gutter-mode +1)

;; TOML
(require 'toml-mode)

;; CUSTOM

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "S-<up>")  'move-line-up)
(global-set-key (kbd "S-<down>")  'move-line-down)
(global-set-key (kbd "C-z")  'undo)
(global-set-key (kbd "C-S-s") 'save-buffer)


(require 'powerline)
(powerline-default-theme)

(dumb-jump-mode)

(add-to-list 'load-path "/your/path/to/dockerfile-mode/")
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))


;; Utilities
(show-paren-mode 1)

(require 'company)
(require 'company-tern)

(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))

(add-to-list 'load-path "~/.emacs.d/ambienttalk/")
(load "ambienttalk")
(setq auto-mode-alist (cons '("??.at" . at-mode) auto-mode-alist))
