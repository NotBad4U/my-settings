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
(setq linum-format "%4d \u2502 ")

;; Spaces > Tabs
(setq c-basic-indent 2)
(setq tab-width 4)
(setq indent-tabs-mode nil)

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

;; THEMES ==============================================

;; NORD
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
(load-theme 'nord t)
(setq nord-comment-brightness 15)



;; EMACS SETTINGS =====================================
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(custom-set-variables
 '(package-selected-packages
   (quote
    (grizzl git-gutter doom-themes toml-mode toml all-the-icons projectile neotree racer company))))
(custom-set-faces)



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