(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(transient-mark-mode 1)

(setq inhibit-splash-screen t
      inhibit-startup-message t
      make-backup-files nil
      show-paren-mode t
      column-number-mode t
      vc-handled-backends nil
      tramp-default-method "sshx"
      tramp-password-prompt-regexp (concat "^.*"
					   (regexp-opt
					    '("passcode" "Passcode"
					      "password" "Password") t)
					   ".*:\0? *")

      large-file-warning-threshold nil
      split-width-threshold 160
      split-height-threshold 16000

      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(require 'package)
(package-initialize)

(require 'whitespace)
(setq-default whitespace-style '(face empty lines-tail trailing space-before-tab))
;(global-whitespace-mode 1)

(defun parent-directory (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun find-git-repo ()
  (let ((base (if buffer-file-name
		  (file-name-directory buffer-file-name)
		default-directory))
	(found nil))
    (while (and (not found) (not (equal "/" base))
      (if (file-exists-p (concat base ".git"))
	  (setq found base)
	(setq base (parent-directory base)))))
    found))

(defun grep-using-ag ()
  (interactive)
  (let ((str (symbol-at-point)))
    (progn
      (setq grep-use-null-device nil)
      (cd (find-git-repo))
      (grep (format "ag --nogroup \"%s\""
		    (read-string (format "ag [%s]: " str) nil nil str nil)))
      (set-buffer "*grep*"))))

(defun compile-bzImage ()
  (interactive)
  (progn
    (cd (find-git-repo))
    (setq compilation-auto-jump-to-first-error t)
    (compile "make bzImage -j16")))

(require 'magit)

(global-set-key (kbd "<f4>") 'grep-using-ag)
(global-set-key (kbd "<f5>") 'compile-bzImage)
(global-set-key (kbd "<f6>") 'magit-status)
(global-set-key (kbd "<f7>") 'magit-blame)

;; Ido
(require 'ido)
(setq ido-enable-flex-matching t
      ido-everywhere t)
(ido-mode 1)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;(setq tags-file-name (concat (find-git-repo) "TAGS"))

;; python mode for TARGETS files
(setq auto-mode-alist (cons '("\\/TARGETS$" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cconf" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cinc" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ctest" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mcconf" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.thrift-cvalidator" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.tw" . python-mode) auto-mode-alist))

;; Indentation
(setq-default c-default-style "linux")
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (flyspell-prog-mode)
	    (c-toggle-hungry-state 1)
	    (c-toggle-syntactic-indentation 1)
	    (local-set-key (kbd "RET") 'newline-and-indent)
	    ;; (let ((tags-file (concat (find-git-repo) "/TAGS")))
	    ;;   (if (file-exists-p tags-file)
	    ;; 	  (visit-tags-table tags-file))
	    ;;   )
	    ))

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(add-to-list 'auto-mode-alist '("/tmp/mutt-.*" . mail-mode))

(set-face-attribute 'default nil :height 110)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(magit-commit-arguments (quote ("--signoff")))
 '(magit-log-arguments (quote ("--decorate" "-n256")))
 '(package-selected-packages (quote (magit magit-filenotify solarized-theme)))
 '(safe-local-variable-values
   (quote
    ((nxml-child-indent . 2)
     (eval c-set-offset
	   (quote arglist-close)
	   0)
     (eval c-set-offset
	   (quote arglist-intro)
	   (quote ++))
     (eval c-set-offset
	   (quote case-label)
	   0)
     (eval c-set-offset
	   (quote statement-case-open)
	   0)
     (eval c-set-offset
	   (quote substatement-open)
	   0)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;(load-theme 'solarized-light)
(load-theme 'leuven)
