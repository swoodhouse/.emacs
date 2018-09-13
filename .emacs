(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(blink-cursor-mode 0)
(package-initialize)
(setq load-prefer-newer t)
(require 'auto-compile)
(auto-compile-on-load-mode 1)
(require 'package)
(require 'ace-jump-mode)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'fsharp-mode)
(require 'ess-site)
(require 'org)
(ac-config-default)

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(tex-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(add-to-list 'completion-styles 'initials t)
(add-to-list 'package-archives
   '("melpa" . "http://melpa.org/packages/") t)

(setq package-load-list '(all))
  (unless package-archive-contents
    (package-refresh-contents))

(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq default-frame-alist '((font . "Source Code Pro-11")))
(add-to-list 'ac-dictionary-directories "~/.emacs.d//lisp/ac-dict")
(load-theme 'sanityinc-solarized-dark t)

(setq org-support-shift-select t)
(setq ess-ask-for-ess-directory nil)
(setq tab-always-indent 'complete)
(global-auto-complete-mode t)
(setq show-paren-delay 0)
(show-paren-mode t)

(define-key global-map [(control ?v)]  'yank)  ; M-y cycles the kill ring, giving you multiple clipboards
(define-key global-map [(control ?s)]  'save-buffer)
(define-key global-map [(control ?f)]  'isearch-forward)
(define-key global-map (kbd "<XF86WakeUp>") 'ace-jump-char-mode)

(add-hook 'fsharp-mode-hook
 (lambda ()
   (define-key fsharp-mode-map (kbd "M-RET") 'fsharp-eval-region)
   (define-key fsharp-mode-map (kbd "C-SPC") 'fsharp-ac/complete-at-point)))

;; ;;; Enhanced undo - restore rectangle selections

(defvar CUA-rectangle nil
  "If non-nil, restrict current region to this rectangle.
Value is a vector [top bot left right corner ins pad select].
CORNER specifies currently active corner 0=t/l 1=t/r 2=b/l 3=b/r.
INS specifies whether to insert on left(nil) or right(t) side.
If PAD is non-nil, tabs are converted to spaces when necessary.
If SELECT is a regexp, only lines starting with that regexp are affected.")

(defvar CUA-undo-list nil
  "Per-buffer CUA mode undo list.")

(defvar CUA-undo-max 64
  "*Max no of undoable CUA rectangle changes (including undo).")

(defun CUA-rect-undo-boundary ()
  (when (listp buffer-undo-list)
    (if (> (length CUA-undo-list) CUA-undo-max)
	(setcdr (nthcdr (1- CUA-undo-max) CUA-undo-list) nil))
    (undo-boundary)
    (setq CUA-undo-list
	  (cons (cons (cdr buffer-undo-list) (copy-sequence CUA-rectangle)) CUA-undo-list))))

(defun CUA-undo (&optional arg)
  "Undo some previous changes.
Knows about CUA rectangle highlighting in addition to standard undo."
  (interactive "*P")
  (if CUA-rectangle
      (CUA-rect-undo-boundary))
  (undo arg)
  (let ((l CUA-undo-list))
    (while l
      (if (eq (car (car l)) pending-undo-list)
	  (setq CUA-next-rectangle 
		(and (vectorp (cdr (car l))) (cdr (car l)))
		l nil)
	(setq l (cdr l)))))
  (setq CUA-start-point nil))

(defvar CUA-tidy-undo-counter 0
  "Number of times `CUA-tidy-undo-lists' have run successfully.")

(defun CUA-tidy-undo-lists (&optional clean)
  (let ((buffers (buffer-list)) (cnt CUA-tidy-undo-counter))
    (while (and buffers (or clean (not (input-pending-p))))
      (with-current-buffer (car buffers)
	(when (local-variable-p 'CUA-undo-list)
	  (if (or clean (null CUA-undo-list) (eq buffer-undo-list t))
	      (progn
		(kill-local-variable 'CUA-undo-list)
		(setq CUA-tidy-undo-counter (1+ CUA-tidy-undo-counter)))
	    (let* ((bul buffer-undo-list)
		   (cul (cons nil CUA-undo-list))
		   (cc (car (car (cdr cul)))))
	      (while (and bul cc)
		(if (setq bul (memq cc bul))
		    (setq cul (cdr cul)
			  cc (and (cdr cul) (car (car (cdr cul)))))))
	      (when cc
		(if CUA-debug
		    (setq cc (length (cdr cul))))
		(if (eq (cdr cul) CUA-undo-list)
		    (setq CUA-undo-list nil)
		  (setcdr cul nil))
		(setq CUA-tidy-undo-counter (1+ CUA-tidy-undo-counter))
		(if CUA-debug
		    (message "Clean undo list in %s (%d)" 
			     (buffer-name) cc)))))))
      (setq buffers (cdr buffers)))
    (/= cnt CUA-tidy-undo-counter)))

(define-key global-map [(control ?z)] 'CUA-undo)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//lisp/ac-dict")

;; needed?
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("d3a406c5905923546d8a3ad0164a266deaf451856eca5f21b36594ffcb08413a" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-startup-truncated nil)
 '(package-selected-packages
   (quote
    (rust-mode clojure-mode cider monokai-theme ## solarized-theme haskell-mode magit popup-complete fsharp-mode ess color-theme-sanityinc-solarized auto-compile ace-jump-mode)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))

(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;M-q
(global-set-key [C-M-q] 'xah-fill-or-unfill)

(defun then_R_operator ()
  "R - %>% operator or 'then' pipe operator"
  (interactive)
  (just-one-space 1)
  (insert "%>%"))
(define-key ess-mode-map (kbd "C-.") 'then_R_operator)
(define-key inferior-ess-mode-map (kbd "C-.") 'then_R_operator)
(put 'downcase-region 'disabled nil)

(require 'ido)
(ido-mode t)

(setq rust-format-on-save t)

(autoload ‘markdown-mode “markdown-mode” “Major mode for editing Markdown files” t) (add-to-list ’auto-mode-alist’(“\.text\‘" . markdown-mode)) (add-to-list ’auto-mode-alist’(”\.markdown\‘" . markdown-mode)) (add-to-list ’auto-mode-alist’(“\.md\’” . markdown-mode))
