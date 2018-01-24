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
 '(org-startup-truncated nil)
 '(package-selected-packages
   (quote
    (magit popup-complete fsharp-mode ess color-theme-sanityinc-solarized auto-compile ace-jump-mode)))
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))))

(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(defun xah-fill-or-unfill ()
  "Reformat current paragraph or region to `fill-column', like `fill-paragraph' or unfill.
When there is a text selection, act on the selection, else, act on a text block separated by blank lines.
URL `http://ergoemacs.org/emacs/modernization_fill-paragraph.html'
Version 2017-01-08"
  (interactive)
  ;; This command symbol has a property 'compact-p, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let ( ($compact-p
          (if (eq last-command this-command)
              (get this-command 'compact-p)
            (> (- (line-end-position) (line-beginning-position)) fill-column)))
         (deactivate-mark nil)
         ($blanks-regex "\n[ \t]*\n")
         $p1 $p2
         )
    (if (use-region-p)
        (progn (setq $p1 (region-beginning))
               (setq $p2 (region-end)))
      (save-excursion
        (if (re-search-backward $blanks-regex nil "NOERROR")
            (progn (re-search-forward $blanks-regex)
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (if (re-search-forward $blanks-regex nil "NOERROR")
            (progn (re-search-backward $blanks-regex)
                   (setq $p2 (point)))
          (setq $p2 (point)))))
    (if $compact-p
        (fill-region $p1 $p2)
      (let ((fill-column most-positive-fixnum ))
        (fill-region $p1 $p2)))
    (put this-command 'compact-p (not $compact-p))))

;M-q
(global-set-key [C-M-q] 'xah-fill-or-unfill)
(put 'downcase-region 'disabled nil)

(defun then_R_operator ()
  "R - %>% operator or 'then' pipe operator"
  (interactive)
  (just-one-space 1)
  (insert "%>%"))
(define-key ess-mode-map (kbd "C-.") 'then_R_operator)
(define-key inferior-ess-mode-map (kbd "C-.") 'then_R_operator)

(require 'ido)
(ido-mode t)
