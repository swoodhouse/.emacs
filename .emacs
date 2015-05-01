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

(add-to-list 'completion-styles 'initials t)
(add-to-list 'package-archives
   '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-load-list '(all))
  (unless package-archive-contents
    (package-refresh-contents))

(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-face-attribute 'default nil :font "Source Code Pro-11")
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(load-theme 'sanityinc-solarized-dark t)

(setq org-support-shift-select t)
(setq ess-ask-for-ess-directory nil)
(setq tab-always-indent 'complete)
(global-auto-complete-mode t)

(define-key global-map [(control ?v)]  'yank)  ; M-y cycles the kill ring, giving you multiple clipboards
(define-key global-map [(control ?s)]  'save-buffer)
(define-key global-map [(control ?f)]  'isearch-forward)
(define-key global-map (kbd "C-a") 'ace-jump-mode)

(add-hook 'fsharp-mode-hook
 (lambda ()
   (define-key fsharp-mode-map (kbd "M-RET") 'fsharp-eval-region)
   (define-key fsharp-mode-map (kbd "C-SPC") 'fsharp-ac/complete-at-point)))


;; needed?
;(custom-set-variables
; custom-set-variables was added by Custom.
; If you edit it by hand, you could mess it up, so be careful.
; Your init file should contain only one such instance.
; If there is more than one, they won't work right.
; '(ac-auto-show-menu 0.1)
; '(ac-auto-start 1)
; '(ac-use-comphist nil)
; '(ess-use-R-completion t)
; '(ess-use-auto-complete t)
; '(inhibit-startup-screen t))

;(ac-flyspell-workaround)



;; ; make it so that M-RET will execute current line if there is no region selected
;; ; get undo/redo working nicely


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

; C-x b change buffers
; C-x k kill buffer
; M-x kill-some-buffers

;; C-c C-r: Evaluate region
;; C-c C-f: Load current buffer into toplevel
;; C-c C-e: Evaluate current toplevel phrase
;; C-M-x: Evaluate current toplevel phrase
;; C-M-h: Mark current toplevel phrase
;; C-c C-s: Show interactive buffer
;; C-c C-c: Compile with fsc
;; C-c x: Run the executable
;; C-c C-a: Open alternate file (.fsi or .fs)
;; C-c l: Shift region to left
;; C-c r: Shift region to right
;; C-c : Move cursor to the beginning of the block
;; C-c C-p: Load a project for autocompletion and tooltips
;; C-c C-d: Jump to definition of symbol at point
;; C-c C-t: Request a tooltip for symbol at point
;; C-c C-q: Quit current background compiler process
;; M-n: Go to next error
;; M-p: Go to previous error
;; To interrupt the interactive mode, use C-c C-c. This is useful if your code does an infinite loop or a very long computation.

;; If you want to shift the region by 2 spaces, use: M-2 C-c r

;; In the interactive buffer, use M-RET to send the code without explicitly adding the ;; thing.

;; Currently intellisense features can be offered for just one project at a time. To load a new F# project, use C-c C-p.

;; While a project is loaded, the following features will be available:

;; Type information for symbol at point will be displayed in the minibuffer
;; Errors and warnings will be automatically highlighted, with mouseover text.
;; To display a tooltip, move the cursor to a symbol and press C-c C-t (default).
;; To jump to the definition of a symbol at point, use C-c C-d.
;; Completion will be invoked automatically on dot, as in Visual Studio. It may be invoked manually using completion-at-point, often bound to M-TAB and C-M-i.
;; To stop the intellisense process for any reason, use C-c C-q.

;; needed?
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))))

;; ;; latex-mode
;; ;; tex-site
;; ;; git.elc
;; ;; csv mode, vlf mode
;; ;; latex presentations via org mode
