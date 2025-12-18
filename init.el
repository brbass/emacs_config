;;----------------------;;
;; General key bindings ;;
;;----------------------;;
;; No startup screen
(setq inhibit-startup-screen t)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 2)

;; Remove anything that could encourage mouse use and weakness
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1)) ;; Removes the top menu bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1)) ;; Removes the button toolbar
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1)) ;; Removes the right scroll bar

;; Clean file complete
(setq minibuffer-auto-raise t)

;; Swap the actions of RET and C-j
(global-set-key (kbd "C-j") #'newline-and-indent)
(global-set-key (kbd "RET") #'newline)

;; Make it so C++ doesn't indent in namespace
(defun my/c++-namespace-indent-fix ()
  "Don't indent code inside C++ namespace blocks."
  (c-set-offset 'innamespace 0))
(add-hook 'c++-mode-hook 'my/c++-namespace-indent-fix)

;; Show the column number and containing function
(column-number-mode 1)
(which-function-mode 1)

;; Reload file quickly
(defun my/revert-buffer ()
  "Revert buffer from disk. Ask for confirmation only if buffer has unsaved changes."
  (interactive)
  (if (buffer-modified-p)
      (when (yes-or-no-p "Buffer has unsaved changes. Revert anyway? ")
        (revert-buffer :ignore-auto :noconfirm))
    (revert-buffer :ignore-auto :noconfirm)))
(global-set-key (kbd "C-c r") 'my/revert-buffer)

;; Make sure we only have one thread in eshell
(add-hook 'eshell-mode-hook
          (lambda ()
            (setenv "OMP_NUM_THREADS" "1")))

;; Ediff otherwise splits vertically
(setq ediff-split-window-function 'split-window-horizontally)

;; Use minibuffers in minibuffers!
;; (setf enable-recursive-minibuffers t)

;;-----------------------------;;
;; Run commands in minibuffers ;;
;;-----------------------------;;
(defun my/get-key (key)
  "Return the nearest preceding value of KEY in a Markdown buffer.
KEY should be a string like \"dir:\" or \"path:\".
Only searches Markdown buffers and returns only a valid directory if applicable."
  (when (derived-mode-p 'markdown-mode 'gfm-mode)
    (save-excursion
      ;; Search for the key
      (when (re-search-backward (format "^%s:[[:space:]]*\\(.*\\)$" (regexp-quote key)) nil t)
        (let ((val (string-trim (match-string 1))))
          ;; For keys representing directories, validate they exist
          (if (member key '("dir" "path"))
              (when (file-directory-p val)
                (file-name-as-directory val))
            val))))))
(defmacro my/with-data-keys (&rest body)
  "Execute BODY in dir: and path: from Markdown buffer if present."
  ;; Store the original variables and get the new ones
  `(let ((orig-dir default-directory)
         (dir (my/get-key "dir"))
         (new-path (my/get-key "path"))
         (old-path (getenv "PATH")))
     (unwind-protect
         (progn
           ;; If dir and path exist, then set them
           (when dir (setq default-directory dir))
           (when new-path (setenv "PATH" (concat new-path ":" old-path)))
           ;; Run the command
           ,@body)
       ;; If dir and path exist, then restore the original values
       (when new-path (setenv "PATH" old-path))
       (when dir (setq default-directory orig-dir)))))
(defun my/async-shell-insert-command-header (buf command)
  "Insert COMMAND as a header at the top of BUF."
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (insert (format "$ %s\n\n" command)))))
(defun my/async-shell-buffer-name (command)
  "Return a buffer name based on COMMAND, stripping leading spaces."
  (let ((trimmed (string-trim-left command)))
    trimmed))
(defun my/async-send-current-line ()
  "Send the current line to an async shell command, running in dir: if present."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (my/with-data-keys
     (let ((buf (my/async-shell-buffer-name line)))
       (async-shell-command line buf)))))
(defun my/async-send-current-region (start end)
  "Send the current region to an async shell command, running in dir: if present."
  (interactive "r")
  (let ((region-text (buffer-substring-no-properties start end)))
    (my/with-data-keys
     (let ((buf (my/async-shell-buffer-name region-text)))
       (async-shell-command region-text buf)))))
(defun my/async-shell-command (command)
  "Run COMMAND asynchronously, using a buffer named after the command, in dir: if present."
  (interactive
   (list (read-shell-command "Async shell command: "
                             nil 'shell-command-history)))
  (my/with-data-keys
   (let ((buf (my/async-shell-buffer-name command)))
     (async-shell-command command buf))))

;; Bind them!
(global-set-key (kbd "M-*") 'my/async-send-current-region)
(global-set-key (kbd "M-|") 'my/async-send-current-line)
(global-set-key (kbd "M-&") 'my/async-shell-command)

;; This imports bash variables for use in the shell
(setq shell-file-name "bash")
(setq shell-command-switch "-ic")

;;--------------------;;
;; Performance tuning ;;
;;--------------------;;
(setq gc-cons-threshold (* 500 1024 1024))      ;; GC threshold 500 MB
(setq gc-cons-percentage 0.2)                   ;; GC percentage
(setq large-file-warning-threshold (* 500 1024 1024)) ;; Warn for files >500 MB

(setq bidi-display-reordering 'left-to-right     ;; Disable expensive bidi
      bidi-paragraph-direction 'left-to-right)
(setq redisplay-skip-fontification-on-input t    ;; Skip fontification while typing
      vc-handled-backends '(Git)                 ;; Only handle Git
      file-notify-watch-descriptor-max 10000)   ;; More file notifications
(setq font-lock-maximum-size 2000000)          ;; No font-lock for very large buffers

;; Defer GC during minibuffer input
(add-hook 'minibuffer-setup-hook
          (lambda () (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook
          (lambda () (setq gc-cons-threshold (* 500 1024 1024))))

;; Native compilation settings (if available)
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent))

;; Subprocess settings
(setq read-process-output-max (* 64 1024 1024))  ;; Read subprocess max 64 MB
(setq process-adaptive-read-buffering nil)

;;-----------------;;
;; Set up packages ;;
;;-----------------;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(dolist (pkg '(ace-window
               ztree
               magit
               ;; Terminal
               vterm
               direnv
               ;; Themes
               ;; gruvbox-theme
               nordic-night-theme
               ;; Minibuffer changes
               consult
               consult-ls-git
               embark
               embark-consult
               marginalia
               orderless
               vertico
               ;; LSP
               flycheck
               lsp-mode
               lsp-pyright
               lsp-treemacs
               lsp-ui))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Ace window for switching windows easily
(require 'ace-window)
(setq aw-scope 'frame)
(global-set-key (kbd "M-o") 'ace-window)
(with-eval-after-load 'term
  (define-key term-raw-map (kbd "M-o") 'ace-window))

;; Recursive tree comparison: M-x ztree-diff
(require 'ztree)

;; For using git in emacs
(require 'magit)

;; Performant terminal emulator
(require 'vterm)
(define-key vterm-mode-map (kbd "C-c C-j") 'vterm-copy-mode)
(define-key vterm-copy-mode-map (kbd "C-c C-k") #'vterm-copy-mode)

;; For automatically loading direnv
(require 'direnv)
(direnv-mode)

;; Better color theme
;; (require 'gruvbox-theme)
;; (load-theme 'gruvbox-dark-hard t)
(require 'nordic-night-theme)
(load-theme 'nordic-night t)

;; Vertico - incremental completion
(require 'vertico)
(vertico-mode 1)
(setq vertico-count 30
      vertico-resize nil
      vertico-cycle nil)
;; (vertico-buffer-mode 1)
;; (setq vertico-buffer-display-action
;;       '(display-buffer-in-side-window
;;         (side . left)
;;         (window-width . 0.3)))
(keymap-set vertico-map "TAB" #'minibuffer-complete)

;; Marginalia - annotations in completion
(require 'marginalia)
(marginalia-mode)

;; Orderless - flexible matching
(require 'orderless)
(setq completion-category-defaults nil
      completion-category-overrides '((file (styles orderless partial-completion)))
      ;; completion-styles '(orderless basic)
      completion-styles '(basic substring partial-completion flex)
      completion-pcm-leading-wildcard t)

;; Acting on selected files
(require 'embark)
(require 'embark-consult)
(global-set-key (kbd "C-c .") 'embark-act)
(global-set-key (kbd "C-c ;") 'embark-export)

;; Consult for better search commands
(require 'consult)
(require 'consult-ls-git)
(global-set-key (kbd "C-c g") 'consult-ripgrep)         ; Search in project with ripgrep
(global-set-key (kbd "C-c s") 'consult-git-grep)        ; Search in project with git grep
(global-set-key (kbd "C-c l") 'consult-line)            ; Search lines in current buffer
(global-set-key (kbd "C-c f") 'consult-ls-git-ls-files) ; Find files (git-aware)

;; Language server support (uncomment the hooks for auto-load)
(require 'lsp-mode)
(setq lsp-prefer-flymake nil  ; Use flycheck instead of flymake
      lsp-session-file nil    ; Do not save LSP session to disk
      lsp-log-io nil)         ; No logging, as this hurts performance   
;; (add-hook 'python-mode-hook  'lsp-deferred)  ;; python-lsp-server or pyright
;; (add-hook 'c++-mode-hook     'lsp-deferred)  ;; clangd
;; (add-hook 'c-mode-hook       'lsp-deferred)  ;; clangd
;; (add-hook 'fortran-mode-hook 'lsp-deferred)  ;; fortls
;; (add-hook 'f90-mode-hook     'lsp-deferred)  ;; fortls (modern Fortran)
;; (add-hook 'sh-mode-hook      'lsp-deferred)  ;; bash-language-server
(global-set-key (kbd "M-'") 'lsp-find-references)
(setq lsp-enable-on-type-formatting nil)  ;; Disable format on typing
(setq lsp-enable-indentation nil)         ;; Disable LSP auto-indentation

(require 'lsp-pyright)   ;; LSP wasn't finding pyright on its own
(require 'lsp-treemacs)  ;; tree-based UI (symbols, errors, hierarchy)
(require 'lsp-ui)        ;; sideline, documentation popups, peek UI
(require 'flycheck)      ;; linting via flycheck

;;-----------------------;;
;; Check for local files ;;
;;-----------------------;;

(defun my/host-arch ()
  "Return a simple host descriptor like ubuntu24_04."
  (let* ((os (string-trim (shell-command-to-string "lsb_release -is | tr '[:upper:]' '[:lower:]'")))
         (ver (string-trim (shell-command-to-string "lsb_release -rs | tr '.' '_'"))))
    (format "%s%s" os ver)))

;; Load machine-specific init file if it exists
(let ((host-init-file (expand-file-name
                       (format "init.%s.el" (my/host-arch))
                       user-emacs-directory)))
  (when (file-exists-p host-init-file)
    (load-file host-init-file)))

;;-------------------------------------;;
;; Section for things added at runtime ;;
;;-------------------------------------;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((eval outline-hide-body))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
