;;-----------------;;
;; Deferred tuning ;;
;;-----------------;;
;; Defer GC during minibuffer input
(add-hook 'minibuffer-setup-hook
          (lambda () (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook
          (lambda () (setq gc-cons-threshold (* 32 1024 1024))))

;; Native compilation settings (if available)
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent))

;; Print out how long Emacs took to load
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s (%d GCs)"
                     (emacs-init-time)
                     gcs-done)))

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

;; Avoid automatic splitting of windows vertically
(setq split-height-threshold nil)

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
           (when dir
             (setq default-directory dir)
             (when (fboundp 'direnv-update-environment)
               (direnv-update-environment)))
           (when new-path (setenv "PATH" (concat new-path ":" old-path)))
           ;; Run the command
           ,@body)
       ;; If dir and path exist, then restore the original values
       (when new-path (setenv "PATH" old-path))
       (when dir
         (setq default-directory orig-dir)
         (when (fboundp 'direnv-update-environment)
           (direnv-update-environment))))))
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

;; To auto-hide the body in outline mode, add this line
;;   ((markdown-mode . ((eval . (outline-hide-body)))))
;; to the .dir-locals.el in the directory with a file
(setq safe-local-variable-values '((eval outline-hide-body)))

;;-----------------;;
;; Set up packages ;;
;;-----------------;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Ace window for switching windows easily
(use-package ace-window
  :defer t
  :bind (("M-o" . ace-window))
  :init
  (with-eval-after-load 'term
    (define-key term-raw-map (kbd "M-o") 'ace-window))
  :config
  (setq aw-scope 'frame))

;; Recursive tree comparison
(use-package ztree
  :defer t
  :commands (ztree-diff ztree-dir))

;; For using git in emacs
(use-package magit
  :defer t
  :commands (magit-status magit-dispatch magit-file-dispatch))

;; Performant terminal emulator
(use-package vterm
  :defer t
  :commands vterm
  :bind (:map vterm-mode-map
         ("C-c C-j" . vterm-copy-mode)
         :map vterm-copy-mode-map
         ("C-c C-k" . vterm-copy-mode)))

;; For automatically loading direnv
(use-package direnv
  :config
  (direnv-mode))
(add-to-list 'warning-suppress-types '(direnv)) ;; otherwise opens warning buffer if direnv fails

;; Better color theme
;; (use-package gruvbox-theme
;;   :config
;;   (load-theme 'gruvbox-dark-hard t))
(use-package nordic-night-theme
  :config
  (load-theme 'nordic-night t))

;; Show available keybindings after prefix
(use-package which-key
  :config
  (which-key-mode))

;; Vertico - incremental completion
(use-package vertico
  :init
  (vertico-mode 1)
  :config
  (setq vertico-count 30
        vertico-resize nil
        vertico-cycle nil)
  ;; (vertico-buffer-mode 1)
  ;; (setq vertico-buffer-display-action
  ;;       '(display-buffer-in-side-window
  ;;         (side . left)
  ;;         (window-width . 0.3)))
  :bind (:map vertico-map
         ("TAB" . minibuffer-complete)))

;; Marginalia - annotations in completion
(use-package marginalia
  :init
  (marginalia-mode))

;; Orderless - flexible matching
(use-package orderless
  :custom
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles orderless partial-completion))))
  ;; (completion-styles '(orderless basic))
  (completion-styles '(basic substring partial-completion flex))
  (completion-pcm-leading-wildcard t))

;; Acting on selected files
(use-package embark
  :defer t
  :bind (("C-c ." . embark-act)
         ("C-c ;" . embark-export)))

(use-package embark-consult
  :after embark)

;; Consult for better search commands
(use-package consult
  :defer t
  :bind (("C-c g" . consult-ripgrep)
         ("C-c s" . consult-git-grep)
         ("C-c l" . consult-line)))

(use-package consult-ls-git
  :defer t
  :bind (("C-c f" . consult-ls-git-ls-files)))

;; Markdown support
(use-package markdown-mode
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t
        markdown-enable-math t
        markdown-header-scaling t)
  :custom-face
  (markdown-header-face-1 ((t (:inherit font-lock-keyword-face :weight bold :underline t))))
  (markdown-header-face-2 ((t (:inherit font-lock-function-name-face :weight bold))))
  (markdown-header-face-3 ((t (:inherit font-lock-type-face :weight bold))))
  (markdown-header-face-4 ((t (:inherit font-lock-constant-face :weight bold))))
  (markdown-header-face-5 ((t (:inherit font-lock-variable-name-face :slant italic))))
  (markdown-header-face-6 ((t (:inherit font-lock-comment-face :slant italic))))
  (markdown-comment-face ((t (:inherit font-lock-comment-face :slant italic))))
  (markdown-italic-face ((t (:inherit font-lock-builtin-face :slant italic)))))

;; Language server support
(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
  :bind (("M-'" . lsp-find-references))
  :config
  (setq lsp-prefer-flymake nil
        lsp-session-file nil
        lsp-log-io nil
        lsp-enable-on-type-formatting nil
        lsp-enable-indentation nil)
  ;; Uncomment to enable LSP automatically in specific modes:
  ;; :hook ((python-mode . lsp-deferred)
  ;;        (c++-mode . lsp-deferred)
  ;;        (c-mode . lsp-deferred)
  ;;        (fortran-mode . lsp-deferred)
  ;;        (f90-mode . lsp-deferred)
  ;;        (sh-mode . lsp-deferred)
  ;;        (rust-mode . lsp-deferred))
  )

(use-package lsp-pyright
  :after lsp-mode)

(use-package lsp-treemacs
  :after lsp-mode)

(use-package lsp-ui
  :after lsp-mode)

(use-package flycheck
  :after lsp-mode)

;; In-buffer completion popup (loads with LSP)
(use-package corfu
  :after lsp-mode
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.2
        corfu-auto-prefix 2))

(use-package corfu-terminal
  :after corfu
  :init
  (corfu-terminal-mode))

;; LLM support
(use-package agent-shell
  :defer t)

;;-----------------------;;
;; Check for local files ;;
;;-----------------------;;

;; (defun my/host-arch ()
;;   "Return a simple host descriptor like ubuntu24_04."
;;   (let* ((os (string-trim (shell-command-to-string "lsb_release -is | tr '[:upper:]' '[:lower:]'")))
;;          (ver (string-trim (shell-command-to-string "lsb_release -rs | tr '.' '_'"))))
;;     (format "%s%s" os ver)))

;; ;; Load machine-specific init file if it exists
;; (let ((host-init-file (expand-file-name
;;                        (format "init.%s.el" (my/host-arch))
;;                        user-emacs-directory)))
;;   (when (file-exists-p host-init-file)
;;     (load-file host-init-file)))

;;-------------------------------------;;
;; Section for things added at runtime ;;
;;-------------------------------------;;
