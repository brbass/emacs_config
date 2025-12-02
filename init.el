;; Window switching
(global-set-key (kbd "M-o") 'other-window)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Remove anything that could encourage mouse use and weakness
(menu-bar-mode -1)    ;; Removes the top menu bar
(tool-bar-mode -1)    ;; Removes the button toolbar
(scroll-bar-mode -1)  ;; Removes the right scroll bar

;; Disable aggressive tab completion (Emacs default behavior)
(setq completion-auto-help 'lazy)
(setq completion-auto-select nil)
(setq completion-styles '(basic partial-completion emacs22))

;; Clean file complete
(setq minibuffer-auto-raise t)

;; Package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install packages if needed
(dolist (pkg '(consult marginalia orderless embark embark-consult vertico))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Vertico - incremental completion
(require 'vertico)
(vertico-mode 1)
(setq vertico-count 10
      vertico-resize nil
      vertico-cycle nil)

;; Marginalia - annotations in completion
(require 'marginalia)
(marginalia-mode)

;; Orderless - flexible matching
(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles orderless partial-completion))))

;; Consult for better search commands
(require 'consult)
(defun consult-git-files ()
  "Select a Git-tracked file, ranked by basename first."
  (interactive)
  (let ((default-directory (or (locate-dominating-file default-directory ".git")
                               default-directory)))
    (unless (file-directory-p (concat default-directory ".git"))
      (user-error "Not inside a Git repository"))
    (let* ((files (split-string (shell-command-to-string "git ls-files") "\n" t))
           ;; Sort files by basename alphabetically
           (sorted-files
            (sort files
                  (lambda (a b)
                    (string-lessp (file-name-nondirectory a)
                                  (file-name-nondirectory b))))))
      (consult--read sorted-files
                     :prompt "Git files: "
                     :require-match t
                     :sort nil
                     :history 'file-name-history
                     :category 'file
                     :state (consult--file-state)))))
(global-set-key (kbd "C-c s") 'consult-ripgrep)     ; Search in project with ripgrep
(global-set-key (kbd "C-c l") 'consult-line)        ; Search lines in current buffer
(global-set-key (kbd "C-c f") 'consult-git-files)        ; Find files (git-aware)

;; Acting on selected files
(require 'embark)
(require 'embark-consult)
(global-set-key (kbd "C-;") 'embark-act)

;; Eglot for LSP (Emacs 29+, or install from package)
(add-hook 'python-mode-hook 'eglot-ensure)        ; python-lsp-server or pyright
(add-hook 'c++-mode-hook 'eglot-ensure)           ; clangd
(add-hook 'c-mode-hook 'eglot-ensure)             ; clangd
(add-hook 'fortran-mode-hook 'eglot-ensure)       ; fortls
(add-hook 'f90-mode-hook 'eglot-ensure)           ; fortls (for modern Fortran)
(add-hook 'sh-mode-hook 'eglot-ensure)            ; bash-language-server

;; Performance tweaks
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
