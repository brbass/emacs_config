# Emacs config

## Features

This keeps mostly default emacs but adds the following:
- Language server support
- Support for searching for and within files quickly
- Some changes to completion that keep things similar to default emacs
- Easier management of async shell commands
- Performance tweaks
Package loading is defered where possible to make startup fast. 

## Keyboard bindings

```lisp
;; General commands
M-o ;; Switch between windows using ace-window
C-c r ;; Reload/revert buffer

;; Running shell commands in buffers
M-* ;; Run selected code as an async shell command
M-| ;; Run current line as an async shell command
M-& ;; Run an async shell command

;; vterm commands
C-c C-j ;; Switch to copy mode in vterm
C-c C-k ;; Switch to normal vterm mode

;; embark commands
C-c . ;; Get a selection of things to do when in the minibuffer
C-c ; ;; Open the minibuffer in a buffer

;; consult commands
C-c f ;; Switch to an alternate file in the current project using git-ls-files (only files in git repo)
C-c g ;; Grep using ripgrep in the current project
C-c s ;; Grep using git grep in the current project (only files in git repo)
C-c l ;; grep over lines in current file

;; lsp commands
M-' ;; Find references to variables
```

## Installation

### Build emacs from source

```bash
sudo apt update
sudo apt build-dep emacs
wget https://ftp.gnu.org/gnu/emacs/emacs-30.2.tar.gz
tar -zxf emacs-30.2.tar.gz
cd emacs-30.2
mkdir build install
cd build
../configure --prefix="$PWD/../install"
make -j 16
make install
```

### Add to path

```bash
export PATH="/path/to/emacs-30.2/install/bin:$PATH"
```

### Install required packages

```bash
# Language servers
sudo apt install clangd
sudo apt install fortran-language-server
npm install -g pyright
npm install -g bash-language-server
rustup component add rust-analyzer

# Search tools
sudo apt install ripgrep

# For vterm
sudo apt install libvterm-dev
```

### Back up your existing config if needed

```bash
mv .emacs .emacs.backup
mv .emacs.d .emacs.d.backup
```

### Clone this package

```bash
git clone https://github.com/brbass/.emacs.d.git ~/.emacs.d
```

## Features to add:

- File preview when using find-file

## Other setup notes

- If a package is not found, run `M-x package-refresh-contents` and try opening emacs again.

