## Emacs config

This keeps mostly default emacs but adds the following:
- Language server support
- Support for searching for and within files quickly
- Some changes to completion that keep things similar to default emacs
- Performance tweaks

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

## Other setup nodes

- If a package is not found, run `M-x package-refresh-contents` and try opening emacs again. 
- Not emacs-related, but the ghostty terminal needs the following fix for using ssh:
```bash
infocmp -x xterm-ghostty | ssh YOUR-SERVER -- tic -x -
```

