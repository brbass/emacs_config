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

### Install LSP servers

```bash
sudo apt install clangd
sudo apt install fortran-language-server
npm install -g pyright
npm install -g bash-language-server
```

### Back up your existing config if needed

```bash
mv .emacs .emacs.backup
mv .emacs.d .emacs.d.backup
```

### Clone this package

```bash
git clone https://github.com/brbass/emacs_config.git ~/.emacs.d
```
