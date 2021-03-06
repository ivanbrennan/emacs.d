# vmacs

## Which Emacs?
### [Emacs Mac Port](https://github.com/railwaycat/homebrew-emacsmacport) (forked from GNU source)
- ✓ smooth-scrolling
- ⨉ best performance
- ⨉ gui and tty can use same server

install:

    brew tap railwaycat/emacsmacport
    brew install emacs-mac --with-spacemacs-icon [--HEAD]

reinstall:

    brew uninstall emacs-mac && brew install emacs-mac --with-spacemacs-icon [--HEAD]

### [Emacs Plus](https://github.com/d12frosted/homebrew-emacs-plus) (GNU source with compile options)
- ⨉ smooth-scrolling
- ✓ best performance
- ✓ gui and tty can use same server

install:

    brew tap d12frosted/emacs-plus
    brew install emacs-plus --with-cocoa --with-gnutls --with-librsvg \
      --with-imagemagick --with-spacemacs-icon [--HEAD]

reinstall:

    brew uninstall emacs-plus && brew install emacs-plus --with-cocoa \
      --with-gnutls --with-librsvg --with-imagemagick --with-spacemacs-icon [--HEAD]
___
## Working full screen
### *To prevent toggle-frame-fullscreen from blacking out external displays:*
  - non-native (not available in Emacs Mac Port)
    - (setq ns-use-native-fullscreen nil)
  - System Preferences > Mission Control
    - uncheck "Displays have separate spaces"

## Install gtags with universal-ctags backend

    cd $(brew --repo homebrew/core)
    git remote add ivanbrennan git@github.com:ivanbrennan/homebrew-core.git
    git checkout global-with-universal-ctags
    brew install global --with-universal-ctags
    git checkout master
    cd -

    export GTAGSCONF=/usr/local/share/gtags/gtags.conf
    export GTAGSLABEL=new-ctags

## Install gnutls and certifi to facilitate TLS

    brew install gnutls
    pip install certifi
