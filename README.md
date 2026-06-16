# Dotfiles

My dotfiles in Home-manager and Nix.

# Installation

1. Install [Nix](https://nixos.org/download/) via **Multi-user installation**.
2. Install [Home-manager](https://github.com/nix-community/home-manager). [This section](https://nix-community.github.io/home-manager/index.xhtml#sec-install-standalone) may be useful for the installation.
3. Build and Install this project as follows:
``` shell
home-manager build --flake .#myubuntu
home-manager switch --flake .#myubuntu
```

## Emacs Setup

After installing Emacs through this configuration, you need to reinstall LSP packages with plist support:

1. Start Emacs (packages will be installed automatically)
2. Run: `M-x lsp-reinstall-packages-for-plists`
3. Restart Emacs

This step is necessary because the configuration uses `LSP_USE_PLISTS=true` (set in `emacs/early-init.el`) for better LSP performance. LSP packages must be compiled with this environment variable to avoid hash-table/plist type errors.

