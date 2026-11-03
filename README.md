# Dotfiles

My dotfiles in Home-manager and Nix.

# Installation

1. Install [Nix](https://nixos.org/download/) via **Multi-user installation**.
2. Build and Install this project as follows (the configuration for the current system, `mymac` or `myubuntu`, is selected automatically):
``` shell
nix build          # build only; the result is linked at ./result
./result/activate  # switch to the built configuration
```
Or build and switch in one step:
``` shell
nix run
```
The standalone [Home-manager](https://github.com/nix-community/home-manager) CLI is not required, but `home-manager switch --flake .#myubuntu` still works if it is installed.

## Emacs Setup

After installing Emacs through this configuration, you need to reinstall LSP packages with plist support:

1. Start Emacs (packages will be installed automatically)
2. Run: `M-x lsp-reinstall-packages-for-plists`
3. Restart Emacs

This step is necessary because the configuration uses `LSP_USE_PLISTS=true` (set in `emacs/early-init.el`) for better LSP performance. LSP packages must be compiled with this environment variable to avoid hash-table/plist type errors.

