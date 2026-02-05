# Dotfiles

My dotfiles in Home-manager and Nix.

# Installation

1. Install [Nix](https://nixos.org/download/) via **Multi-user installation**.
2. Install [Home-manager](https://github.com/nix-community/home-manager)
3. Build and Install this project as follows:
``` shell
home-manager build --flake .#myubuntu
home-manager switch --flake .#myubuntu
```

