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

