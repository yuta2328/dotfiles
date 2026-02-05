{ pkgs,
  inputs,
  ...
} :
with pkgs ;
[
  # GUI
  slack
  bitwarden-desktop
  emacs
  ghostty
  # CLI
  curl
  fish
  git
  gh
  gemini-cli
  github-copilot-cli
  # font
  source-han-code-jp
  twitter-color-emoji
  # processor
  rustup
  texliveFull
  opam
  coq
  rlwrap
  stack
  python314
  nodejs_24
  php
  docker
  php84Packages.composer
  # emacs
  udev-gothic-nf
  nerd-fonts.symbols-only
  ripgrep
  cmake
  libvterm
  libtool
  tex-fmt
  harper
  nil
  pyright
]
