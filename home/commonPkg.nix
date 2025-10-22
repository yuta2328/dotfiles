{ pkgs,
  inputs,
  ...
} :
with pkgs ;
[
  curl
  dotter
  fish
  git
  gh
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
  # gui
  emacs
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
