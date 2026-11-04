# Linux (myubuntu)
{ pkgs, lib, ... }:

{
  imports = [ ./common.nix ];

  home = {
    homeDirectory = "/home/yuta";
    packages = with pkgs; [
      slack
      bitwarden-desktop
      # processor
      coq
      # emacs
      libvterm
    ];
  };

  nix.gc.randomizedDelaySec = "45min";

  programs.fish.shellInit = lib.mkBefore ''
    if test -f $HOME/miniconda3/bin/conda
        eval $HOME/miniconda3/bin/conda "shell.fish" "hook" $argv | source
    else if test -f "$HOME/miniconda3/etc/fish/conf.d/conda.fish"
        . "$HOME/miniconda3/etc/fish/conf.d/conda.fish"
    end
  '';
}
