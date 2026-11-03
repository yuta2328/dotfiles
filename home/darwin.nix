# macOS (mymac)
{ pkgs, lib, ... }:

{
  imports = [ ./common.nix ];

  home.homeDirectory = "/Users/yuta";

  # launchd starts agents with a bare environment; give the daemon the login
  # shell so exec-path-from-shell can import PATH from fish.
  launchd.agents.emacs.config.EnvironmentVariables.SHELL = "${pkgs.fish}/bin/fish";

  programs.fish = {
    shellAliases = {
      chrome = ''open -a "Google Chrome"'';
      icloud = "cd ~/Library/Mobile\\ Documents/com~apple~CloudDocs/";
    };
    shellInit = lib.mkBefore ''
      set -gx PATH /opt/homebrew/bin $PATH
    '';
  };
}
