# macOS (mymac)
{ config, lib, pkgs, ... }:

{
  imports = [ ./common.nix ];

  home.homeDirectory = "/Users/yuta";

  # launchd starts agents with a bare environment; give the daemon the login
  # shell so exec-path-from-shell can import PATH from fish.
  launchd.agents.emacs.config.EnvironmentVariables.SHELL = "${pkgs.fish}/bin/fish";

  # home-manager passes nix.gc.options as one argv element on Darwin;
  # split it so nix-collect-garbage sees "--delete-older-than" "30d".
  launchd.agents.nix-gc.config.ProgramArguments = lib.mkForce (
    [ "${pkgs.nix}/bin/nix-collect-garbage" ]
    ++ lib.splitString " " config.nix.gc.options
  );

  programs.fish.shellAliases = {
    chrome = ''open -a "Google Chrome"'';
    icloud = "cd ~/Library/Mobile\\ Documents/com~apple~CloudDocs/";
  };
}
