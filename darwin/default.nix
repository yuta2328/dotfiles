{ pkgs, ... }:

{
  # Nix configuration
  nix.settings = {
    experimental-features = "nix-command flakes";
    trusted-users = [ "@admin" ];
  };

  # Garbage collect the whole store every Sunday at 03:15.
  nix.gc = {
    automatic = true;
    interval = { Weekday = 7; Hour = 3; Minute = 15; };
    options = "--delete-older-than 30d";
  };

  # System packages
  environment.systemPackages = with pkgs; [
    vim
    git
  ];

  # macOS system settings
  system = {
    defaults = {
      dock = {
        autohide = true;
        orientation = "bottom";
        show-recents = false;
        tilesize = 48;
      };
      
      finder = {
        AppleShowAllExtensions = true;
        FXEnableExtensionChangeWarning = false;
        _FXShowPosixPathInTitle = true;
      };
      
      NSGlobalDomain = {
        AppleInterfaceStyle = "Dark";
        AppleShowAllExtensions = true;
        InitialKeyRepeat = 15;
        KeyRepeat = 2;
        NSAutomaticCapitalizationEnabled = false;
        NSAutomaticSpellingCorrectionEnabled = false;
      };
    };
    
    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
    
    stateVersion = 5;
  };

  # The user that system.defaults and other per-user settings apply to.
  system.primaryUser = "yuta";

  # Shell configuration
  programs.fish.enable = true;

  # Used for backwards compatibility
  system.configurationRevision = null;
}
