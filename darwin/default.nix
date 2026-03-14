{ pkgs, ... }:

{
  # Nix configuration
  nix.settings = {
    experimental-features = "nix-command flakes";
    trusted-users = [ "@admin" ];
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

  # Shell configuration
  programs.fish.enable = true;

  # Auto-upgrade nix package and the daemon service
  services.nix-daemon.enable = true;

  # Used for backwards compatibility
  system.configurationRevision = null;
}
