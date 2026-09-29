{ pkgs, ... }:

{
  home = {
    username = "yuta";
    homeDirectory = "/Users/yuta";
    stateVersion = "24.11";
    
    packages = with pkgs; [
      emacs
      # CLI
      curl
      fish
      git
      gh
      lsd
      cmake
      curl
      codex
      claude-code
      coursier
      # font
      plemoljp
      twitter-color-emoji
      # processor
      rustup
      texliveFull
      opam
      rlwrap
      stack
      python314
      nodejs_24
      sqlite
      swi-prolog
      # emacs
      udev-gothic-nf
      nerd-fonts.symbols-only
      ripgrep
      cmake
      glib
      libtool
      texlab
      tex-fmt
      harper
      metals
      nil
      pyright
    ];
    
    file = {
      ".gitignore_global".source = ../git/.gitignore_global;
      ".gitconfig".source = ../git/.gitconfig;
      ".emacs.d/init.el".source = ../emacs/init.el;
      ".emacs.d/early-init.el".source = ../emacs/early-init.el;
      ".emacs.d/templates".source = ../emacs/templates;
      ".emacs.d/opam-user-setup.el".source = ../emacs/opam-user-setup.el;
      ".emacs.d/ef-oreore-theme.el".source = ../emacs/ef-oreore-theme.el;
      ".emacs.d/ef-oreoredark-theme.el".source = ../emacs/ef-oreoredark-theme.el;
      ".ocp-indent".source = ../ocaml/.ocp-indent;
    };
  };

  # Garbage collect this user's profile generations weekly.
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      monospace = [ "PlemolJP" "Nerd Font Symbols" ];
      sansSerif = [ "PlemolJP" ];
      serif = [ "PlemolJP" ];
      emoji = [ "Twitter Color Emoji" ];
    };
  };

  programs.fish = {
    enable = true;
    plugins = [
      {
        name = "bass";
        inherit (pkgs.fishPlugins.bass) src;
      }
    ];
    shellAliases = {
      chrome = ''open -a "Google Chrome"'';
      ocaml = "rlwrap ocaml";
      metaocaml = "rlwrap metaocaml";
      rel = "exec $SHELL -l";
      icloud = "cd ~/Library/Mobile\\ Documents/com~apple~CloudDocs/";
    };
    functions = {
      vterm_printf = {
        body = ''
          if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end
              printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
          else if string match -q -- "screen*" "$TERM"
              printf "\eP\e]%s\007\e\\" "$argv"
          else
              printf "\e]%s\e\\" "$argv"
          end
        '';
      };
      sdk = {
        description = "Run SDKMAN from Fish";
        body = ''
          set -l sdkman_init $HOME/.sdkman/bin/sdkman-init.sh
          if not test -f $sdkman_init
              echo "sdk: $sdkman_init not found" >&2
              return 1
          end

          bass source $sdkman_init ';' sdk $argv
        '';
      };
    };
    interactiveShellInit = ''
      source ~/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
    '';
    shellInit = ''
      set -gx PATH /opt/homebrew/bin $PATH
      set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
      set -gx PATH $HOME/.cabal/bin $HOME/.ghcup/bin $PATH
      set -gx PATH $PATH $HOME/.local/bin
      set -gx AGDA_DIR $HOME/.config/agda
    '';
  };
}
