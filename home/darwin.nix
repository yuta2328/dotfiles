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
      glib
      libtool
      texlab
      tex-fmt
      harper
      nil
      pyright
      google-java-format
    ];
    
    file = {
      ".gitignore_global".source = ../git/.gitignore_global;
      ".gitconfig".source = ../git/.gitconfig;
      ".emacs.d/init.el".source = ../emacs/init.el;
      ".emacs.d/early-init.el".source = ../emacs/early-init.el;
      ".emacs.d/templates".source = ../emacs/templates;
      ".emacs.d/opam-user-setup.el".source = ../emacs/opam-user-setup.el;
      ".emacs.d/ef-oreore-theme.el".source = ../emacs/ef-oreore-theme.el;
      ".ocp-indent".source = ../ocaml/.ocp-indent;
    };
  };

  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      monospace = [ "Source Han Code JP" "Nerd Font Symbols" ];
      sansSerif = [ "Source Han Code JP" ];
      serif = [ "Source Han Code JP" ];
      emoji = [ "Twitter Color Emoji" ];
    };
  };

  programs.fish = {
    enable = true;
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
    };
    interactiveShellInit = ''
      source ~/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
    '';
    shellInit = ''
      if test -f /opt/miniconda3/bin/conda
          eval /opt/miniconda3/bin/conda "shell.fish" "hook" $argv | source
      else
          if test -f "/opt/miniconda3/etc/fish/conf.d/conda.fish"
              . "/opt/miniconda3/etc/fish/conf.d/conda.fish"
          else
              set -x PATH "/opt/miniconda3/bin" $PATH
          end
      end

      set -gx PATH /opt/homebrew/bin $PATH
      set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
      set -gx PATH $HOME/.cabal/bin $HOME/.ghcup/bin $PATH
      set -gx PATH $PATH $HOME/.local/bin
      set -gx AGDA_DIR $HOME/.config/agda
    '';
  };
}
