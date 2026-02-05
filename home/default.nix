{
  pkgs,
  ...
}: {
  imports = [
    
  ];
  home = {
    username = "yuta";
    homeDirectory = "/home/yuta";
    stateVersion = "24.11";
    packages =
      with pkgs;
      [
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
      ];
    file = {
      ".gitignore_global".source = ../../git/.gitignore_global;
      ".gitconfig".source = ../../git/.gitconfig;
      ".emacs.d/init.el".source = ../../emacs/init.el;
      ".emacs.d/early-init.el".source = ../../emacs/early-init.el;
      ".emacs.d/templates".source = ../../emacs/templates;
      ".emacs.d/opam-user-setup.el".source = ../../emacs/opam-user-setup.el;
      ".emacs.d/themes".source = ../../emacs/themes;      
      ".ocp-indent".source = ../../ocaml/.ocp-indent;
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
}
