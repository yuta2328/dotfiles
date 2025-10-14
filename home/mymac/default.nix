{
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ../commonPkg.nix
  ];

  home = {
    username = "yuta";
    homeDirectory = "/home/yuta";
    stateVersion = "24.11";
    packages = with pkgs;
      [
        skimpdf
        iterm2
        (import ./commonPkg.nix { inherit pkgs inputs; } )
      ];
  };
}
