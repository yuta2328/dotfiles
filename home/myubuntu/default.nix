{
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ../../modules/commonPkg
  ];

  home = {
    username = "yuta";
    homeDirectory = "/home/yuta";
    stateVersion = "24.11";
    packages = with pkgs;
      [
        (import ./commonPkg.nix { inherit pkgs inputs; } )
      ];
  };
}
