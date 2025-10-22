{
  pkgs,
  inputs,
  ...
}: {
  imports = [
    
  ];
  home = {
    username = "yuta";
    homeDirectory = "/Users/yuta";
    stateVersion = "24.11";
    packages = with pkgs;
      [
        skimpdf
        iterm2
        (import ./../commonPkg.nix { inherit pkgs inputs; } )
      ];
  };
}
