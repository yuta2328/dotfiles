{
  description = "My Nix-managed dotfiles using Home Manager";
  
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin.url = "github:LnL7/nix-darwin"; # macOS用
    darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, ... }@inputs: {
    darwinConfigurations = {
      mymac = inputs.darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          ./darwin/default.nix
          inputs.home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.yuta = import ./home/darwin.nix;
          }
        ];
      };
    };
    homeConfigurations = {
      mymac = inputs.home-manager.lib.homeManagerConfiguration {
        pkgs = import inputs.nixpkgs {
          system = "aarch64-darwin";
        };
        modules = [
          ./home/darwin.nix
        ];
      };
      myubuntu = inputs.home-manager.lib.homeManagerConfiguration {
        pkgs = import inputs.nixpkgs {
          system = "x86_64-linux";
        };
        modules = [
          ./home/default.nix
        ];
      };
    };
  };
}
