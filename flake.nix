{
  description = "My Nix-managed dotfiles using Home Manager";
  
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
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
        system = "x86_64-darwin";
        modules = [
          ./home/mymac/default.nix
          inputs.home-manager.nixosModules.home-manager
        ];
        # specialArgs = { inherit inputs; };
      };
    };
    homeConfigurations = {
      myubuntu = inputs.home-manager.lib.homeManagerConfiguration {
	      pkgs = import inputs.nixpkgs {
	        system = "x86_64-linux";
	      };
        modules = [
          ./home/myubuntu/default.nix
          inputs.home-manager.homeManagerModules.home-manager
        ];
        # specialArgs = { inherit inputs; };
      };
    };
  };
}
