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
    # `nix build` builds the Home Manager generation for the current system;
    # `./result/activate` (or `nix run`) switches to it.
    packages = {
      aarch64-darwin.default = self.homeConfigurations.mymac.activationPackage;
      x86_64-linux.default = self.homeConfigurations.myubuntu.activationPackage;
    };
    apps = {
      aarch64-darwin.default = {
        type = "app";
        program = "${self.packages.aarch64-darwin.default}/activate";
      };
      x86_64-linux.default = {
        type = "app";
        program = "${self.packages.x86_64-linux.default}/activate";
      };
    };

    # macOS system settings only (Dock, Finder, keyboard, ...). The user
    # environment comes from homeConfigurations.mymac; see the README.
    darwinConfigurations = {
      mymac = inputs.darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [ ./darwin/default.nix ];
      };
    };
    homeConfigurations = {
      mymac = inputs.home-manager.lib.homeManagerConfiguration {
        pkgs = import inputs.nixpkgs {
          system = "aarch64-darwin";
          config.allowUnfree = true;
        };
        modules = [
          ./home/darwin.nix
        ];
      };
      myubuntu = inputs.home-manager.lib.homeManagerConfiguration {
        pkgs = import inputs.nixpkgs {
          system = "x86_64-linux";
          config.allowUnfree = true;
        };
        modules = [
          ./home/default.nix
        ];
      };
    };
  };
}
