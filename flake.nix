{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";

    impermanence.url = "github:nix-community/impermanence";
  };

  outputs = { self, nixpkgs, impermanence, ... }: {
    nixosConfigurations = {
      c220 = nixpkgs.lib.nixosSystem {
        modules = [
          impermanence.nixosModules.impermanence
          ./hosts/c220/configuration.nix
        ];

        system = "x86_64-linux";
      };

      supermicro = nixpkgs.lib.nixosSystem {
        modules = [
          impermanence.nixosModules.impermanence
          ./hosts/supermicro/configuration.nix
        ];

        system = "x86_64-linux";
      };

      topton = nixpkgs.lib.nixosSystem {
        modules = [
          impermanence.nixosModules.impermanence
          ./hosts/topton/configuration.nix
        ];

        system = "x86_64-linux";
      };
    };
  };
}
