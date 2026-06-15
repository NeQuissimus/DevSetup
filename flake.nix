{
  description = "NeQuissimus NixOS configurations";

  nixConfig = {
    extra-substituters = [
      "https://nixos-raspberrypi.cachix.org"
    ];

    extra-trusted-public-keys = [
      "nixos-raspberrypi.cachix.org-1:4iMO9LXa8BqhU+Rpg6LQKiGa2lsNh/j2oiYLNOQ5sPI="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixos-raspberrypi = {
      url = "github:nvmd/nixos-raspberrypi/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-cachyos-kernel.url = "github:xddxdd/nix-cachyos-kernel/release";
    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-rk3588 = {
      url = "github:gnull/nixos-rk3588";
    };
  };

  outputs =
    { self, nixpkgs, nixpkgs-unstable, nixos-raspberrypi, nix-cachyos-kernel, nur, nixos-rk3588, ... }:
    let
      mkNixosSystem =
        host:
        ipv4Address:
        nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [ ./hosts/${host}/configuration.nix ];
          specialArgs = { inherit ipv4Address; };
        };
    in
    {
      nixosConfigurations = {
        armour = mkNixosSystem "armour";
        #c220 = mkNixosSystem "c220";
        #supermicro = mkNixosSystem "supermicro";
        topton = mkNixosSystem "topton" "10.102.0.37";

        rpi4b = nixos-raspberrypi.lib.nixosSystem {
          system = "aarch64-linux";
          specialArgs = { ipv4Address = "10.0.0.53"; };
          modules = [
            (
              { nixos-raspberrypi, ... }:
              {
                imports = with nixos-raspberrypi.nixosModules; [
                  raspberry-pi-4.base
                ];
              }
            )
            ./hosts/rpi4b/configuration.nix
          ];
        };

        opi5plus = nixpkgs-unstable.lib.nixosSystem {
          system = "aarch64-linux";
          specialArgs = { 
            ipv4Address = "10.0.0.54"; 
            rk3588 = {
              nixpkgs = nixpkgs-unstable;
              pkgsKernel = import nixpkgs-unstable { system = "aarch64-linux"; };
            };
          };
          modules = [
            nixos-rk3588.nixosModules.boards.orangepi5plus.core
            nixos-rk3588.nixosModules.boards.orangepi5plus.sd-image

            ./hosts/opi5plus/configuration.nix
          ];
        } // {
          packages = {
            sdImage = self.nixosConfigurations.opi5plus.config.system.build.sdImage;
          };
        };
      };
    };
}
