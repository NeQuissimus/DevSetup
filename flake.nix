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
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-raspberrypi = {
      url = "github:nvmd/nixos-raspberrypi/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-cachyos-kernel.url = "github:xddxdd/nix-cachyos-kernel/release";
  };

  outputs =
    { self, nixpkgs, nixpkgs-unstable, nixos-raspberrypi, nix-cachyos-kernel, ... }:
    let
      mkNixosSystem =
        host:
        nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [ ./hosts/${host}/configuration.nix ];
        };
    in
    {
      nixosConfigurations = {
        armour = mkNixosSystem "armour";
        c220 = mkNixosSystem "c220";
        supermicro = mkNixosSystem "supermicro";
        topton = mkNixosSystem "topton";

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

        opi5plus = nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          specialArgs = { ipv4Address = "10.0.0.54"; nixpkgs = nixpkgs-unstable; };
          modules = [
            ./hosts/opi5plus/configuration.nix
            ./hosts/opi5plus/sdcard.nix
          ];
        } // {
          packages = {
            sdImage = self.nixosConfigurations.opi5plus.config.system.build.sdImage;
          };
        };
      };
    };
}
