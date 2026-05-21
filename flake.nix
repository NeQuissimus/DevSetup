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
    nixos-raspberrypi = {
      url = "github:nvmd/nixos-raspberrypi/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs, nixos-raspberrypi, ... }:
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
        c220 = mkNixosSystem "c220";
        supermicro = mkNixosSystem "supermicro";
        topton = mkNixosSystem "topton";

        rpi4b = nixos-raspberrypi.lib.nixosSystem {
          system = "aarch64-linux";
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
      };
    };
}
