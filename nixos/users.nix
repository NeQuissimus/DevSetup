{ config, lib, pkgs, secrets, ... }:

{
  systemd.user.services.emacs.unitConfig.ConditionGroup = "users";

  users = {
    mutableUsers = true;

    groups = { docker = { }; };

    users = {
      docker = {
        group = "docker";
        isSystemUser = true;
        subUidRanges = [{
          startUid = 100000;
          count = 65536;
        }];
        subGidRanges = [{
          startGid = 100000;
          count = 65536;
        }];
      };

      nequi = {
        extraGroups = [ ]
          ++ lib.optionals config.virtualisation.docker.enable [ "docker" ]
          ++ lib.optionals config.security.sudo.enable [ "wheel" ];
        isNormalUser = true;
        name = "nequi";
        shell = pkgs.bash;
        uid = 1000;
        openssh.authorizedKeys.keyFiles = [
          (builtins.fetchurl {
            url = "https://github.com/NeQuissimus.keys";
            sha256 =
              "sha256:0sidlq3gwzdms5v9a81qxdwk3xi51d1gjkd259f0pl4x64nj4b4y";
          })
        ];
      };
    };
  };
}
