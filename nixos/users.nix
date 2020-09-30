{ config, lib, pkgs, secrets, ... }:

{
  systemd.user.services.emacs.unitConfig.ConditionGroup = "users";

  users = {
    users = {
      docker = {
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
        uid = 1000;
        openssh.authorizedKeys.keyFiles = [
          (builtins.fetchurl {
            url = "https://github.com/NeQuissimus.keys";
            sha256 = "0jqxaz61n0jvm25bj7nyxjfkbsdrj1cm477fwxqm32wpfz0b23lf";
          })
        ];
      };

      # Make sure to have a user with a password :D
      root.shell = if (lib.any (x: x != null) (map (user:
        (user.hashedPassword != null || user.password != null
          || user.passwordFile != null))
        (lib.attrValues config.users.users))) then
        pkgs.nologin
      else
        pkgs.bash;
    };
  };
}
