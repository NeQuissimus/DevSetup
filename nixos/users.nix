{ config, lib, pkgs, secrets, ... }:

{
  systemd.user.services.emacs.unitConfig.ConditionGroup = "users";

  users = {
    mutableUsers = false;

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
        createHome = true;
        hashedPassword =
          "$6$l9.p7uvza/su4w8p$dFidebmBvqAkUJMugKqkWYSmblJ9xv1X/KgVLKG4ZfiHxeiXiXg4HtvgJIqRQQ25LDF3YbZU2v/umQ8RqLudg.";
        name = "nequi";
        shell = pkgs.zsh;
        uid = 1000;
        openssh.authorizedKeys.keyFiles = [
          (builtins.fetchurl {
            url = "https://github.com/NeQuissimus.keys";
            sha256 =
              "sha256:0wbaszv6wz282fipnspi4hqdizbb9g4brdzcmdq4x3fma125k7br";
          })
        ];
      };
    };
  };
}
