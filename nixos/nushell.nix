{ config, lib, pkgs, ... }: {
  users.defaultUserShell = "${pkgs.nushell}/bin/nu";
}
