{
  networking.wireless.extraConfig = ''
    bgscan="learn:30:-45:300:/var/lib/wpa_supplicant/db.bgscan"
  '';

  systemd.tmpfiles.rules = [
    "d /var/lib/wpa_supplicant 777 root wheel"
  ] ;
}
