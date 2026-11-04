{ pkgs, ... }:
{
  environment.systemPackages = [
    pkgs.strata
  ];

  programs.omp.enable = true;

  services.strata = {
    enable = true;
    openFirewall = true;
    port = 8080;
  };
}
