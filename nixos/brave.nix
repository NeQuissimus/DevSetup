{ pkgs, ... }:
{
  environment = {
    etc."brave/policies/managed/GroupPolicy.json".source = ./brave/GroupPolicy.json;
    systemPackages = [ pkgs.brave-origin ];
  };
}
