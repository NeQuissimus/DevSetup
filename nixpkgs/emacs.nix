{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
  ]) ++ (with epkgs.melpaPackages; [
    auto-complete
    dockerfile-mode
    magit
    markdown-mode
    nix-mode
    projectile
    scala-mode
    smartparens
    undo-tree
    zerodark-theme
  ]) ++ (with epkgs.elpaPackages; [
    auctex
    beacon
    nameless
  ]))
