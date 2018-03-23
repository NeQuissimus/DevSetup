{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
  ]) ++ (with epkgs.melpaPackages; [
    auto-complete
    dockerfile-mode
    indent-guide
    json-mode
    magit
    markdown-mode
    nix-mode
    projectile
    restclient
    rust-mode
    scala-mode
    smartparens
    smooth-scrolling
    undo-tree
    use-package
    yaml-mode
    zerodark-theme
  ]) ++ (with epkgs.elpaPackages; [
    beacon
    nameless
  ]))
