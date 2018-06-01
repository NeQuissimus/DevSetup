{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
  ]) ++ (with epkgs.melpaPackages; [
    auto-complete
    dockerfile-mode
    haskell-mode
    hl-todo
    indent-guide
    json-mode
    magit
    markdown-mode
    neotree
    nix-mode
    projectile
    rust-mode
    scala-mode
    smartparens
    smooth-scrolling
    use-package
    yaml-mode
    zerodark-theme
  ]) ++ (with epkgs.elpaPackages; [
    beacon
    nameless
  ]))
