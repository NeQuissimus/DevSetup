{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
  ]) ++ (with epkgs.melpaPackages; [
    auto-complete
    dockerfile-mode
    feature-mode
    groovy-mode
    haskell-mode
    hl-todo
    indent-guide
    json-mode
    markdown-mode
    nix-mode
    rspec-mode
    smartparens
    smooth-scrolling
    spacemacs-theme
    undo-tree
    use-package
    yaml-mode
  ]) ++ (with epkgs.elpaPackages; [
    beacon
  ]) ++ [
    pkgs.notmuch
  ])
