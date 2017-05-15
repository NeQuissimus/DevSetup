{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
  ]) ++ (with epkgs.melpaPackages; [
    ensime
    markdown-mode
    use-package
  ]) ++ (with epkgs.elpaPackages; [
    beacon
  ]) ++ [
    pkgs.notmuch
  ])
