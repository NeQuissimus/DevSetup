{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
    magit
  ]) ++ (with epkgs.melpaPackages; [
    all-the-icons
    auto-complete
    dockerfile-mode
    flycheck
    haskell-mode
    hl-todo
    indent-guide
    json-mode
    lsp-mode
    lsp-ui
    markdown-mode
    multiple-cursors
    neotree
    nix-mode
    projectile
    sbt-mode
    scala-mode
    smartparens
    smooth-scrolling
    use-package
    yaml-mode
    zerodark-theme
  ]) ++ (with epkgs.elpaPackages; [
    beacon
  ]))
