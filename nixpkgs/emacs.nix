{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
  ]) ++ (with epkgs.melpaPackages; [
    all-the-icons
    auto-complete
    dockerfile-mode
    flycheck
    forge
    haskell-mode
    hl-todo
    indent-guide
    json-mode
    lsp-mode
    lsp-ui
    magit
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
