{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  myEmacs = pkgs.emacs;

  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; []) ++ (with epkgs.melpaPackages; [
    all-the-icons
    ample-theme
    auto-complete
    company-lsp
    dockerfile-mode
    company
    editorconfig
    flycheck
    forge
    groovy-mode
    haskell-mode
    hasklig-mode
    hl-todo
    htmlize
    indent-guide
    json-mode
    lsp-mode
    lsp-ui
    magit
    markdown-mode
    multi-term
    multiple-cursors
    nix-mode
    nyan-mode
    ox-gfm
    projectile
    rainbow-delimiters
    rust-mode
    sbt-mode
    scala-mode
    smartparens
    smooth-scrolling
    treemacs
    treemacs-magit
    treemacs-icons-dired
    treemacs-projectile
    use-package
    yaml-mode
    yasnippet
  ]) ++ (with epkgs.elpaPackages; [
    beacon
  ]) ++ [
  ]
)
