{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  myEmacs = pkgs.emacs;

  # Patched to work with aggressive-indent
  myScalaMode = (pkgs.emacsPackagesNgGen myEmacs).melpaBuild {
    pname = "scala-mode";
    ename = "scala-mode";
    version = "20190607";
    src = fetchFromGitHub {
      owner = "NeQuissimus";
      repo = "emacs-scala-mode";
      rev = "b98fcdf2eb4813a53ab8bb881758909c2a6e5f2e";
      sha256 = "1dqcakdimi79qqk843b9gxp8b4ch8sax0bbwp1ywvkv0k21ikjwq";
    };
    recipe = fetchurl {
      url = "https://raw.githubusercontent.com/milkypostman/melpa/564aa1637485192a97803af46b3a1f8e0d042c9a/recipes/scala-mode";
      sha256 = "12x377iw085fbkjb034dmcsbi7hma17zkkmbgrhkvfkz8pbgaic8";
      name = "recipe";
    };
    packageRequires = [];
    meta = {
      homepage = "https://melpa.org/#/scala-mode";
      license = lib.licenses.free;
    };
  };

  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; []) ++ (with epkgs.melpaPackages; [
    all-the-icons
    auto-complete
    company-lsp
    dockerfile-mode
    company
    editorconfig
    flycheck
    forge
    groovy-mode
    haskell-mode
    hl-todo
    indent-guide
    json-mode
    lsp-mode
    lsp-ui
    magit
    markdown-mode
    multiple-cursors
    nix-mode
    nyan-mode
    projectile
    rainbow-delimiters
    rust-mode
    sbt-mode
    smartparens
    smooth-scrolling
    spacemacs-theme
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
    myScalaMode
  ]
)
