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

  lsp-mode = (pkgs.emacsPackagesNgGen myEmacs).melpaBuild {
    pname = "lsp-mode";
    version = "20190606.1958";
    src = fetchFromGitHub {
      owner = "emacs-lsp";
      repo = "lsp-mode";
      rev = "34b769cebde2b7ba3f11230636a1fcd808551323";
      sha256 = "1cxglnk2hpkfv7yhxfm4xyd3gfjw0x8ysab3v3fazprnsiz7xlxr";
    };
    packageRequires = with (pkgs.emacsPackagesNgGen myEmacs); [
      dash
      dash-functional
      emacs
      f
      ht
      markdown-mode
      spinner
    ];
    recipe = pkgs.writeText "recipe" ''
      (lsp-mode :repo "emacs-lsp/lsp-mode" :fetcher github)
    '';
  };

  lsp-ui = (pkgs.emacsPackagesNgGen myEmacs).melpaBuild {
    pname = "lsp-ui";
    version = "20190523.1521";
    src = fetchGit {
      url = "https://github.com/emacs-lsp/lsp-ui.git";
      rev = "3ccc3e3386732c3ee22c151e6b5215a0e4c99173";
    };
    packageRequires = with (pkgs.emacsPackagesNgGen myEmacs); [
      dash
      dash-functional
      emacs
      lsp-mode
      markdown-mode
    ];
    recipe = pkgs.writeText "recipe" ''
      (lsp-ui :repo "emacs-lsp/lsp-ui"
              :fetcher github
              :files (:defaults "lsp-ui-doc.html"))
    '';
    };

  company-lsp = (pkgs.emacsPackagesNgGen myEmacs).melpaBuild {
    pname = "company-lsp";
    version = "20190525.207";
    src = fetchGit {
      url = "https://github.com/tigersoldier/company-lsp.git";
      rev = "cd1a41583f2d71baef44604a14ea71f49b280bf0";
    };
    packageRequires = with (pkgs.emacsPackagesNgGen myEmacs); [
      company
      dash
      emacs
      lsp-mode
      s
    ];
    recipe = pkgs.writeText "recipe" ''
      (company-lsp :repo "tigersoldier/company-lsp" :fetcher github)
    '';
  };

  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; []) ++ (with epkgs.melpaPackages; [
    all-the-icons
    auto-complete
    dockerfile-mode
    company
    flycheck
    forge
    haskell-mode
    hl-todo
    indent-guide
    json-mode
    magit
    markdown-mode
    multiple-cursors
    neotree
    nix-mode
    nyan-mode
    projectile
    rainbow-delimiters
    sbt-mode
    smartparens
    smooth-scrolling
    spacemacs-theme
    use-package
    yaml-mode
    yasnippet
  ]) ++ (with epkgs.elpaPackages; [
    beacon
  ]) ++ [
    company-lsp
    lsp-mode
    lsp-ui
    myScalaMode
  ]
)
