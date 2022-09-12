{ config, pkgs, ... }:
let
  flycheck-inline = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/flycheck/flycheck-inline/8e00b4c5951a9515a450a14aefe92e9f6ddcfbde/flycheck-inline.el";
    sha256 = "sha256-afK557er+ntZAqVeYvFYsNrV9xM5B7RPhX/LcbPV3Y4=";
  };
  sensible-defaults = pkgs.fetchurl {
    url =
      "https://raw.githubusercontent.com/hrs/sensible-defaults.el/d9001f0efc9382f8587357f2670fc4d8594858af/sensible-defaults.el";
    sha256 = "09l03619bchh6dh0fnanvmwp50pmnv4x8c8qqgyv4kmwd553ba9n";
  };
  sublima = pkgs.fetchurl {
    url =
      "https://raw.githubusercontent.com/Parveshdhull/sublima/624ade6b59853222bc595ab2a1017d7af516c7d8/sublima.el";
    sha256 = "sha256-ELXPjVo6DXxM7yxuM//OukxYXx1bMsjeT5NFBqLJeus=";
  };

  emacs-osx = (import (builtins.fetchTarball {
      url = https://github.com/NeQuissimus/emacs-osx/archive/main.tar.gz;
    }));
in {
  home = {
    file = {
      ".emacs.d/config.org".source = ./config.org;
      ".emacs.d/init.el".source = ./init.el;
      ".emacs.d/flycheck-inline.el".source = flycheck-inline;
      ".emacs.d/sensible-defaults.el".source = sensible-defaults;
      ".emacs.d/sublima.el".source = sublima;
      ".emacs.d/transient/values.el".source = ./values.el;
    };

    packages = with pkgs; [ hasklig ispell openjdk11_headless sqlite ];
  };

  programs.emacs = {
    extraPackages = epkgs:
      (with epkgs.melpaPackages; [
        all-the-icons
        auto-complete
        dockerfile-mode
        company
        dap-mode
        editorconfig
        flycheck
        forge
        go-mode
        golden-ratio
        groovy-mode
        haskell-mode
        hasklig-mode
        hl-todo
        htmlize
        indent-guide
        ivy
        json-mode
        lsp-metals
        lsp-mode
        lsp-treemacs
        lsp-ui
        magit
        markdown-mode
        material-theme
        multi-term
        multiple-cursors
        nix-mode
        nyan-mode
        org-roam
        ox-gfm
        posframe
        projectile
        rainbow-delimiters
        rust-mode
        sbt-mode
        scala-mode
        smartparens
        smooth-scrolling
        toc-org
        treemacs
        treemacs-magit
        treemacs-icons-dired
        treemacs-projectile
        use-package
        vterm
        with-editor
        yaml-mode
        yasnippet
      ]) ++ (with epkgs.elpaPackages; [ beacon ]);

    package = emacs-osx.emacsOsxNative;
  };

}
