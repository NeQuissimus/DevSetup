{ config, pkgs, ... }:
let
  sensible-defaults = pkgs.fetchurl {
    url =
      "https://raw.githubusercontent.com/hrs/sensible-defaults.el/d9001f0efc9382f8587357f2670fc4d8594858af/sensible-defaults.el";
    sha256 = "09l03619bchh6dh0fnanvmwp50pmnv4x8c8qqgyv4kmwd553ba9n";
  };
  sublima = pkgs.fetchurl {
    url =
      "https://raw.githubusercontent.com/Parveshdhull/sublima/5c7acd0eb8e5b6dad5f9a662e7d6f4a927d5683c/sublima.el";
    sha256 = "1gv9ngbffp0069a0yrvz65w0ncbjm10bkmib0dw74k282pqsxc5n";
  };
in {
  home = {
    file = {
      ".emacs.d/config.org".source = ./config.org;
      ".emacs.d/init.el".source = ./init.el;
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
        apropospriate-theme
        auto-complete
        dockerfile-mode
        company
        editorconfig
        flycheck
        forge
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
        lsp-ui
        magit
        markdown-mode
        multi-term
        multiple-cursors
        nix-mode
        nyan-mode
        org-roam
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
        vterm
        yaml-mode
        yasnippet
      ]) ++ (with epkgs.elpaPackages; [ beacon ]);
  };

}
