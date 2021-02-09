{ config, pkgs, ... }: {
  programs.emacs = {
    extraPackages = epkgs:
      (with epkgs.melpaPackages; [
        all-the-icons
        apropospriate-theme
        auto-complete
        company-lsp
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
        vterm
        yaml-mode
        yasnippet
      ]) ++ (with epkgs.elpaPackages; [ beacon ]);
  };

}
