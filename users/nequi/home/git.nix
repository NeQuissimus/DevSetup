{ config, pkgs, ... }: {
  programs.git = {
    aliases = {
      clear = "clean -dfx";
      lg =
        "log --all --decorate --color --graph --pretty=format:'%Cred%h%Creset %Cgreen(%cr)%Creset - %s %C(bold blue)<%an>[%G?]%Creset%C(auto)%d%Creset' --abbrev-commit";
      ll = ''
        log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate --numstat'';
      undo = "reset HEAD~1 --mixed";
    };

    delta = {
      enable = true;

      options = {
        decorations = {
          commit-decoration-style = "bold yellow box ul";
          file-style = "bold yellow ul";
          file-decoration-style = "none";
        };

        features = "line-numbers";
        inspect-raw-lines = "false";
        whitespace-error-style = "22 reverse";
      };
    };

    extraConfig = {
      apply.whitespace = "fix";
      branch.autosetuprebase = "always";
      core.whitespace = "fix,-indent-with-non-tab,trailing-space,cr-at-eol";
      diff.colorMoved = "default";
      init.defaultBranch = "main";
      merge.renamelimit = "4096";
      pull.rebase = "true";
      push.default = "upstream";
      submodule.recurse = "true";
      tag.sort = "version:refname";
    };

    ignores = [
      ".metals"
      ".bloop"
      ".scalafix.conf"
      ".scalafmt.conf"
      # https://www.toptal.com/developers/gitignore/api/emacs
      "*~"
      "#*#"
      "/.emacs.desktop"
      "/.emacs.desktop.lock"
      "*.elc"
      "auto-save-list"
      "tramp"
      ".#*"
      ".org-id-locations"
      "*_archive"
      "*_flymake.*"
      "/eshell/history"
      "/eshell/lastdir"
      "/elpa/"
      "*.rel"
      "/auto/"
      ".cask/"
      "dist/"
      "flycheck_*.el"
      "/server/"
      ".projectile"
      ".dir-locals.el"
      "/network-security.data"
    ];

    signing = {
      key = "ACD70987F33D77B8A956E89FA8AECBD02786E3F0";
      signByDefault = true;
    };

    userEmail = "tim@nequissimus.com";
    userName = "Tim Steinbach";
  };

}
