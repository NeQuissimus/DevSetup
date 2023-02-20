
{ config, pkgs, ... }: {
  programs.git = {
    aliases = {
      bclean = "!(git for-each-ref --format '%(refname:short)' refs/heads | grep -v 'master\\|main' | xargs git branch -D)";
      clear = "clean -dfx";
      lg =
        "log --all --decorate --color --graph --pretty=format:'%Cred%h%Creset %Cgreen(%cr)%Creset - %s %C(bold blue)<%an>%Creset%C(auto)%d%Creset' --abbrev-commit";
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
      advice.detachedHead = "false";
      apply.whitespace = "fix";
      branch.autosetuprebase = "always";
      core.commitGraph = "true";
      core.editor = "nano";
      core.whitespace = "fix,-indent-with-non-tab,trailing-space,cr-at-eol";
      diff.algorithm = "patience";
      diff.colorMoved = "default";
      gc.writeCommitGraph = "true";
      help.autocorrect = "5";
      init.defaultBranch = "main";
      merge.renamelimit = "4096";
      pull.rebase = "true";
      push.default = "upstream";
      submodule.recurse = "true";
      tag.sort = "version:refname";
    };

    ignores = [
      ".bsp"
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

#    signing.signByDefault = true;

    userName = "Tim Steinbach";
  };

}
