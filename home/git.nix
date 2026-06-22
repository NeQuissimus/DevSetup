{ pkgs, ... }:
let
  email = "tim@nequissimus.com";
  gpgKey = "";
  name = "Tim Steinbach";
in {
  programs.git = {
    enable = true;

    ignores = [
      ".#*"
      ".bloop"
      ".bsp"
      ".cask/"
      ".claude"
      ".classpath"
      ".dir-locals.el"
      ".idea"
      ".intellij"
      ".metals"
      ".org-id-locations"
      ".project"
      ".projectile"
      ".scalafix.conf"
      ".scalafmt.conf"
      ".settings"
      "*_archive"
      "*_flymake.*"
      "*.elc"
      "*.rel"
      "*~"
      "/.emacs.desktop.lock"
      "/.emacs.desktop"
      "/auto/"
      "/bin"
      "/elpa/"
      "/eshell/history"
      "/eshell/lastdir"
      "/network-security.data"
      "/result"
      "/server/"
      "#*#"
      "auto-save-list"
      "dist/"
      "flycheck_*.el"
      "gradle-wrapper.properties"
      "tramp"
    ];

    maintenance.enable = true;

    settings = {
      advice.detachedHead = "false";

      alias = {
        bclean =
          "!(git for-each-ref --format '%(refname:short)' refs/heads | grep -v 'master\\|main' | xargs git branch -D)";
        c = "!(${pkgs.gitmoji-cli}/bin/gitmoji -c)";
        clear = "clean -dfx";
        fpush = "push --force-with-lease";
        lg =
          "log --all --decorate --color --graph --pretty=format:'%Cred%h%Creset %Cgreen(%cr)%Creset - %s %C(bold blue)<%an>%Creset%C(auto)%d%Creset' --abbrev-commit";
        ll = ''
          log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate --numstat'';
        undo = "reset HEAD~1 --mixed";
        worktree-cleanup = ''
          !f() {
            main="''${1:-main}"
            echo "Fetching origin/$main..."
            git fetch origin "$main" 2>/dev/null
            found=0
            git worktree list --porcelain | while IFS= read -r line; do
              case "$line" in
                "worktree "*) wt="''${line#worktree }";;
                "branch refs/heads/"*)
                  branch="''${line#branch refs/heads/}"
                  if [ "$branch" != "$main" ] && git merge-base --is-ancestor "$branch" "origin/$main" 2>/dev/null; then
                    echo "Removing worktree: $wt (branch: $branch)"
                    git worktree remove "$wt" && git branch -d "$branch"
                    found=1
                  fi;;
              esac
            done
            git worktree prune
            echo "Done."
          }; f
        '';
      };

      apply.whitespace = "fix";
      branch.autosetuprebase = "always";
      core.commitGraph = "true";
      core.editor = "nano";
      core.whitespace = "fix,-indent-with-non-tab,trailing-space,cr-at-eol";
      credential.helper = "osxkeychain";
      diff.algorithm = "histogram";
      diff.colorMoved = "default";
      gc.writeCommitGraph = "true";
      help.autocorrect = "5";
      init.defaultBranch = "main";

      merge.conflictStyle = "zdiff3";
      merge.renamelimit = "4096";
      pull.ff = "only";
      pull.rebase = "true";
      push.autoSetupRemote = "true";
      push.default = "upstream";
      rebase.updateRefs = "true";
      submodule.recurse = "true";
      tag.sort = "version:refname";

      user = {
        email = email;
        name = name;
      };
    };

    signing = {
      key = gpgKey;
      signByDefault = true;
    };
  };
}