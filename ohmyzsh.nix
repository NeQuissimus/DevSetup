{
  programs.zsh.interactiveShellInit = ''
if [ ! -e "$HOME/.oh-my-zsh" -a ! $(id -u) = "0" ]; then
  cd $HOME
  sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
  cat << EOF > $HOME/.zshrc
  export ZSH=$HOME/.oh-my-zsh
  ZSH_THEME="sunaku"
  CASE_SENSITIVE="false"
  export UPDATE_ZSH_DAYS=7
  ENABLE_CORRECTION="true"
  HIST_STAMPS="dd.mm.yyyy"
  plugins=(git)
  export PATH="$HOME/bin:/var/setuid-wrappers:$HOME/.nix-profile/bin:$HOME/.nix-profile/sbin:$HOME/.nix-profile/lib/kde4/libexec:/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/default/sbin:/nix/var/nix/profiles/default/lib/kde4/libexec:/run/current-system/sw/bin:/run/current-system/sw/sbin:/run/current-system/sw/lib/kde4/libexec"
  source $HOME/.oh-my-zsh/oh-my-zsh.sh
  alias mvn='mvn -q'
  alias xclip='xclip -selection c'
  alias lock='xtrlock-pam'
EOF
fi
  '';
}
