  export ZSH=/home/nequi/.oh-my-zsh
  ZSH_THEME="sunaku"
  CASE_SENSITIVE="false"
  export UPDATE_ZSH_DAYS=7
  ENABLE_CORRECTION="true"
  HIST_STAMPS="dd.mm.yyyy"
  plugins=(git)
  export PATH="/home/nequi/bin:/var/setuid-wrappers:/home/nequi/.nix-profile/bin:/home/nequi/.nix-profile/sbin:/home/nequi/.nix-profile/lib/kde4/libexec:/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/default/sbin:/nix/var/nix/profiles/default/lib/kde4/libexec:/run/current-system/sw/bin:/run/current-system/sw/sbin:/run/current-system/sw/lib/kde4/libexec"
  source /home/nequi/.oh-my-zsh/oh-my-zsh.sh
  alias mvn='mvn -q'
  alias nano='nano -w'
  alias xclip='xclip -selection c'
  alias lock='xtrlock-pam'
  alias volume_up='amixer -q sset Master 5%+'
  alias volume_down='amixer -q sset Master 5%-'
  export JAVA_HOME="${$(readlink -e $(type -p java))%*/bin/java}"
  setxkbmap -option ctrl:nocaps
