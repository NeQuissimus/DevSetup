export ZSH=$(ls -d /nix/store/*oh-my-zsh-git* -t | grep -v '.*drv' | head -1)/share/oh-my-zsh/
ZSH_THEME="sunaku"
CASE_SENSITIVE="false"
export UPDATE_ZSH_DAYS=7
ENABLE_CORRECTION="true"
HIST_STAMPS="dd.mm.yyyy"
plugins=(git)
CODE_COLLAB="/home/nequi/bin/ccollab-cmdline"
export TERMINAL="xterm"
export PATH="${CODE_COLLAB}:/home/nequi/bin:/var/setuid-wrappers:/home/nequi/.nix-profile/bin:/home/nequi/.nix-profile/sbin:/home/nequi/.nix-profile/lib/kde4/libexec:/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/default/sbin:/nix/var/nix/profiles/default/lib/kde4/libexec:/run/current-system/sw/bin:/run/current-system/sw/sbin:/run/current-system/sw/lib/kde4/libexec"
source ${ZSH}/oh-my-zsh.sh
alias mvn='mvn -q'
alias nano='nano -i -E -w -c'
alias xclip='xclip -selection c'
alias lock='xtrlock-pam'
alias volume_up='amixer -q sset Master 5%+'
alias volume_down='amixer -q sset Master 5%-'
alias javarepl='java -jar /home/nequi/bin/javarepl.jar'
export JAVA_HOME="${$(readlink -e $(type -p java))%*/bin/java}"
setxkbmap -option ctrl:nocaps
[[ -z "${HOME}" ]] || rm -rf "${HOME}/Downloads/*"
