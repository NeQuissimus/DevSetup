export ZSH="${HOME}/.nix-profile/share/oh-my-zsh"

ZSH_THEME="avit"
CASE_SENSITIVE="false"
ENABLE_CORRECTION="true"
HIST_STAMPS="dd.mm.yyyy"
plugins=(git)

export TERMINAL="xterm"
export PATH="$/home/nequi/bin:/var/setuid-wrappers:/home/nequi/.nix-profile/bin:/home/nequi/.nix-profile/sbin:/home/nequi/.nix-profile/lib/kde4/libexec:/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/default/sbin:/nix/var/nix/profiles/default/lib/kde4/libexec:/run/current-system/sw/bin:/run/current-system/sw/sbin:/run/current-system/sw/lib/kde4/libexec"
export JAVA_HOME="${$(readlink -e $(type -p java))%*/bin/java}"

source ${ZSH}/oh-my-zsh.sh

alias mvn='mvn -q'
alias nano='nano -E -w -c'
alias volume='awk -F"[][]" "/dB/ { print $2 }" <(amixer sget Master)'
alias volume_up='amixer -q sset Master 5%+; volume'
alias volume_down='amixer -q sset Master 5%-; volume'
alias javarepl='java -jar /home/nequi/bin/javarepl.jar'
alias sbt='sbt -mem 4096'
alias docker_clean='docker kill $(docker ps -q); docker rm $(docker ps -a -q); docker rmi -f $(docker images -q)'
alias rsync='rsync -azvvP'

setxkbmap -option ctrl:nocaps
[[ -z "${HOME}" ]] || [[ -e "${HOME}/Downloads" ]] && rm -rf "${HOME}/Downloads" && mkdir "${HOME}/Downloads"

function nix-search() { echo "Searching..."; nix-env -qaP --description '*' | grep -i "$1"; }
function nix-update() { echo "Updating user environment... "; nix-channel --update; nix-env -i $(nix-env -q | grep -v 'user-misc'); }
