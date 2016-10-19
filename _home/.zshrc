export ZSH="${HOME}/.nix-profile/share/oh-my-zsh"

ZSH_THEME="sunaku"
CASE_SENSITIVE="false"
export UPDATE_ZSH_DAYS=7
ENABLE_CORRECTION="true"
HIST_STAMPS="dd.mm.yyyy"
plugins=(git)
#export JAVA_OPTS='-XX:+UseG1GC -Xms3g -Xmx3g -XX:-UseBiasedLocking -XX:+UseCompressedOops -XX:MetaspaceSize=512M -Xss2m -Djava.security.egd=file:/dev/./urandom -XX:ReservedCodeCacheSize=256m -XX:+PrintGCDetails -XX:+PrintGCTimeStamps'
export TERMINAL="xterm"
export PATH="$/home/nequi/bin:/var/setuid-wrappers:/home/nequi/.nix-profile/bin:/home/nequi/.nix-profile/sbin:/home/nequi/.nix-profile/lib/kde4/libexec:/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/default/sbin:/nix/var/nix/profiles/default/lib/kde4/libexec:/run/current-system/sw/bin:/run/current-system/sw/sbin:/run/current-system/sw/lib/kde4/libexec"
source ${ZSH}/oh-my-zsh.sh
alias mvn='mvn -q'
alias nano='nano -E -w -c'
alias xclip='xclip -selection c'
alias lock='xtrlock-pam'
alias volume_up='amixer -q sset Master 5%+'
alias volume_down='amixer -q sset Master 5%-'
alias javarepl='java -jar /home/nequi/bin/javarepl.jar'
alias sbt='sbt -mem 4096'
alias docker_clean='docker kill $(docker ps -q); docker rm $(docker ps -a -q); docker rmi -f $(docker images -q)'
alias docker_compose='curl -sL https://github.com/docker/compose/releases/download/1.8.0/run.sh | sh'
export JAVA_HOME="${$(readlink -e $(type -p java))%*/bin/java}"
setxkbmap -option ctrl:nocaps
[[ -z "${HOME}" ]] || rm -rf "${HOME}/Downloads/*"

nix-search(){ echo "Searching..."; nix-env -qaP --description '*' | grep -i "$1"; }
