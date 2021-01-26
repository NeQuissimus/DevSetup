#!/usr/bin/env bash

set -eux

p="$(pwd)"

nix_update="/home/nequi/dev/nix-update/result/bin/nix-update"

function updateScript() {
  local package="${1}"
  yes '' | nix-shell maintainers/scripts/update.nix --argstr path "${package}" --argstr commit true
  nix-build -A "${package}.tests"
}

function update() {
  local package="${1}"
  shift
  "${nix_update}" "${package}" --commit --test $@
}

function updateNoTest() {
  local package="${1}"
  shift
  "${nix_update}" "${package}" --commit --build $@
}

function update_alsaFirmware() {
  latest="$(git -c 'versionsort.suffix=-' ls-remote --exit-code --refs --sort='version:refname' --tags https://github.com/alsa-project/alsa-firmware '*' | tail --lines=1 | cut --delimiter='/' --fields=3 | sed 's|^v||g')"
  updateNoTest "alsa-firmware" --version "${latest}"
}

function update_alsaLib() {
  latest="$(git -c 'versionsort.suffix=-' ls-remote --exit-code --refs --sort='version:refname' --tags https://github.com/alsa-project/alsa-lib '*' | tail --lines=1 | cut --delimiter='/' --fields=3 | sed 's|^v||g')"
  updateNoTest "alsaLib" --version "${latest}"
}

function update_alsaPlugins() {
  latest="$(git -c 'versionsort.suffix=-' ls-remote --exit-code --refs --sort='version:refname' --tags https://github.com/alsa-project/alsa-plugins '*' | tail --lines=1 | cut --delimiter='/' --fields=3 | sed 's|^v||g')"
  updateNoTest "alsaPlugins" --version "${latest}"
}

function update_alsaTools() {
  latest="$(git -c 'versionsort.suffix=-' ls-remote --exit-code --refs --sort='version:refname' --tags https://github.com/alsa-project/alsa-tools '*' | tail --lines=1 | cut --delimiter='/' --fields=3 | sed 's|^v||g')"
  updateNoTest "alsaTools" --version "${latest}"
}

function update_alsaUtils() {
  latest="$(git -c 'versionsort.suffix=-' ls-remote --exit-code --refs --sort='version:refname' --tags https://github.com/alsa-project/alsa-utils '*' | tail --lines=1 | cut --delimiter='/' --fields=3 | sed 's|^v||g')"
  updateNoTest "alsaUtils" --version "${latest}"
}

function update_sudo() {
  latest="$(git -c 'versionsort.suffix=-' ls-remote --exit-code --refs --sort='version:refname' --tags https://github.com/sudo-project/sudo '*' | tail --lines=1 | cut --delimiter='/' --fields=3 | sed 's|^SUDO_||g' | tr '_' '.')"
  update "sudo" --version "${latest}"
}

function update_bind() {
  majorMinor="$(nix-instantiate --eval -E "with import ./. {}; lib.versions.majorMinor (lib.getVersion bind)" | tr -d '"' | tr '.' '_')"
  latest="$(git -c 'versionsort.suffix=-' ls-remote --exit-code --refs --sort='version:refname' --tags https://gitlab.isc.org/isc-projects/bind9.git v${majorMinor}'*' | tail --lines=1 | cut --delimiter='/' --fields=3 | sed 's|^v||g' | tr '_' '.')"
  update "bind" --version "${latest}"
}

cd "${NIXPKGS_CHECKOUT}"
git checkout -- .
git clean -xdf
git checkout master
git reset --hard origin/master
git pull

update "ammonite"
update "bat"
updateNoTest "coursier"
updateNoTest "gitAndTools.hub"
updateNoTest "httpstat"
updateNoTest "python3Packages.botocore" && updateNoTest "python3Packages.boto3" && update "awscli"
update "lsd"
update "sbt"
update "shadow"
update "yq"

# https://github.com/Mic92/nix-update/issues/11
updateScript "oh-my-zsh"
updateScript "sbt-extras"

# https://github.com/Mic92/nix-update/issues/29
update "jq" -vr 'jq-(.*)'
update "ripgrep" -vr '^([0-9].*)'

updateScript "jenkins"
updateScript "minecraft"
updateScript "minecraft-server"
updateScript "nano"
updateScript "scala_2_10"
updateScript "scala_2_11"
updateScript "scala_2_12"
updateScript "scala_2_13"
updateScript "xterm"

# Custom versions / weird tags
#update_alsaFirmware
#update_alsaLib
#update_alsaPlugins
#update_alsaTools
#update_alsaUtils
update_bind
update_sudo

# Kernel is its own beast...
./pkgs/os-specific/linux/kernel/update.sh
nix-build -A linux_4_4.configfile \
          -A linux_4_9.configfile \
          -A linux_4_14.configfile \
          -A linux_4_19.configfile \
          -A linux_5_4.configfile \
          -A linux_5_9.configfile \
          -A linux_5_10.configfile \
          -A linux_latest.configfile \
          -A linux_hardened.configfile \
          -A linux_latest_hardened.configfile \
          -A linux_testing.configfile
nix-build ./nixos/release.nix -A tests.kernel-latest.x86_64-linux \
          -A tests.kernel-testing.x86_64-linux \
          -A tests.kernel-lts.x86_64-linux \
          -A tests.latestKernel.login.x86_64-linux \
          -A tests.latestKernel.hardened.x86_64-linux

cd "${p}"
