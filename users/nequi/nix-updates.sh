#!/usr/bin/env nix-shell
#!nix-shell -i bash -p nix-update git

set -eux

p="$(pwd)"

function updateScript() {
  local package="${1}"
  yes '' | nix-shell maintainers/scripts/update.nix --argstr path "${package}" --argstr commit true
  nix-build -A "${package}.tests"
}

function update() {
  local package="${1}"
  nix-update "${package}" --commit --test
}

function updateNoTest() {
  local package="${1}"
  nix-update "${package}" --commit
}

cd "${NIXPKGS_CHECKOUT}"
git checkout -- .
git clean -xdf
git checkout master
git reset --hard origin/master
git pull

update "ammonite"
updateNoTest "coursier"
updateNoTest "httpstat"
updateNoTest "python3Packages.botocore" && updateNoTest "python3Packages.boto3" && update "awscli"
update "sbt"

# https://github.com/Mic92/nix-update/issues/11
updateScript "oh-my-zsh"
updateScript "sbt-extras"

updateScript "jenkins"
updateScript "nano"
updateScript "scala_2_10"
updateScript "scala_2_11"
updateScript "scala_2_12"
updateScript "scala_2_13"
updateScript "xterm"

# Needs test, reference to update script
./pkgs/games/minecraft/update.sh
nix-build -A minecraft

# Kernel is its own beast...
./pkgs/os-specific/linux/kernel/update.sh
nix-build -A linux_4_4.configfile \
          -A linux_4_9.configfile \
          -A linux_4_14.configfile \
          -A linux_4_19.configfile \
          -A linux_5_4.configfile \
          -A linux_5_8.configfile \
          -A linux_5_9.configfile \
          -A linux_latest.configfile \
          -A linux_hardened.configfile \
          -A linux_latest_hardened.configfile \
          -A linux_testing.configfile
nix-build ./nixos/release.nix -A tests.kernel-latest.x86_64-linux \
          -A tests.kernel-testing.x86_64-linux \
          -A tests.kernel-lts.x86_64-linux \
          -A tests.latestKernel.login.x86_64-linux

cd "${p}"
