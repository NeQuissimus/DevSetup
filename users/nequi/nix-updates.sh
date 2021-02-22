#!/usr/bin/env bash

set -eux

p="$(pwd)"

nix_update="${HOME}/dev/nix-update/result/bin/nix-update"

BASE_BRANCH="master"
BASE_REMOTE="origin"
PUSH_REMOTE="downstream"

BRANCHES=()

function needCommand() {
  type -p "${1}"
}

needCommand "cat"
needCommand "fold"
needCommand "git"
needCommand "head"
needCommand "nix-build"
needCommand "nix-instantiate"
needCommand "nix-shell"
needCommand "tr"
needCommand "yes"

function hasTests() {
  local package="${1}"
  nix-instantiate --eval -E "with import ./. {}; ${package}.passthru.tests" >/dev/null 2>&1
}

function createBranch() {
  local package="${1}"

  cd "${NIXPKGS_CHECKOUT}"
  git checkout -- .
  git clean -xdf
  git checkout "${BASE_BRANCH}"
  git reset --hard "${BASE_REMOTE}/${BASE_BRANCH}"
  git pull

  git checkout -b "${package}"
}

function pushBranch() {
  local branch="$(git branch --show-current)"

  if [ "$(git cherry master | grep '\+')" ]; then
    git push -f "${PUSH_REMOTE}" "${branch}"
    BRANCHES+=("${branch}")
  fi

  git checkout "${BASE_BRANCH}"
  git branch -D "${branch}"
}

function updateScript() {
  local package="${1}"
  yes '' | nix-shell maintainers/scripts/update.nix --argstr path "${package}" --argstr commit true
  hasTests "${package}" && nix-build -A "${package}.tests"
  nix-shell -p nixpkgs-review --run "nixpkgs-review wip"
}

function update() {
  local package="${1}"
  (hasTests "${package}" && updateWithTest $@) || updateNoTest $@
}

function updateWithTest() {
  local package="${1}"
  shift
  "${nix_update}" "${package}" --commit --test --review --format $@
}

function updateNoTest() {
  local package="${1}"
  shift
  "${nix_update}" "${package}" --commit --build --review --format $@
}

function updateWithBranch() {
  createBranch "${!#}"

  for package in "${@}"; do
    update "${package}"
  done

  pushBranch
}

function update_sudo() {
  latest="$(git -c 'versionsort.suffix=-' ls-remote --exit-code --refs --sort='version:refname' --tags https://github.com/sudo-project/sudo '*' | tail --lines=1 | cut --delimiter='/' --fields=3 | sed 's|^SUDO_||g' | tr '_' '.')"
  update "sudo" --version "${latest}"
}


updateWithBranch "ammonite"
updateWithBranch "bat"
updateWithBranch "coursier"
updateWithBranch "gitAndTools.hub"
updateWithBranch "httpstat"
updateWithBranch "python3Packages.botocore" "python3Packages.boto3" "awscli"
updateWithBranch "python3Packages.protobuf3-to-dict" "python3Packages.smdebug-rulesconfig" "python3Packages.sagemaker"
updateWithBranch "lsd"
updateWithBranch "sbt"
updateWithBranch "shadow"
updateWithBranch "yq"

createBranch "jq" && update "jq" -vr 'jq-(.*)' && pushBranch
createBranch "ripgrep" && update "ripgrep" -vr '^([0-9].*)' && pushBranch

# https://github.com/Mic92/nix-update/issues/11
createBranch "omz" && updateScript "oh-my-zsh" && pushBranch
createBranch "sbtextras" && updateScript "sbt-extras" && pushBranch

createBranch "jenkins" && updateScript "jenkins" && pushBranch
createBranch "minecraft" && updateScript "minecraft" && pushBranch
createBranch "minecraft-server" && updateScript "minecraft-server" && pushBranch
createBranch "nano" && updateScript "nano" && pushBranch
createBranch "scala210" && updateScript "scala_2_10" && pushBranch
createBranch "scala211" && updateScript "scala_2_11" && pushBranch
createBranch "scala212" && updateScript "scala_2_12" && pushBranch
createBranch "scala213" && updateScript "scala_2_13" && pushBranch
createBranch "xterm" && updateScript "xterm" && pushBranch

# Custom versions / weird tags
createBranch "sudo" && update_sudo && pushBranch

# Kernel is its own beast...
createBranch "kernels"
./pkgs/os-specific/linux/kernel/update.sh
nix-build -A linux_4_4.configfile \
          -A linux_4_9.configfile \
          -A linux_4_14.configfile \
          -A linux_4_19.configfile \
          -A linux_5_4.configfile \
          -A linux_5_10.configfile \
          -A linux_5_11.configfile \
          -A linux_latest.configfile \
          -A linux_hardened.configfile \
          -A linux_latest_hardened.configfile \
          -A linux_testing.configfile
nix-build ./nixos/release.nix -A tests.kernel-latest.x86_64-linux \
          -A tests.kernel-testing.x86_64-linux \
          -A tests.kernel-lts.x86_64-linux \
          -A tests.latestKernel.login.x86_64-linux \
          -A tests.latestKernel.hardened.x86_64-linux \
          -A tests.kernel-latest-ath-user-regd.x86_64-linux
pushBranch

echo "${BRANCHES[@]}"

cd "${p}"
