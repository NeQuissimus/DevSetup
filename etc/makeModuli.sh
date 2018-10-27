#!/usr/bin/env bash

dir=$(dirname $0)

ssh-keygen -G /tmp/moduli-2048.candidates -b 2048
ssh-keygen -T /tmp/moduli-2048 -f /tmp/moduli-2048.candidates
cp /tmp/moduli-2048 "${dir}"/moduli
