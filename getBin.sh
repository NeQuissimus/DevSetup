#!/usr/bin/env bash

mkdir -p ./bin
rm -rf ./bin/*

curl https://raw.githubusercontent.com/b4b4r07/httpstat/master/httpstat.sh -o ./bin/httpstat
sed -i 's/\/bin\/bash/\/usr\/bin\/env bash/g' ./bin/httpstat

chmod +x ./bin/*
