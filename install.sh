#!/bin/sh
# A script for installing files corresponding to EPrelude to your system.

if [ `whoami` != root ]; then
  echo This script needs to be run as sudo in order to install correctly.
  exit
fi

# EPrelude information is located in /usr/local/share/eprelude
[ -d /usr/local/share ] || mkdir -p /usr/local/share

#echo Creating /usr/local/share/eprelude
cp -r -p -i eprelude /usr/local/share

#echo Creating /usr/local/bin/ehci
cp -r -p -i ehc/ehci /usr/local/bin

echo Install Complete.
