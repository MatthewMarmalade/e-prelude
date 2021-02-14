#!/bin/sh
# A script for installing files corresponding to EPrelude to your system.

if [ `whoami` != root ]; then
  echo This script needs to be run as sudo in order to install correctly.
  exit
fi

# EPrelude infocpation is located in /usr/local/var/eprelude
echo Creating /usr/local/var/eprelude
cp -r eprelude /usr/local/var

echo Creating /usr/local/bin/ehci
cp -r ehc/ehci /usr/local/bin

echo Install Complete.
