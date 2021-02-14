#!/bin/sh
# A script for removing files corresponding to EPrelude from your system.

if [ `whoami` != root ]; then
  echo This script needs to be run as sudo in order to uninstall correctly.
  exit
fi


# EPrelude information is located in /usr/local/var/eprelude and /usr/local/bin/ehci
echo Deleting /usr/local/var/eprelude/.ghci...
rm /usr/local/var/eprelude/.ghci
echo Deleting /usr/local/var/eprelude/EPrelude.hs...
rm /usr/local/var/eprelude/EPrelude.hs
echo Deleting /usr/local/var/eprelude/Text/PrettyPrint/GenericPretty.hs...
rm /usr/local/var/eprelude/Text/PrettyPrint/GenericPretty.hs
echo Deleting /usr/local/var/eprelude/Text/PrettyPrint...
rmdir /usr/local/var/eprelude/Text/PrettyPrint
echo Deleting /usr/local/var/eprelude/Text...
rmdir /usr/local/var/eprelude/Text
echo Deleting /usr/local/var/eprelude...
rmdir /usr/local/var/eprelude
echo Deleting /usr/local/bin/ehci
rm /usr/local/bin/ehci
echo Uninstall Complete.
