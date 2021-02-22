#!/bin/sh
# A script for removing files corresponding to EPrelude from your system.

if [ `whoami` != root ]; then
  echo This script needs to be run as sudo in order to uninstall correctly.
  exit
fi


# EPrelude information is located in /usr/local/share/eprelude and /usr/local/bin/ehci
#echo Deleting /usr/local/share/eprelude/Text/PrettyPrint/GenericPretty.hs...
rm -f -i /usr/local/share/eprelude/Text/PrettyPrint/GenericPretty.hs
#echo Deleting /usr/local/share/eprelude/Text/PrettyPrint...
rm -d -f -i /usr/local/share/eprelude/Text/PrettyPrint
#echo Deleting /usr/local/share/eprelude/Text...
rm -d -f -i /usr/local/share/eprelude/Text
#echo Deleting /usr/local/share/eprelude/.ghci
rm -f -i /usr/local/share/eprelude/.ghci
#echo Deleting /usr/local/share/eprelude/EPrelude.hs
rm -f -i /usr/local/share/eprelude/EPrelude.hs
#echo Deleting /usr/local/share/eprelude...
rm -d -f -i /usr/local/share/eprelude
#echo Deleting /usr/local/bin/edhci
rm -f -i /usr/local/bin/edhci
echo Uninstall Complete.
