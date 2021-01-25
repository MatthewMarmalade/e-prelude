#!/bin/sh
ghci -XDeriveGeneric -XDeriveAnyClass -XNoImplicitPrelude -interactive-print=print Test.hs
