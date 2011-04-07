#!/bin/bash
#ghc -package llvm -package Decimal -package readline -package parsec -package utf8-string -o cleo cleo.hs
ghc -package parsec -package utf8-string -o cleo cleo.hs
