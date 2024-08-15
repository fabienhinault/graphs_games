#!/bin/bash

DOTDIR=$1
DOTFILE=$DOTDIR/_.dot

dot -Tsvg $DOTFILE -o ${DOTDIR}/_.svg



