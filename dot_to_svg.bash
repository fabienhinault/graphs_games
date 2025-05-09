#!/bin/bash

DOTDIR=$1
BASENAME=$(basename $DOTDIR)
DOTFILE=$DOTDIR/$BASENAME.dot

dot -Tsvg $DOTFILE -o ${DOTDIR}/$BASENAME.svg



