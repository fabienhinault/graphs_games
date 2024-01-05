#!/bin/bash

DOTFILE=$1
DOTDIR=$(dirname $DOTFILE)
echo $DOTDIR
BASENAME=${DOTFILE%.dot}
JSPATH=$(realpath --relative-to=$DOTDIR ./edinburgh.js)
ROOTPATH=$(realpath --relative-to=$DOTDIR ./)


dot -Tsvg $DOTFILE -o ${BASENAME}.svg
sed "/<!-- insert svg content just above this line -->/e cat ${BASENAME}.svg" ./edinburgh.html | \
sed "/\/\/ insert json content just above this line/e  cat ${BASENAME}.json ; echo" | \
sed "/<!-- insert script markup for edinburgh.js just above this line -->/i <script src=\"$JSPATH\"></script>" | \
sed -r "s#src=\"([a-zA-Z0-9_]*)\\.svg\"#src=\"$ROOTPATH/\\1\\.svg\"#" | \
sed "s#href=\"edinburgh.css\"#href=\"$ROOTPATH/edinburgh.css\"#" > \
${BASENAME}.html
SOLO=$(echo $BASENAME | sed "s#$DOTDIR\/#$DOTDIR\/solo_#")
sed 's/edinburgh.js"/edinburgh_solo.js"/' ${BASENAME}.html > $SOLO.html



