#!/bin/bash

DOTFILE=$1
DOTDIR=$(dirname $DOTFILE)
echo $DOTDIR
BASENAME=${DOTFILE%.dot}
JSPATH=$(realpath --relative-to=$DOTDIR ./edinburgh.js)

dot -Tsvg $DOTFILE -o ${BASENAME}.svg
sed "/<!-- insert svg content just above this line -->/e cat ${BASENAME}.svg" ./edinburgh.html | \
sed "/\/\/ insert json content just above this line/e  cat ${BASENAME}.json ; echo" | \
sed "/<!-- insert script markup for edinburgh.js just above this line -->/i <script src=\"$JSPATH\"></script>" > \
${BASENAME}.html
SOLO=$(echo $BASENAME | sed "s#$DOTDIR\/#$DOTDIR\/solo_#")
sed 's/edinburgh.js"/edinburgh_solo.js"/' ${BASENAME}.html > $SOLO.html



