NEWNAME=${1%.dot}.dot
mv $1 $NEWNAME
sed -i "s/^\([0-9]*\)\-\-\([0-9]*\);/\1--\2 \[class=\"_\1_ _\2_\"\];/" $NEWNAME
sed -i "1 r $2" $NEWNAME
