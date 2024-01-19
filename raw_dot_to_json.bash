DOTFILE=$1
DOTDIR=$(dirname $DOTFILE)
BASENAME=${DOTFILE%.dot}

sed -n -e '1 i [' -e 's/^\([0-9]*\)\-\-\([0-9]*\);.*/  [\1, \2],/p' -e '$ s/,//' $1 | \
	sed -e '$ s/,$//' -e '$ a ]' | \
	jq '. | reduce .[] as [$i, $j] ([]; .[$i] += [$j] | .[$j] += [$i])' | \
	jq 'walk(if(type == "number") then tostring else . end)' > ${BASENAME}.json
