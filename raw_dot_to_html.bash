./raw_dot_to_json.bash $1
./enrich_dot.bash $1 $2
./dot_to_html.bash $1.dot 
