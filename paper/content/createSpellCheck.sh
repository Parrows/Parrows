ls -1 | grep '\.tex$' | while read -r a; do echo "aspell -c -t" $a; done > spellcheck.sh