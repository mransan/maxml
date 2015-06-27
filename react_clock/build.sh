source ../ocaml_env.sh

# clean up
#
rm -f *.o
rm -f *.cm*
rm -f *.tsk

# compile ml bindings
#
FILES="
main
"
CMXS=""

for file in $FILES
do
  $OCAMLFIND ocamlopt -package react -package unix -c $file.ml
  CMXS="$CMXS $file.cmx"
done

#$OCAMLOPT --help 

$OCAMLFIND ocamlopt -package react -package unix -linkpkg -o react_clock.tsk $CMXS


