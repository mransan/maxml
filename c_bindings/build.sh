source ../ocaml_env.sh

# clean up
#
rm -f *.o
rm -f *.cm*

# compile C binding
#
$GCC -o c_binding_impl.o -c $OCAML_C_INCLUDE c_binding_impl.c

# compile ml bindings
#
FILES="
c_binding
main
"

CMXS=""

for file in $FILES
do
  $OCAMLOPT -c "$file.ml"
  CMXS="$file.cmx $CMXS"
done

$OCAMLOPT -o c_binding.tsk $CMXS c_binding_impl.o 
