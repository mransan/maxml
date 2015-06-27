source ../ocaml_env.sh

# clean up
#
rm -f *.o
rm -f *.cm*
rm -f *.tsk

# compile C binding
#
$GCC -o rt_c_binding.o \
    $OCAML_C_INCLUDE \
    -m64 \
    -D_POSIX_PTHREAD_SEMANTICS \
    -c rt_c_binding.c

# compile ml bindings
#
FILES="
rt_ml_binding
main
"

CMXS=""

for file in $FILES
do
  $OCAMLFIND ocamlopt -package ctypes -c $file.ml
  CMXS="$CMXS $file.cmx"
done

#$OCAMLOPT --help 

$OCAMLFIND ocamlopt -package ctypes -package ctypes.foreign -linkpkg -o rt.tsk $CMXS \
    rt_c_binding.o \
    -cc $GCC \
    -cclib -Wl,-no_compact_unwind \
    -cclib $LIBFFI_LIB_INC \
    -cclib $LIBFFI_LIB


