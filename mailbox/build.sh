source ../ocaml_env.sh

# clean up
#
rm -f *.o
rm -f *.cm*
rm -f *.tsk

# compile C binding
#
$GCC -o ml_sleep.o -c $OCAML_C_INCLUDE ml_sleep.c


# export $PATH
$OCAMLFIND ocamlopt -thread -c  mailbox.ml

$OCAMLFIND ocamlopt -thread \
    -package unix -package threads -linkpkg \
    -o  mailbox.tsk \
    mailbox.cmx \
    ml_sleep.o 
