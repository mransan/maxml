source ../ocaml_env.sh

rm -rf *.o
rm -rf *.cm*
rm -rf *.tsk

$GPP -O2 -o cpp2ml.o \
     -Wc++11-compat-deprecated-writable-strings \
     -c $OCAML_C_INCLUDE \
     cpp2ml.cpp

$OCAMLFIND ocamlopt -package unix -linkpkg \
          -o using_cpp_container.tsk  \
          -cc "$GPP"  \
          cpp2ml.o main.ml
