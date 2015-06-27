# -------------
# This file defines the OCAML environment variables
# 
# It should be included in each example build script:
# 
# source ../ocaml_env.sh
#
# 

DOCUMENTS_DIR=/Users/maximeransan/Documents/
OCAMLDIR=$DOCUMENTS_DIR/ocaml_compiler/
OCAMLOPT=$OCAMLDIR/bin/ocamlopt.opt
OCAMLLEX=$OCAMLDIR/bin/ocamllex
OCAMLYACC=$OCAMLDIR/bin/ocamlyacc
OCAMLDOC=$OCAMLDIR/bin/ocamldoc

OCAML_C_INCLUDE="-I$OCAMLDIR/lib/ocaml/"

GCC=/usr/bin/gcc 
GPP=/usr/bin/g++

OCAMLFIND=$DOCUMENTS_DIR/findlib/build/bin/ocamlfind 

LIBFFI_LIB_INC=-L$DOCUMENTS_DIR/libffi/build/lib/
LIBFFI_LIB=-lffi 


PATH=$OCAMLDIR/bin/:$PATH
