# -------------
# This file defines the OCAML environment variables
# 
# It should be included in each example build script:
# 
# source ../ocaml_env.sh
#
# 

BUILD=/Users/maximeransan/Documents/install/build
OCAMLOPT=$BUILD/bin/ocamlopt.opt
OCAMLLEX=$BUILD/bin/ocamllex
OCAMLYACC=$BUILD/bin/ocamlyacc
OCAMLDOC=$BUILD/bin/ocamldoc

OCAML_C_INCLUDE="-I$BUILD/lib/ocaml/"

GCC=/usr/bin/gcc 
GPP=/usr/bin/g++

OCAMLFIND=$BUILD/bin/ocamlfind 

LIBFFI_LIB_INC=-L$DOCUMENTS_DIR/libffi/build/lib/
LIBFFI_LIB=-lffi 


PATH=$BUILD/bin/:$PATH
