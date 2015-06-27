source ../ocaml_env.sh

# clean up
#
rm -f *.o
rm -f *.cm*
rm -f *.tsk

$OCAMLLEX lex1.mll
$OCAMLLEX lex2.mll
$OCAMLLEX lex3.mll
$OCAMLLEX lex4.mll
$OCAMLLEX lex5.mll
$OCAMLLEX lex6.mll
$OCAMLLEX xml_lexer.mll

# export $PATH
$OCAMLFIND ocamlopt -o  lex.tsk \
    xml_types.mli \
    xml_types.ml \
    xml_lexer.ml \
    lex6.ml \
    lex5.ml \
    lex4.ml \
    lex3.ml \
    lex2.ml \
    lex1.ml \
    main.ml
