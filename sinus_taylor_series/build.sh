source ../ocaml_env.sh

$OCAMLOPT -o sinus.tsk main.ml


g++ -o sinus_cpp.tsk main_cpp.cpp
