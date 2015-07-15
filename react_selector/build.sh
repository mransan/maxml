source ../ocaml_env.sh

rm *.o
rm *.cm*

$OCAMLFIND ocamlopt -package react -package unix -linkpkg -I ./ -o run.tsk \
    pipe_connection.mli pipe_connection.ml \
    selector.mli selector.ml \
    encoding.mli encoding.ml \
    encoding_event.mli encoding_event.ml \
    fork_util.mli fork_util.ml \
    run.ml
