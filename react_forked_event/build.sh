source ../ocaml_env.sh

$OCAMLFIND ocamlopt -package react -package unix -linkpkg -I ./ -o run.tsk \
    dispatcher.mli dispatcher.ml \
    pipe_connection.mli pipe_connection.ml \
    selector.mli selector.ml \
    encoding.mli encoding.ml \
    encoding_event.mli encoding_event.ml \
    fork_util.mli fork_util.ml \
    fn.ml \
    forked_event.mli forked_event.ml \
    run.ml
