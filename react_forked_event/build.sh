source ../ocaml_env.sh

$OCAMLFIND ocamlopt -package react -package unix -linkpkg -I ./ -o run.tsk \
    dispatcher.mli dispatcher.ml \
    pipe_connection.mli pipe_connection.ml \
    select.mli select.ml \
    encoding.mli encoding.ml \
    forked_util.mli forked_util.ml \
    fn.ml \
    forked_event.mli forked_event.ml \
    run.ml
