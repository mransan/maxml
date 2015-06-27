source ../ocaml_env.sh

$OCAMLFIND ocamlopt -package react -package unix -linkpkg -I ./ -o run.tsk \
    dispatcher.mli dispatcher.ml \
    encoding.ml \
    fn.ml \
    forked_event.mli forked_event.ml \
    run.ml
