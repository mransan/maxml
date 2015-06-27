source ../ocaml_env.sh

# clean up
#
rm -f *.o
rm -f *.cm*
rm -f *.tsk

# export $PATH
$OCAMLFIND ocamlopt -package unix -linkpkg  -o  algorithms.tsk \
    util.ml \
    binary_number.ml \
    random_list.ml \
    lazy_list.ml \
    queue.ml \
    rt_queue.ml \
    rotate_queue.ml \
    lazy_queue.ml \
    dl_list.mli \
    dl_list.ml \
    heap.mli \
    heap.ml  \
    sub_array.ml \
    sort.ml \
    main.ml
