DIRS="
./algorithms_book_examples
./binomial_model
./blpapi
./bucket_problem
./c_bindings
./command_line_parsing
./extended_algebra
./intro_proba
./language
./learning_examples
./market
./mixed_int_float_algebra
./module
./module_sig
./moving_average
./pack_module
./print_tree
./react_breakout
./react_clock
./root_finding
./rt
./shoc/c
./shoc/ocaml
./small_float_number_algebra
"
./clean_example 

BASE=`pwd`
for dir_i in $DIRS
do
    echo "Building $dir_i ..."
    cd $dir_i 
    ./build.sh 
    cd $BASE
done
