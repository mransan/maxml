
echo "building sub"
cd ./sub/
./build.sh
cd ../
echo "building Other sub"
cd ./other_sub/
./build.sh
cd ../

source ../ocaml_env.sh


sub=./sub_bin
other_sub=./other_sub_bin

$OCAMLOPT -I $sub -I $other_sub -c main.ml 
$OCAMLOPT -o sub_module.tsk -I $sub  -I $other_sub sub.cmxa other_sub.cmxa main.cmx 
$OCAMLDOC -html -I $sub -I $other_sub  main.ml $sub/sub.mli 
