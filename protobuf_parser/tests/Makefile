

PB_HINC=../../../install/build/include/
PB_LINC=../../../install/build/lib/


OCAMLOPT=../../../install/build/bin/ocamlopt.opt
PBC_INC=../../../install/build/lib/ocaml/site-lib/ocaml-protobuf/


test01.pb.cc: test01.proto
	../../../install/build/bin/protoc --cpp_out ./ test01.proto

test01.tsk: main01.cpp test01.pb.cc 
	g++ -I ./  -I $(PB_HINC) $? -L $(PB_LINC) -l protobuf -o $@

test01_types.ml test01_types.mli : test01.proto
	../ml-protoc -debug -out ./test01_types test01.proto

test01_ml.tsk: test01_types.mli test01_types.ml test01_ml.ml 
	$(OCAMLOPT) -I $(PBC_INC) protobuf_codec.cmxa $? -o $@ 

.PHONY: all clean test_01 

test01: test01_ml.tsk test01.tsk 
	./test01.tsk encode
	./test01_ml.tsk decode
	./test01_ml.tsk encode
	./test01.tsk decode

clean:
	rm -f test01.pb*
	rm -f test01_types.*
	rm -f *.tsk
	rm -f *.data
	rm -f *.o

all: test01
