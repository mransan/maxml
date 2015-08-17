PB_HINC=../../../install/build/include/
PB_LINC=../../../install/build/lib/

OCAMLOPT=../../../install/build/bin/ocamlopt.opt
PBC_INC=../../../install/build/lib/ocaml/site-lib/ocaml-protobuf/

PROTOC=../../../install/build/bin/protoc 
ML_PROTOC=../ml-protoc


test01.pb.cc: test01.proto
	$(PROTOC) --cpp_out ./ test01.proto

test01_cpp.tsk: main01_cpp.cpp test01.pb.cc 
	g++ -I ./  -I $(PB_HINC) $? -L $(PB_LINC) -l protobuf -o $@

test01_types.ml test01_types.mli : test01.proto
	$(ML_PROTOC) -debug -out ./test01_types test01.proto

test01_ml.tsk: test01_types.mli test01_types.ml main01_ml.ml 
	$(OCAMLOPT) -I $(PBC_INC) str.cmxa protobuf_codec.cmxa $? -o $@ 

.PHONY: all clean test01 

test01: test01_ml.tsk test01_cpp.tsk 
	./test01_cpp.tsk encode
	./test01_ml.tsk decode
	./test01_ml.tsk encode
	./test01_cpp.tsk decode

test02.pb.cc: test02.proto
	$(PROTOC) --cpp_out ./ test02.proto

test02_cpp.tsk: main02_cpp.cpp test02.pb.cc 
	g++ -I ./  -I $(PB_HINC) $? -L $(PB_LINC) -l protobuf -o $@

test02_types.ml test02_types.mli : test02.proto
	$(ML_PROTOC) -debug -out ./test02_types test02.proto

test02_ml.tsk: test02_types.mli test02_types.ml main02_ml.ml 
	$(OCAMLOPT) -I $(PBC_INC) str.cmxa protobuf_codec.cmxa $? -o $@ 

.PHONY: all clean test02 

test02: test02_ml.tsk test02_cpp.tsk 
	./test02_cpp.tsk encode
	./test02_ml.tsk decode
	./test02_ml.tsk encode
	./test02_cpp.tsk decode

all: test01 test02

clean:
	rm -f test0*.pb*
	rm -f test0*_types.*
	rm -f *.tsk
	rm -f *.data
	rm -f *.o

all: test01
