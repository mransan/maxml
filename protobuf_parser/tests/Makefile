PB_HINC=../../../install/build/include/
PB_LINC=../../../install/build/lib/

OCAMLOPT=../../../install/build/bin/ocamlopt.opt
PBC_INC=../../../install/build/lib/ocaml/site-lib/ocaml-protobuf/

PROTOC=../../../install/build/bin/protoc 
ML_PROTOC=../ml-protoc

test_util.cmx:test_util.ml
	$(OCAMLOPT) -I $(PBC_INC) -c $? 

%.pb.cc: %.proto
	$(PROTOC) --cpp_out ./ $<

%_cpp.tsk: %_cpp.cpp %.pb.cc 
	g++ -I ./  -I $(PB_HINC) $? -L $(PB_LINC) -l protobuf -o $@

%_pb.ml %_pb.mli : %.proto
	$(ML_PROTOC) -debug $<

%_ml.tsk: test_util.cmx %_pb.mli %_pb.ml %_ml.ml 
	$(OCAMLOPT) -I $(PBC_INC) unix.cmxa str.cmxa protobuf_codec.cmxa $? -o $@ 

.PHONY: all clean 

test%: test%_ml.tsk test%_cpp.tsk 
	time ./test$*_cpp.tsk encode
	time ./test$*_ml.tsk decode
	time ./test$*_ml.tsk encode
	time ./test$*_cpp.tsk decode


testCompat: test04_ml.tsk test03_cpp.tsk 
	./test03_cpp.tsk encode
	./test04_ml.tsk decode
	./test04_ml.tsk encode
	./test03_cpp.tsk decode

all: test01 test02 testCompat test05 test06 test07 test08 


clean:
	rm -f test0*.pb*
	rm -f test0*_pb.*
	rm -f *.tsk
	rm -f *.data
	rm -f *.o
	rm -f *.cm*
