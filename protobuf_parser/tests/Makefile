

PB_HINC=../../../install/build/include/
PB_LINC=../../../install/build/lib/


OCAMLOPT=../../../install/build/bin/ocamlopt.opt
PBC_INC=../../../install/build/lib/ocaml/site-lib/ocaml-protobuf/


test01.tsk: main01.cpp test01.pb.cc 
	g++ -I ./  -I $(PB_HINC) $? -L $(PB_LINC) -l protobuf -o $@

test01_ml.tsk: test01_ml.ml
	$(OCAMLOPT) -I $(PBC_INC) protobuf_codec.cmxa $? -o $@ 

.PHONY: all clean test_01 

test01: test01.tsk test01_ml.tsk 
	./test01.tsk
	./test01_ml.tsk

clean:
	rm -f *.tsk
	rm -f *.o

all: test01
