#include <test05.pb.h>

#include <test_util.h>

#include <iostream>
#include <fstream>
    
namespace { 
int inc = INT_MAX / 1000000; 
}

IntList create_test() {
    IntList t;

    for(int i = INT_MIN; i<INT_MAX - 2*inc ; i+= inc) {
        t.add_l(i);
    } 

    return t;
}

int main(int argc, char const* const argv[]) {
    check_argv(argc, argv);
    std::string mode(argv[1]);

    std::cout << "inc : " << inc << std::endl; 
    std::cout << "min_int : " << INT_MIN << ", max_int : " << INT_MAX << std::endl; 

    if(mode == "encode") {
        return encode_to_file(create_test(), "test05.c2ml.data");
    }
    else if(mode == "decode") {
        IntList t; 
        validate_decode(t, "test05.ml2c.data", false);
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}

