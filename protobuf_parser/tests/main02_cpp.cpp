#include <test02.pb.h>

#include <iostream>
#include <fstream>


AllBasicsTypes create_test_all_basic_types() {
    AllBasicsTypes abt;
    abt.set_o01(1.0); 
    abt.set_o02(2.0); 
    abt.set_o03(-123); 
    abt.set_o04(456); 
    abt.set_o05(123); 
    abt.set_o06(456); 
    abt.set_o07(-123); 
    abt.set_o08(-456); 
    abt.set_o09(123); 
    abt.set_o10(456); 
    // abt.set_o11(-123); 
    // abt.set_o12(-456); 
    abt.set_o13(true); 
    abt.set_o14("Iam a test string"); 
    abt.set_o15("Iam a test byte"); 

    return abt;
}

template <typename T>
int encode_to_file(T const& message, std::string const& file_name) {

    std::ofstream out(file_name);
    message.SerializeToOstream(&out);
    if(! out.good()) {
        std::cerr << "Error writing message to file"
                  << std::endl;
        return 1;
    }
    out.close(); 
    return 0;
}

template <typename T>
int decode_from_file(T& message, std::string const& file_name) {

    std::ifstream in(file_name); 
    if(!in.is_open() || !in.good()) {
        std::cerr << "Error opening the file"
                  << std::endl;
        return 1;
    }
    bool success = message.ParseFromIstream(&in);
    if(!success) {
        return 1;
    }
    else {
        return 0;
    }
}

int main(int argc, char const* const argv[]) {

    if(argc < 2) {
        std::cerr << "Invalid number of argument, must be: "
                  << std::endl
                  << argv[0]
                  << " [encode|decode]"
                  << std::endl;

        return 1;
    }

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_test_all_basic_types(), "test02.c2ml.data");
    }
    else if(mode == "decode") {
        AllBasicsTypes abt; 
        int rc = decode_from_file(abt, "test02.ml2c.data");
        if(rc) {
            std::cerr << "C++: Failed to decode" 
                      << std::endl; 
            return 1;
        }
        else {
            std::cout << "C++: -- Good --" 
                      << std::endl
                      << abt.DebugString()
                      << std::endl;
            return 0;
        }
       
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}

