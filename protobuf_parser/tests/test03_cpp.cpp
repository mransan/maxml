#include <test03.pb.h>

#include <iostream>
#include <fstream>

Test create_test() {
    Test t;
    t.set_i(123);
    t.set_j(456);

    return t;
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
        return encode_to_file(create_test(), "test03.c2ml.data");
    }
    else if(mode == "decode") {
        Test t; 
        int rc = decode_from_file(t, "test04.ml2c.data");
        if(rc) {
            std::cerr << "C++: Failed to decode" 
                      << std::endl; 
            return 1;
        }
        else {
            std::cout << "C++: -- Good --" 
                      << std::endl
                      << t.DebugString()
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

