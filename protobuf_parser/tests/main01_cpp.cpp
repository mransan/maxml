#include <test01.pb.h>

#include <iostream>
#include <fstream>

using namespace foo::bar; 

Couple create_test_couple() {
    Couple cp; 
    {
        Person& p = *cp.mutable_p1();
        p.set_first_name("John");
        p.set_last_name("Doe"); 
        p.set_date_of_birth(19820429);
        p.set_employed_by("Google");
    }
    {
        Person& p = *cp.mutable_p2();
        p.set_first_name("Marie");
        p.set_last_name("Dupont"); 
        p.set_date_of_birth(19820306);
        p.set_employed_by("INRIA");
        
        Person_TelNumber& t = *p.mutable_tel_number();
        t.set_area_code(917);
        t.set_number(1111111);
    }

    return cp;
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
        return encode_to_file(create_test_couple(), "test01.c2ml.data");
    }
    else if(mode == "decode") {
        Couple cp; 
        int rc = decode_from_file(cp, "test01.ml2c.data");
        if(rc) {
            std::cerr << "C++: Failed to decode" 
                      << std::endl; 
            return 1;
        }
        else {
            std::cout << "C++: -- Good --" 
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

