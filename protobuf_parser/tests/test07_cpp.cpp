#include <test07.pb.h>

#include <test_util.h>

#include <iostream>


Node create_test_node() {
    Node n; 
    {
        Node& l = *n.mutable_left();
        l.set_value(2); 
        l.clear_left(); 
        l.clear_right();
    }
    {
        Node& r = *n.mutable_right();
        r.set_value(3); 
        r.clear_left(); 
        r.clear_right();
    }
    n.set_value(1); 
    return n;
}


int main(int argc, char const* const argv[]) {

    check_argv(argc, argv);

    std::string mode(argv[1]);

    if(mode == "encode") {
        return encode_to_file(create_test_node(), "test07.c2ml.data");
    }
    else if(mode == "decode") {
        Node n; 
        validate_decode(n, "test07.ml2c.data");
        assert(n.DebugString() == create_test_node().DebugString()); 
    }
    else {
        std::cerr << "Invalid second argument: " 
                  << argv[1]
                  << std::endl;
        return 1;
    }
}

