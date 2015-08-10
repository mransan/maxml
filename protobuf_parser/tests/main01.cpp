#include <test01.pb.h>

#include <iostream>
#include <fstream>

int main() {

    N_M m; 
    m.set_v1(123);
    m.set_v2("I am a test string");

    N n; 
    n.set_n1(1.2);
    *(n.mutable_n2()) = m;
    n.set_o1(123);
    n.set_o2("i am o2");

    std::ofstream out("test01.data");
    n.SerializeToOstream(&out);
    if(! out.good()) {
        std::cerr << "Error writing message to file"
                  << std::endl;
        return 1;
    }

    return 0;
}

