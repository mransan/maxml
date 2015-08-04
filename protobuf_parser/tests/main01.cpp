

#include <test01.pb.h>

#include <iostream>
#include <fstream>

int main() {

    M m; 
    m.set_v1(123);
    m.set_v2("I am a test string");

    std::ofstream out("test01.data");
    m.SerializeToOstream(&out);
    if(! out.good()) {
        std::cerr << "Error writing message to file"
                  << std::endl;

        return 1;
    }

    return 0;
}

