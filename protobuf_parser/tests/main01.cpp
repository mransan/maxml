#include <test01.pb.h>

#include <iostream>
#include <fstream>

using namespace foo::bar; 

int main() {

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


    std::ofstream out("test01.data");
    cp.SerializeToOstream(&out);
    if(! out.good()) {
        std::cerr << "Error writing message to file"
                  << std::endl;
        return 1;
    }

    return 0;
}

