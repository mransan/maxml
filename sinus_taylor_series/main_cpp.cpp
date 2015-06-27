#include <iostream>
#include <iomanip>
#include <sstream>
#include <cmath>


double sine(double x, int n) {

    double sum  = x; 
    double sign = 1; 
    double nom  = x; 
    double den  = 1; 

    for(std::size_t i=1; i<n; i=i+2) {
        sign *= -1;
        nom   = nom*x*x; 
        den   = den*(i+1)*(i+2);
        sum  += sign * nom / den; 
    }

    return sum; 
}

void test(std::ostream& out, double x) {
    static int CASES[] = {3, 5, 11, 51, 111 }; 
    static std::size_t CASES_SIZE = sizeof(CASES)/sizeof(CASES[0]);

    out << std::setw(10) << x << " = "; 
    for(std::size_t i = 0; i < CASES_SIZE; ++i) {

        int n = CASES[i]; 
        double t_value = sine(x,n); 
        double c_value = std::sin(x); 
        double error = 100 * std::abs((c_value-t_value)/c_value); 
        out << std::setw(15) << error << "|"; 
    }

    out << std::endl;
}


int main () {

    std::ostringstream out; 
    out.setf(std::ios::fixed|std::ios::showpoint);
    out << std::setw(15) << std::setprecision(2);
    for(std::size_t i=0; i<100000; ++i) {
        test(out, i*0.2);
    }

    std::cout << out.str() ;
    return 0;
}
