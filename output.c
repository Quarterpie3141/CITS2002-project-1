#include <stdio.h>
#include <stdlib.h>

double one = 0 ;
double value = 0 ;

double increment(double value) {
    double one = 1.000000;
    return value + one;
    
return 0; 
}

int main() {
    
    printf("%f\n", increment(3.000000) + increment(4.000000));
    return 0;
}
