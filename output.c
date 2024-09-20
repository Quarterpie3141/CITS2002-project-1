#include <stdio.h>

double one;
double value;

double increment(double value) {
    return value + one;
return 0; 
}

int main() {
    double one = 1.000000;
    printf("%f\n", increment(3.000000) + increment(4.000000));
    return 0;
}
