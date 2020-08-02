#include <stdio.h>

int n; 

int main (void) {  
    printf("Choose a number: ");
    scanf("%d", &n);
    for (int i; i < n; i++) {
        printf("#\n");
    }
}
