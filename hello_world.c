#include <stdio.h>

char name[256];

int main(void) {
    printf("Type your name: ");
    gets(name);
    printf("Hello, %s!\n", name);
    return 0;
}