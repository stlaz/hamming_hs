#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(int argc, char *argv[]) {
    srand(time(NULL));
    int rnd;
    unsigned int mask = 1;
    unsigned int c = 0;
    
    FILE *in = fopen("encoded.out", "rb");
    FILE *out = fopen("rnd.data", "wb");
    
    while((c = fgetc(in)) != EOF) {
        rnd = rand() % 8;
        putc(c ^ (mask << rnd), out);
    }
    fclose(in);
    fclose(out);

    return 0;
}
