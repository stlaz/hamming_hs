#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(int argc, char *argv[]) {
    srand(time(NULL));
    int rnd;
    unsigned int mask = 1;
    unsigned char input[50] = {'\0'};
    unsigned int c = 0;
    
    FILE *in = fopen("encoded.out", "rb");
    
    int i = 0;
    while((c = fgetc(in)) != EOF) {
        input[i] = c;
        i++;
    }
    fclose(in);

    FILE *out = fopen("rnd.data", "wb");
    // FIXME: the two of the while cycles can be merged
    for(int j = 0; j < i; j++) {
        rnd = rand() % 8;
        // printf("[%d] %d -> %d\n", j, input[j], input[j] ^ (mask << rnd));
        putc(input[j] ^ (mask << rnd), out);
    }
    fclose(out);
    return 0;
}
