#include <stdint.h>
#include <stdio.h>
#include <time.h>
#include <inttypes.h>

int main(void) {

    struct timespec tms;

    /* The C11 way */
    /* if (! timespec_get(&tms, TIME_UTC)) { */

    /* POSIX.1-2008 way */
    if (clock_gettime(CLOCK_REALTIME,&tms)) {
        return -1;
    }
    /* seconds, multiplied with 1 million */
    int64_t micros = tms.tv_sec * 1000;
    /* Add full milliseconds */
    millis += tms.tv_nsec;
    printf("Nanoseconds: %"PRId64"\n",millis);
    return 0;
}
