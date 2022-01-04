#include <inttypes.h>
#include <sqlite3.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

int main(void) {



    sqlite3 *db;
    if (sqlite3_open("the.db", &db)) {
        printf("Could not open the.db\n");
        exit(-1);
    }

    if (sqlite3_exec(db,
          "insert into tab values(1, 'one', null);"
          "insert into tab values(2,  2.2 ,'two');",
          NULL, NULL, NULL)) {

       printf("Error executing sql statement\n");
    }
    else {
        printf("records inserted\n");
    }

    sqlite3_close(db);

    struct timespec tms;

    /* The C11 way */
    /* if (! timespec_get(&tms, TIME_UTC)) { */

    /* POSIX.1-2008 way */
    if (clock_gettime(CLOCK_REALTIME,&tms)) {
        return -1;
    }
    /* seconds, multiplied with 1 million */
    int64_t millis = tms.tv_sec * 1000;
    /* Add full milliseconds */
    millis += tms.tv_nsec;
    printf("Nanoseconds: %"PRId64"\n",millis);
    return 0;
}
