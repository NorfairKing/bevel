#include <inttypes.h>
#include <sqlite3.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

sqlite3_int64 getTime() {
  struct timespec tms;

  if (clock_gettime(CLOCK_REALTIME, &tms)) {
    return -1;
  }
  int64_t nanos = tms.tv_sec * 1000000000;
  nanos += tms.tv_nsec;
  return nanos;
}

int main(int argc, char **argv) {

  if (argc < 2) {
    fprintf(stderr, "Call this command with history id and exit code");
    return 1;
  }

  char *bevel_db_file = getenv("BEVEL_DATABASE");
  if (bevel_db_file == NULL) {
    char *data_dir = getenv("XDG_DATA_HOME");
    if (data_dir == NULL) {
      char *home = getenv("HOME");
      if (home == NULL) {
        home = ".";
      };
      data_dir = strcat(home, "/.local/share");
    }
    bevel_db_file = strcat(data_dir, "/bevel/history.sqlite3");
  }

  sqlite3 *db;

  int opened = sqlite3_open(bevel_db_file, &db);

  if (opened != SQLITE_OK) {
    goto sqlite_error;
  }

  sqlite3_stmt *stmt;
  int prepared = sqlite3_prepare_v2(
      db, "UPDATE command SET end = ?, exit = ? where id = ?", -1, &stmt, NULL);

  if (prepared != SQLITE_OK) {
    goto sqlite_error;
  }

  // We need these things to finish up a command row:
  //
  // 1. End time
  sqlite3_int64 end = getTime();
  // 2. Exit code (second command-line argument)
  int exit = atoi(argv[2]);
  // 3. Command id (first command-line argument)
  sqlite3_int64 command_id = atoi(argv[1]);

  sqlite3_bind_int64(stmt, 1, end);
  sqlite3_bind_int(stmt, 2, exit);
  sqlite3_bind_int64(stmt, 3, command_id);

  int inserted = sqlite3_step(stmt);

  if (inserted != SQLITE_DONE) {
    goto sqlite_error;
  }

  sqlite3_finalize(stmt);
  sqlite3_close(db);

  return 0;

sqlite_error:
  fprintf(stderr, "Failed to insert command: %s\n", sqlite3_errmsg(db));
  sqlite3_close(db);

  return 1;
}
