#include <inttypes.h>
#include <limits.h>
#include <pwd.h>
#include <sqlite3.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#define DB_FILE "/home/syd/.local/share/bevel/data.sqlite3"

sqlite3_int64 getTimeMillis() {
  struct timespec tms;

  if (clock_gettime(CLOCK_REALTIME, &tms)) {
    return -1;
  }
  int64_t millis = tms.tv_sec * 1000;
  millis += tms.tv_nsec;
  return millis;
}

int main(void) {

  sqlite3 *db;

  int opened = sqlite3_open(DB_FILE, &db);

  if (opened != SQLITE_OK) {
    goto error;
  }

  sqlite3_stmt *stmt;
  int prepared =
      sqlite3_prepare_v2(db,
                         "INSERT INTO command (text, begin, workdir, user, "
                         "host) VALUES (?, ?, ?, ?, ?)",
                         -1, &stmt, NULL);

  if (prepared != SQLITE_OK) {
    goto error;
  }

  // We need these things for a "command has started" insertion:
  //
  // 1. Text of the command

  // 2. Begin time
  sqlite3_int64 begin = getTimeMillis();
  // 3. Workdir
  // 4. User
  struct passwd *userinfo;
  userinfo = getpwuid(geteuid());
  // 5. Host
  char hostname[HOST_NAME_MAX + 1];
  gethostname(hostname, HOST_NAME_MAX + 1);

  // We bind all these values to the prepared statement for insertion
  sqlite3_bind_text(stmt, 1, "here goes the command", -1, NULL);
  sqlite3_bind_int64(stmt, 2, begin);
  sqlite3_bind_text(stmt, 3, "here goes the workdir", -1, NULL);
  sqlite3_bind_text(stmt, 4, userinfo->pw_name, -1, NULL);
  sqlite3_bind_text(stmt, 5, hostname, -1, NULL);

  // TODO deal with errors here
  int inserted = sqlite3_step(stmt);

  if (inserted != SQLITE_DONE) {
    goto error;
  }

  sqlite3_int64 command_id = sqlite3_last_insert_rowid(db);
  printf("%lld\n", command_id);

  sqlite3_finalize(stmt);
  sqlite3_close(db);

  return 0;

error:
  fprintf(stderr, "Failed to insert command: %s\n", sqlite3_errmsg(db));
  sqlite3_close(db);

  return 1;
}
