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

sqlite3_int64 getTime() {
  struct timespec tms;

  if (clock_gettime(CLOCK_REALTIME, &tms)) {
    return -1;
  }
  int64_t nanos = tms.tv_sec * 1000000000;
  nanos += tms.tv_nsec;
  return nanos;
}

char *getDbFile() {
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
  return bevel_db_file;
}

int before(char **argv) {
  char *bevel_db_file = getDbFile();

  sqlite3 *db;
  int opened = sqlite3_open(bevel_db_file, &db);

  if (opened != SQLITE_OK) {
    goto sqlite_error;
  }

  sqlite3_stmt *stmt;
  int prepared =
      sqlite3_prepare_v2(db,
                         "INSERT INTO command (text, begin, workdir, user, "
                         "host) VALUES (?, ?, ?, ?, ?)",
                         -1, &stmt, NULL);

  if (prepared != SQLITE_OK) {
    goto sqlite_error;
  }

  // We need these things for a "command has started" insertion:
  //
  // 1. Text of the command
  char *text = argv[1];
  // 2. Begin time
  sqlite3_int64 begin = getTime();
  // 3. Workdir
  char workdir[PATH_MAX];
  getcwd(workdir, sizeof(workdir));
  // 4. User
  struct passwd *userinfo;
  userinfo = getpwuid(geteuid());
  // 5. Host
  char hostname[HOST_NAME_MAX + 1];
  gethostname(hostname, HOST_NAME_MAX + 1);

  // We bind all these values to the prepared statement for insertion
  sqlite3_bind_text(stmt, 1, text, -1, NULL);
  sqlite3_bind_int64(stmt, 2, begin);
  sqlite3_bind_text(stmt, 3, workdir, -1, NULL);
  sqlite3_bind_text(stmt, 4, userinfo->pw_name, -1, NULL);
  sqlite3_bind_text(stmt, 5, hostname, -1, NULL);

  int inserted = sqlite3_step(stmt);

  if (inserted != SQLITE_DONE) {
    goto sqlite_error;
  }

  sqlite3_int64 command_id = sqlite3_last_insert_rowid(db);
  printf("%lld\n", command_id);

  sqlite3_finalize(stmt);
  sqlite3_close(db);

  return 0;

sqlite_error:
  fprintf(stderr, "Failed to insert command: %s\n", sqlite3_errmsg(db));
  sqlite3_close(db);

  return 1;
}

int after(char **argv) {

  char *bevel_db_file = getDbFile();

  sqlite3 *db;

  int opened = sqlite3_open(bevel_db_file, &db);

  if (opened != SQLITE_OK) {
    goto sqlite_error;
  }

  sqlite3_stmt *stmt;
  int prepared = sqlite3_prepare_v2(
      db, "UPDATE command SET end = ?, exit = ?, workdir = ? where id = ?", -1,
      &stmt, NULL);

  if (prepared != SQLITE_OK) {
    goto sqlite_error;
  }

  // We need these things to finish up a command row:
  //
  // 1. End time
  sqlite3_int64 end = getTime();
  // 2. Exit code (second command-line argument)
  int exit = atoi(argv[2]);
  // 3. Workdir again, in case the command was 'cd'
  char workdir[PATH_MAX];
  getcwd(workdir, sizeof(workdir));
  // 4. Command id (first command-line argument)
  sqlite3_int64 command_id = atoi(argv[1]);

  sqlite3_bind_int64(stmt, 1, end);
  sqlite3_bind_int(stmt, 2, exit);
  sqlite3_bind_text(stmt, 3, workdir, -1, NULL);
  sqlite3_bind_int64(stmt, 4, command_id);

  int inserted = sqlite3_step(stmt);

  if (inserted != SQLITE_DONE) {
    goto sqlite_error;
  }

  sqlite3_finalize(stmt);
  sqlite3_close(db);

  return 0;

sqlite_error:
  fprintf(stderr, "Failed to complete command: %s\n", sqlite3_errmsg(db));
  sqlite3_close(db);

  return 1;
}

int main(int argc, char **argv) {
  switch (argc) {
  case 0:
    goto arg_error;
  case 1:
    goto arg_error;
  case 2:
    return before(argv);
  case 3:
    return after(argv);
  default:
    goto arg_error;
  }

arg_error:
  fprintf(stderr, "Before: HISTORY_ID=$(bevel-gather <COMMAND>)\n"
                  "After:  bevel-gather <HISTORY_ID> <EXIT_CODE>\n");
  return 1;
}
