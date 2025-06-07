CREATE TABLE "command"("id" INTEGER PRIMARY KEY,"text" VARCHAR NOT NULL,"begin" INTEGER NOT NULL,"end" INTEGER NULL DEFAULT NULL,"workdir" VARCHAR NOT NULL,"user" VARCHAR NOT NULL,"host" VARCHAR NOT NULL,"exit" INTEGER NULL DEFAULT NULL,"server_id" INTEGER NULL DEFAULT NULL);

-- ATTENTION CODE REVIEWER
-- If this file has been updated, please make sure to check
-- whether this test failed before that happened:
-- "Bevel.Client.Data.DBSpec.Can automatically migrate from the previous database schema"
-- If this test failed beforehand, but this golden test has
-- been updated anyway, that means the current migration is
-- dangerous with respect to the current database.
