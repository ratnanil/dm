# FIXME: #313: learn only from current source

test_that("Standard learning from MSSQL (schema 'dbo') or Postgres (schema 'public') and get_src_tbl_names() works?", {
  skip_if_src_not(c("mssql", "postgres", "maria"))

  # dm_learn_from_mssql() --------------------------------------------------
  con_db <- my_test_con()

  # create an object on the MSSQL-DB that can be learned
  withr::defer(
    try(walk(
      remote_tbl_names,
      ~ try(dbExecute(con_db, paste0("DROP TABLE ", .x)))
    ))
  )

  dm_for_filter_copied <- copy_dm_to(con_db, dm_for_filter(), temporary = FALSE, table_names = ~ DBI::SQL(unique_db_table_name(.x)))
  order_of_deletion <- c("tf_2", "tf_1", "tf_5", "tf_6", "tf_4", "tf_3")

  remote_tbl_names <-
    map_chr(
      dm_get_tables(dm_for_filter_copied)[order_of_deletion],
      dbplyr::remote_name
    ) %>%
    SQL() %>%
    DBI::dbUnquoteIdentifier(conn = con_db) %>%
    map_chr(~ .x@name[["table"]])

  remote_tbl_map <- set_names(remote_tbl_names, gsub("^(tf_.).*$", "\\1", remote_tbl_names))

  # test 'get_src_tbl_names()'
  src_tbl_names <- sort(unname(gsub("^.*\\.", "", get_src_tbl_names(con_db))))
  expect_identical(
    # fail if there are other tables in the default schema
    src_tbl_names[grep("tf_._", src_tbl_names)],
    sort(dbQuoteIdentifier(con_db, remote_tbl_names))
  )

  expect_snapshot({
    dm_from_con(con_db)[integer()]
  })
})

test_that("Standard learning from MSSQL (schema 'dbo') or Postgres (schema 'public') and get_src_tbl_names() works?", {
  skip_if_src_not(c("mssql", "postgres", "maria"))

  # dm_learn_from_mssql() --------------------------------------------------
  con_db <- my_test_con()

  # create an object on the MSSQL-DB that can be learned
  withr::defer(
    try(walk(
      remote_tbl_names,
      ~ try(dbExecute(con_db, paste0("DROP TABLE ", .x)))
    ))
  )

  dm_for_filter_copied <- copy_dm_to(con_db, dm_for_filter(), temporary = FALSE, table_names = ~ DBI::SQL(unique_db_table_name(.x)))
  order_of_deletion <- c("tf_2", "tf_1", "tf_5", "tf_6", "tf_4", "tf_3")

  remote_tbl_names <-
    map_chr(
      dm_get_tables(dm_for_filter_copied)[order_of_deletion],
      dbplyr::remote_name
    ) %>%
    SQL() %>%
    DBI::dbUnquoteIdentifier(conn = con_db) %>%
    map_chr(~ .x@name[["table"]])

  remote_tbl_map <- set_names(remote_tbl_names, gsub("^(tf_.).*$", "\\1", remote_tbl_names))

  expect_silent(dm_db_learned_all <- dm_from_con(con_db, learn_keys = TRUE))

  # Select and fix table names
  dm_db_learned <-
    dm_db_learned_all %>%
    dm_select_tbl(!!!remote_tbl_map)

  expect_equivalent_dm(
    dm_db_learned,
    dm_for_filter()[order_of_deletion],
    # FIXME: Enable fetching of on_delete information
    ignore_on_delete = TRUE
  )

  # learning without keys:
  expect_silent(dm_db_learned_no_keys <- dm_from_con(con_db, learn_keys = FALSE))

  # for learning from DB without learning the key relations
  dm_for_filter_no_keys <-
    dm_for_filter()[order_of_deletion] %>%
    dm_get_def() %>%
    mutate(
      pks = list_of(new_pk()),
      fks = list_of(new_fk())
    ) %>%
    new_dm3()

  # Select and fix table names
  dm_db_learned_no_keys <-
    dm_db_learned_no_keys %>%
    dm_select_tbl(!!!remote_tbl_map)

  expect_equivalent_dm(
    dm_db_learned_no_keys,
    dm_for_filter_no_keys
  )
})


test_that("Learning from specific schema on MSSQL or Postgres works?", {
  skip_if_src_not(c("mssql", "postgres", "maria"))

  # produces a randomized schema name with a length of 4-10 characters
  # consisting of the symbols in `reservoir`
  random_schema <- function() {
    reservoir <- c(letters)
    how_long <- 10
    paste0(reservoir[sample(seq_len(length(reservoir)), how_long, replace = TRUE)], collapse = "")
  }

  schema_name <- random_schema()

  con_db <- my_test_con()

  schema_name_q <- DBI::dbQuoteIdentifier(con_db, schema_name)

  DBI::dbExecute(con_db, paste0("CREATE SCHEMA ", schema_name_q))

  dm_for_disambiguate_copied <- copy_dm_to(
    con_db,
    dm_for_disambiguate(),
    temporary = FALSE,
    schema = schema_name
  )
  order_of_deletion <- c("iris_3", "iris_2", "iris_1")
  remote_tbl_names <- set_names(
    paste0(schema_name_q, ".\"", order_of_deletion, "\""),
    order_of_deletion
  )

  withr::defer({
    walk(
      remote_tbl_names,
      ~ try(dbExecute(con_db, paste0("DROP TABLE ", .x)))
    )
    try(dbExecute(con_db, paste0("DROP SCHEMA ", schema_name_q)))
  })

  normalize_table_name <- function(x) {
    tolower(gsub('["`]', "", x))
  }

  # test 'get_src_tbl_names()'
  expect_identical(
    sort(normalize_table_name(get_src_tbl_names(con_db, schema = schema_name))),
    SQL(sort(normalize_table_name(remote_tbl_names)))
  )

  # learning with keys:
  dm_db_learned <-
    dm_from_con(con_db, schema = schema_name, learn_keys = TRUE) %>%
    dm_select_tbl(!!!order_of_deletion)

  expect_equivalent_dm(
    dm_db_learned,
    dm_for_disambiguate()[order_of_deletion]
  )

  # learning without keys:
  dm_db_learned_no_keys <-
    expect_silent(dm_from_con(con_db, schema = schema_name, learn_keys = FALSE)) %>%
    dm_select_tbl(!!!order_of_deletion)

  dm_for_disambiguate_no_keys <-
    dm_for_disambiguate()[order_of_deletion] %>%
    dm_get_def() %>%
    mutate(
      pks = list_of(new_pk()),
      fks = list_of(new_fk())
    ) %>%
    new_dm3()

  expect_equivalent_dm(
    dm_db_learned_no_keys,
    dm_for_disambiguate_no_keys
  )
})

test_that("Learning from SQLite works (#288)?", {
  skip("FIXME")
  src_sqlite <- skip_if_error(src_sqlite()(":memory:", TRUE))

  copy_to(src_sqlite(), tibble(a = 1:3), name = "test")

  expect_equivalent_dm(
    src_sqlite() %>%
      dm_from_con() %>%
      dm_select_tbl(test) %>%
      collect(),
    dm(test = tibble(a = 1:3))
  )
})


test_that("'schema_if()' works", {
  con_db <- my_db_test_src()$con

  # all 3 naming parameters set ('table' is required)
  expect_match(
    unclass(expect_s4_class(
      schema_if(
        schema = "schema",
        table = "table",
        con = con_db,
        dbname = "db"
      ),
      "SQL"
    )),
    "\"db\".\"schema\".\"table\"|`db`.`schema`.`table`"
  )

  # schema and table set
  expect_match(
    unclass(expect_s4_class(
      schema_if(
        schema = "schema",
        table = "table",
        con = con_db
      ),
      "SQL"
    )),
    "\"schema\".\"table\"|`schema`.`table`"
  )

  # dbname and table set
  expect_error(schema_if(schema = NA, con = con_db, table = "table", dbname = "db"))

  # only table set
  expect_match(
    unclass(expect_s4_class(
      schema_if(schema = NA, table = "table", con = con_db),
      "SQL"
    )),
    "\"table\"|`table`"
  )
})


# Learning from other DB on MSSQL -----------------------------------------
test_that("Learning from MSSQL (schema 'dbo') on other DB works?", {
  skip_if_src_not("mssql")
  # dm_learn_from_mssql() --------------------------------------------------
  con_db <- my_test_con()

  # delete database after test
  withr::defer({
    try(dbExecute(con_db, "DROP TABLE [test_database_dm].[dbo].[test_2]"))
    try(dbExecute(con_db, "DROP TABLE [test_database_dm].[dbo].[test_1]"))
    try(dbExecute(con_db, "DROP DATABASE test_database_dm"))
  })

  # create another DB and 2 connected tables
  DBI::dbExecute(con_db, "CREATE DATABASE test_database_dm")
  dbWriteTable(
    con_db,
    DBI::Id(db = "test_database_dm", schema = "dbo", table = "test_1"),
    value = tibble(a = c(5L, 5L, 4L, 2L, 1L), b = 1:5)
  )
  dbWriteTable(
    con_db,
    DBI::Id(db = "test_database_dm", schema = "dbo", table = "test_2"),
    value = tibble(c = c(1L, 1L, 1L, 5L, 4L), d = c(10L, 11L, 10L, 10L, 11L))
  )
  # set PK
  DBI::dbExecute(con_db, "ALTER TABLE [test_database_dm].[dbo].[test_1] ALTER COLUMN [b] INTEGER NOT NULL")
  DBI::dbExecute(con_db, "ALTER TABLE [test_database_dm].[dbo].[test_1] ADD PRIMARY KEY ([b])")
  # set FK relation
  DBI::dbExecute(con_db, "ALTER TABLE [test_database_dm].[dbo].[test_2] ADD FOREIGN KEY ([c]) REFERENCES [test_database_dm].[dbo].[test_1] ([b]) ON DELETE NO ACTION ON UPDATE NO ACTION")


  # test 'get_src_tbl_names()'
  src_tbl_names <- unname(get_src_tbl_names(con_db, dbname = "test_database_dm"))
  expect_identical(
    src_tbl_names,
    sort(DBI::SQL(paste0(
      DBI::dbQuoteIdentifier(con_db, "test_database_dm"), ".",
      DBI::dbQuoteIdentifier(con_db, "dbo"), ".",
      DBI::dbQuoteIdentifier(con_db, c("test_1", "test_2"))
    )))
  )

  dm_local_no_keys <- dm(
    test_1 = tibble(a = c(5L, 5L, 4L, 2L, 1L), b = 1:5),
    test_2 = tibble(c = c(1L, 1L, 1L, 5L, 4L), d = c(10L, 11L, 10L, 10L, 11L))
  )

  expect_message(dm_db_learned <- dm_from_con(con_db, dbname = "test_database_dm"))
  dm_learned <- dm_db_learned %>% collect()
  expect_equivalent_dm(
    dm_learned,
    dm_local_no_keys %>%
      dm_add_pk(test_1, b) %>%
      dm_add_fk(test_2, c, test_1)
  )

  # learning without keys:
  dm_learned_no_keys <- expect_silent(
    dm_from_con(
      con_db,
      dbname = "test_database_dm",
      learn_keys = FALSE
    ) %>%
      collect()
  )
  expect_equivalent_dm(
    dm_learned_no_keys[c("test_1", "test_2")],
    dm_local_no_keys
  )
})


# Learning from a specific schema in another DB on MSSQL -----------------------------------------
test_that("Learning from a specific schema in another DB for MSSQL works?", {
  skip_if_src_not("mssql")
  # dm_learn_from_mssql() --------------------------------------------------
  con_db <- my_test_con()

  original_dbname <- attributes(con_db)$info$dbname

  # create another DB, a schema and 2 connected tables
  DBI::dbExecute(con_db, "CREATE DATABASE test_database_dm")
  DBI::dbExecute(con_db, "USE test_database_dm")
  DBI::dbExecute(con_db, "CREATE SCHEMA dm_test")
  DBI::dbExecute(con_db, paste0("USE ", original_dbname))

  dbWriteTable(
    con_db,
    DBI::Id(db = "test_database_dm", schema = "dm_test", table = "test_1"),
    value = tibble(a = c(5L, 5L, 4L, 2L, 1L), b = 1:5)
  )
  dbWriteTable(
    con_db,
    DBI::Id(db = "test_database_dm", schema = "dm_test", table = "test_2"),
    value = tibble(c = c(1L, 1L, 1L, 5L, 4L), d = c(10L, 11L, 10L, 10L, 11L))
  )
  # set PK
  DBI::dbExecute(con_db, "ALTER TABLE [test_database_dm].[dm_test].[test_1] ALTER COLUMN [b] INTEGER NOT NULL")
  DBI::dbExecute(con_db, "ALTER TABLE [test_database_dm].[dm_test].[test_1] ADD PRIMARY KEY ([b])")
  # set FK relation
  DBI::dbExecute(
    con_db,
    "ALTER TABLE [test_database_dm].[dm_test].[test_2] ADD FOREIGN KEY ([c]) REFERENCES [test_database_dm].[dm_test].[test_1] ([b]) ON DELETE NO ACTION ON UPDATE NO ACTION"
  )


  # delete database after test
  withr::defer({
    try(dbExecute(con_db, "DROP TABLE [test_database_dm].[dm_test].[test_2]"))
    try(dbExecute(con_db, "DROP TABLE [test_database_dm].[dm_test].[test_1]"))
    # dropping schema is unnecessary
    try(dbExecute(con_db, "DROP DATABASE test_database_dm"))
  })

  # test 'get_src_tbl_names()'
  src_tbl_names <- unname(get_src_tbl_names(con_db, schema = "dm_test", dbname = "test_database_dm"))
  expect_identical(
    src_tbl_names,
    sort(DBI::SQL(paste0(
      DBI::dbQuoteIdentifier(con_db, "test_database_dm"), ".",
      DBI::dbQuoteIdentifier(con_db, "dm_test"), ".",
      DBI::dbQuoteIdentifier(con_db, c("test_1", "test_2"))
    )))
  )

  dm_local_no_keys <- dm(
    test_1 = tibble(a = c(5L, 5L, 4L, 2L, 1L), b = 1:5),
    test_2 = tibble(c = c(1L, 1L, 1L, 5L, 4L), d = c(10L, 11L, 10L, 10L, 11L))
  )

  expect_message(dm_db_learned <- dm_from_con(con_db, schema = "dm_test", dbname = "test_database_dm"))
  dm_learned <- dm_db_learned %>% collect()
  expect_equivalent_dm(
    dm_learned[c("test_1", "test_2")],
    dm_local_no_keys %>%
      dm_add_pk(test_1, b) %>%
      dm_add_fk(test_2, c, test_1)
  )

  # learning without keys:
  dm_learned_no_keys <- expect_silent(
    dm_from_con(
      con_db,
      schema = "dm_test",
      dbname = "test_database_dm",
      learn_keys = FALSE
    ) %>%
      collect()
  )
  expect_equivalent_dm(
    dm_learned_no_keys[c("test_1", "test_2")],
    dm_local_no_keys
  )
})

# Must be in the same file as the other learning tests
# to avoid race conditions.
test_that("dm_meta() contents", {
  skip_if_src_not(c("mssql", "postgres", "maria"))

  # produces a randomized schema name with a length of 4-10 characters
  # consisting of the symbols in `reservoir`
  random_schema <- function() {
    reservoir <- c(letters)
    how_long <- 10
    paste0(reservoir[sample(seq_len(length(reservoir)), how_long, replace = TRUE)], collapse = "")
  }

  schema_name <- random_schema()

  # dm_learn_from_mssql() --------------------------------------------------
  con_db <- my_test_con()

  schema_name_q <- DBI::dbQuoteIdentifier(con_db, schema_name)

  DBI::dbExecute(con_db, paste0("CREATE SCHEMA ", schema_name_q))

  # create an object on the MSSQL-DB that can be learned
  withr::defer({
    try(walk(
      order_of_deletion,
      ~ try(dbExecute(con_db, paste0("DROP TABLE ", schema_name_q, ".", .x, " CASCADE")))
    ))
    try(dbExecute(con_db, paste0("DROP SCHEMA ", schema_name_q)))
  })

  dm_for_filter_copied <- copy_dm_to(con_db, dm_for_filter(), temporary = FALSE, schema = schema_name)
  order_of_deletion <- c("tf_2", "tf_1", "tf_5", "tf_6", "tf_4", "tf_3")

  meta <- dm_meta(con_db, schema = schema_name)

  constraints <- dm_examine_constraints(meta, progress = FALSE)
  expect_true(all(constraints$is_key))

  arrange_all_but_constraint_name <- function(.x) {
    names <- setdiff(colnames(.x), "constraint_name")
    .x %>%
      arrange(!!!syms(names))
  }

  expect_snapshot({
    meta %>%
      dm_select_tbl(-schemata) %>%
      dm_zoom_to(table_constraints) %>%
      filter(constraint_type %in% c("PRIMARY KEY", "FOREIGN KEY")) %>%
      dm_update_zoomed() %>%
      dm_get_tables() %>%
      map(select, -any_of("column_default"), -contains("catalog"), -contains("schema")) %>%
      map(arrange_all_but_constraint_name) %>%
      map(collect) %>%
      map(~ if ("constraint_name" %in% colnames(.x)) {
        .x %>% mutate(constraint_name = as.integer(forcats::fct_inorder(constraint_name)))
      } else {
        .x
      }) %>%
      map(arrange_all) %>%
      jsonlite::toJSON(pretty = TRUE) %>%
      gsub(schema_name, "schema_name", .) %>%
      gsub('(_catalog": ")[^"]*(")', "\\1catalog\\2", .) %>%
      writeLines()
  })
})

test_that("dm_from_con() with mariaDB", {
  skip_if_offline()
  my_db <- RMariaDB::dbConnect(
    RMariaDB::MariaDB(),
    username = "guest",
    password = "relational",
    dbname = "Financial_ijs",
    host = "relational.fit.cvut.cz"
  )
  expect_snapshot_output(my_dm <- dm_from_con(my_db))
  expect_snapshot(dm::dm_get_all_fks(my_dm))
  expect_snapshot(dm::dm_get_all_pks(my_dm))
})
