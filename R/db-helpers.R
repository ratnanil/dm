unique_db_table_name <- local({
  i <- 0

  function(table_name) {
    i <<- i + 1
    glue("{table_name}_", as.character(i), "_", systime_convenient(), "_", get_pid())
  }
})

systime_convenient <- function() {
  # FIXME: Race condition here, but fast enough
  local_options(digits.secs = 6)

  if (Sys.getenv("IN_PKGDOWN") != "") {
    "2020_08_28_07_13_03"
  } else {
    time <- as.character(Sys.time())
    gsub("[-:. ]", "_", time)
  }
}

get_pid <- function() {
  if (Sys.getenv("IN_PKGDOWN") != "") {
    "12345"
  } else {
    as.character(Sys.getpid())
  }
}

class_to_db_class <- function(dest, class_vector) {
  if (is_mssql(dest) || is_postgres(dest)) {
    case_when(
      class_vector == "character" ~ "VARCHAR(100)",
      class_vector == "integer" ~ "INT",
      TRUE ~ class_vector
    )
  } else {
    return(class_vector)
  }
}

is_db <- function(x) {
  inherits(x, "src_sql")
}

is_src_db <- function(dm) {
  is_db(dm_get_src_impl(dm))
}

is_duckdb <- function(dest) {
  inherits(dest, c("duckdb_connection", "src_duckdb_connection"))
}

is_mssql <- function(dest) {
  inherits(dest, c(
    "Microsoft SQL Server", "src_Microsoft SQL Server", "dblogConnection-Microsoft SQL Server", "src_dblogConnection-Microsoft SQL Server"
  ))
}

is_postgres <- function(dest) {
  inherits(dest, "src_PostgreSQLConnection") ||
    inherits(dest, "src_PqConnection") ||
    inherits(dest, "PostgreSQLConnection") ||
    inherits(dest, "PqConnection")
}

is_mariadb <- function(dest) {
  inherits_any(dest, c("MariaDBConnection", "src_MariaDBConnection", "src_DoltConnection", "src_DoltLocalConnection"))
}

src_from_src_or_con <- function(dest) {
  if (is.src(dest)) dest else dbplyr::src_dbi(dest)
}

con_from_src_or_con <- function(dest) {
  if (is.src(dest)) dest$con else dest
}

repair_table_names_for_db <- function(table_names, temporary, con, schema = NULL) {
  if (temporary) {
    if (!is.null(schema)) {
      abort_temporary_not_in_schema()
    }
    # FIXME: Better logic for temporary table names
    if (is_mssql(con)) {
      names <- paste0("#", table_names)
    } else {
      names <- table_names
    }
    names <- unique_db_table_name(names)
  } else {
    # permanent tables
    if (!is.null(schema) && !is_mssql(con) && !is_postgres(con) && !is_mariadb(con)) {
      abort_no_schemas_supported(con = con)
    }
    names <- table_names
  }
  names <- set_names(names, table_names)
  quote_ids(names, con_from_src_or_con(con), schema)
}

get_src_tbl_names <- function(src, schema = NULL, dbname = NULL) {
  if (!is_mssql(src) && !is_postgres(src) && !is_mariadb(src) && !is_duckdb(src)) {
    warn_if_arg_not(schema, only_on = c("MSSQL", "Postgres", "MariaDB", "DuckDB"))
    warn_if_arg_not(dbname, only_on = "MSSQL")
    return(set_names(src_tbls(src)))
  }

  con <- con_from_src_or_con(src)

  if (!is.null(schema)) {
    check_param_class(schema, "character")
    check_param_length(schema)
  }

  meta <- dm_meta(con, catalog = dbname, schema = schema, simple = TRUE)

  meta$tables %>%
    collect() %>%
    # create remote names for the tables in the given schema (name is table_name; cannot be duplicated within a single schema)
    transmute(table_name, remote_name = schema_if(table_schema, table_name, con, dbname)) %>%
    deframe()
}

# `schema_*()` : default schema if NULL, otherwise unchanged
schema_mssql <- function(con, schema) {
  if (is_null(schema)) {
    schema <- "dbo"
  }
  schema
}

schema_postgres <- function(con, schema) {
  if (is_null(schema)) {
    schema <- "public"
  }
  schema
}

schema_mariadb <- function(con, schema) {
  if (is_null(schema)) {
    schema <- sql("database()")
  }
  schema
}

dbname_mssql <- function(con, dbname) {
  if (is_null(dbname)) {
    dbname <- ""
    dbname_sql <- ""
  } else {
    check_param_class(dbname, "character")
    dbname_sql <- paste0(DBI::dbQuoteIdentifier(con, dbname), ".")
  }
  set_names(dbname_sql, dbname)
}
