test_that("path argument", {
  skip_if_not_installed("brio")

  path <- tempfile()
  dm() %>% dm_paste(path = path)
  expect_identical(readLines(path), c("dm::dm(", ")"))
})

local_options(lifecycle_verbosity = "warning")

test_that("output", {
  skip_if_not_installed("nycflights13")

  expect_snapshot({
    "empty"
    empty_dm() %>% dm_paste()

    "empty table"
    dm(a = tibble()) %>% dm_paste(options = "tables")

    "baseline"
    dm_for_filter() %>% dm_paste()

    "changing the tab width"
    dm_for_filter() %>% dm_paste(tab_width = 4)

    "we don't care if the tables really exist"
    dm_for_filter() %>%
      dm_rename_tbl(tf_1_new = tf_1) %>%
      dm_paste()

    "produce `dm_select()` statements in addition to the rest"
    dm_for_filter() %>%
      dm_select(tf_5, k = k, m) %>%
      dm_select(tf_1, a) %>%
      dm_add_tbl(x = copy_to_my_test_src(tibble(q = 1L), qq)) %>%
      dm_paste(options = "select")

    "produce code with colors"
    dm_for_filter() %>%
      dm_set_colors("orange" = tf_1:tf_3, "darkgreen" = tf_5:tf_6) %>%
      dm_paste()

    "tick if needed"
    a <- tibble(x = 1)
    names(a) <- "a b"
    dm(a) %>%
      dm_zoom_to(a) %>%
      dm_insert_zoomed("a b") %>%
      dm_add_pk(a, "a b") %>%
      dm_add_fk("a b", "a b", a) %>%
      dm_set_colors(green = "a b") %>%
      dm_paste(options = "all")

    "FK referencing non default PK"
    b <- tibble(x = 1, y = "A", z = "A")
    c <- tibble(x = "A", y = "A")

    dm(b, c) %>%
      dm_add_pk(c, x) %>%
      dm_add_fk(b, y, c) %>%
      dm_add_fk(b, z, c, y) %>%
      dm_paste()

    "on_delete if needed"
    dm(b, c) %>%
      dm_add_pk(c, x) %>%
      dm_add_fk(b, y, c, on_delete = "cascade") %>%
      dm_add_fk(b, z, c, y, on_delete = "no_action") %>%
      dm_paste()

    "all of nycflights13"
    dm_nycflights13() %>%
      dm_paste(options = "all")

    "deprecation warning for select argument"
    dm() %>%
      dm_paste(select = TRUE)

    "error for bad option"
    writeLines(conditionMessage(
      expect_error(dm_paste(dm(), options = c("bogus", "all", "mad")))
    ))
  })
})
