test_that("`json_nest()` and `json_unnest()` work", {

  expect_snapshot({
    df <- tibble::tibble(x = c(1, 1, 1, 2, 2, 3), y = 1:6, z = 6:1)
    nested <- json_nest(df, data = c(y, z))
    nested
  })

  df_roundtrip <- json_unnest(nested, data)
  df_roundtrip
  expect_equal(df, df_roundtrip)
})
