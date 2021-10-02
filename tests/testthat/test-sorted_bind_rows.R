
test_that("sorted_bind functions", {

  x <- tibble::tibble(
    id = rnorm(n = 100),
    other = stringr::str_glue("anything-{rnorm(n = 100)}")
  ) %>%
    sorted_tibble(
      key = id
    )

  y <- tibble::tibble(
    id = rnorm(n = 4),
    other = stringr::str_glue("anything-{rnorm(n = 4)}")
  )

  ans <- sorted_bind_rows(
    sorted_tibble = x,
    binding_tibble = y
  )

  expect_equal(is.unsorted(ans$id), FALSE)

  expect_equal(attr(ans, "key"), "id")

  expect_equal(is(ans, "sorted_tbl"), TRUE)



})




