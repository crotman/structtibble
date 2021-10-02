test_that("multiplication works", {

  test <- tibble::tibble(
    id_1 = rnorm(n = 100)
  )

  ans <- sorted_tibble(test, key = id_1 )

  expect_equal(is.unsorted(ans$id_1), FALSE)

  expect_equal(attr(ans, "key"), "id_1")

  expect_equal(is(ans, "sorted_tbl"), TRUE)

})




