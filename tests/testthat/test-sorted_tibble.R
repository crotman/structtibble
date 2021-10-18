test_that("sorted_tibble works", {

  test <- tibble::tibble(
    id_1 = rnorm(n = 100)
  )

  ans <- sorted_tibble(test, key = id_1, autonumbered_id = id )

  expect_equal(is.unsorted(ans$id_1), FALSE)

  expect_equal(attr(ans, "key"), "id_1")

  expect_equal(is(ans, "sorted_tbl"), TRUE)

  expect_equal( max(ans$id), attr(ans, "max_autonumbered_id"))

})


test_that("sorted_tibble with empty works", {

  teste <- sorted_tibble(
    x = tibble::tibble(a = integer(0)),
    key = a,
    autonumbered_id = id
  )

  expect_equal( attr(teste, "max_autonumbered_id"), 0 )

})



