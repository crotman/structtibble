

test_that("pop", {



  pop_tibble <- tibble::tibble(
    a = c(1, 2, 3),
    b = c("a", "b", "c")
  )

  popped_head_tibble <- tibble::tibble(
    a = c(2, 3),
    b = c("b", "c")
  )

  popped_head_element <- tibble::tibble(
    a = c(1),
    b = c("a")
  )

  popped_tail_tibble <- tibble::tibble(
    a = c(1, 2),
    b = c("a", "b")
  )

  popped_tail_element <- tibble::tibble(
    a = c(3),
    b = c("c")
  )


  test_pop_head <- pop_tibble(
    x = pop_tibble
  )


  test_pop_tail <- pop_tibble(
    x = pop_tibble,
    side = "tail"
  )

  expect_equal(
    object = test_pop_tail$x,
    expected = popped_tail_element
  )

  expect_equal(
    object = test_pop_tail$tibble,
    expected = popped_tail_tibble
  )


  expect_equal(
    object = test_pop_tail$x,
    expected = popped_tail_element
  )

  expect_equal(
    object = test_pop_tail$tibble,
    expected = popped_tail_tibble
  )



})



test_that("pop", {

  x <- tibble::tibble(
    a = character(0)
  )

  popped <- pop_tibble(
    x
  )

  expect_null(popped)

})


test_that("pop", {

  x <- tibble::tibble(
    a = 1
  )

  empty <- tibble::tibble(
    a = numeric(0)
  )

  popped <- pop_tibble(
    x
  )

  expect_equal(popped$tibble, empty)

})










