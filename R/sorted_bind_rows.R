
sorted_bind_rows <- function(sorted_tibble, binding_tibble ){

  if(!is(sorted_tibble,"sorted_tbl")){
    stop("sorted_tibble is not sorted")
  }

  key <- attr(sorted_tibble, "key")

  if(!key %in% names(binding_tibble)){
    stop("binding_tibble does not contain key")
  }

  answer <- dplyr::bind_rows(
    sorted_tibble,
    binding_tibble
  )

  answer <- answer %>% dplyr::arrange(.data[[key]])

  attr(answer, "key") <- key

  class(answer) <- c("sorted_tbl", class(answer))

  answer

}


