
#' Title
#'
#' @param sorted_tibble
#' @param binding_tibble
#'
#' @return
#' @export
#'
#' @examples
sorted_bind_rows <- function(sorted_tibble, binding_tibble ){

  if(!is(sorted_tibble,"sorted_tbl")){
    stop("sorted_tibble is not sorted")
  }

  key <- attr(sorted_tibble, "key")
  id <- attr(sorted_tibble, "autonumbered_id")
  max_id <- attr(sorted_tibble, "max_autonumbered_id")


  if(!key %in% names(binding_tibble)){
    stop("binding_tibble does not contain key")
  }

  answer <- dplyr::bind_rows(
    sorted_tibble,
    binding_tibble %>%
      dplyr::arrange() %>%
      dplyr::mutate(
        {{id}} := attr(sorted_tibble, "max_autonumbered_id") + dplyr::row_number() )
  )

  answer <- answer %>% dplyr::arrange(.data[[key]])


  attr(answer, "key") <- key
  attr(answer, "max_autonumbered_id") <- attr(answer, "max_autonumbered_id") + nrow(binding_tibble)

  class(answer) <- c("sorted_tbl", class(answer))

  answer

}


