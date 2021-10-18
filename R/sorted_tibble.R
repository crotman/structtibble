#' creates a sorted tibble
#'
#' @return
#' @export
#'
#' @examples
sorted_tibble <- function(x, key, autonumbered_id ){

  #TODO: verify if autonumbered_id exists

  ans <- x %>%
    dplyr::arrange({{ key }}) %>%
    dplyr::mutate({{autonumbered_id}} := dplyr::row_number())


  attr(ans, "key") <- ans %>% dplyr::select({{ key }}) %>% names()

  attr(ans, "autonumbered_id") <- ans %>% dplyr::select({{ autonumbered_id }}) %>% names()

  attr(ans, "max_autonumbered_id") <- ans %>% dplyr::pull({{ autonumbered_id  }}) %>% max(c(., 0))

  class(ans) <- c("sorted_tbl", class(ans))

  ans

}




