

#' Title
#'
#' @param x
#' @param side
#'
#' @return
#' @export
#'
#' @examples
pop_tibble <- function(x, side = "head"){

  if(nrow(x) == 0){
    return(NULL)
  }

  slice_element <- switch(
    side,
    "head" = dplyr::slice_head,
    "tail" = dplyr::slice_tail
  )


  slice_tibble <- switch(
    side,
    "tail" = dplyr::slice_head,
    "head" = dplyr::slice_tail
  )

  element <- x %>%
    slice_element(n = 1)

  tibble <- x %>%
    slice_tibble(n = nrow(.)-1)

  list(
    x = element,
    tibble = tibble
  )

}
