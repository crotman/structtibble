#' creates a sorted tibble
#'
#' @return
#' @export
#'
#' @examples
sorted_tibble <- function(x, key){

  ans <- x %>%
    dplyr::arrange({{ key }})

  attr(ans, "key") <- ans %>% dplyr::select({{ key }}) %>% names()

  class(ans) <- c("sorted_tbl", class(ans))

  ans

}









