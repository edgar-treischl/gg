#' Make a pie chart
#'
#' @param data A character vector with one element.
#' @param slices What to split on.
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' ggpie(palmerpenguins::penguins, slices = island)

ggpie <- function(data, slices) {
  data |>
    dplyr::group_by({{slices}}) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::mutate(sum = sum(n),
                  freq = n/sum * 100) |>
    ggplot2::ggplot(ggplot2::aes(x="", y=freq, fill={{slices}}))+
    ggplot2::geom_col(width = 1)+
    ggplot2::coord_polar(theta = "y")+
    ggplot2::theme_void()

}


utils::globalVariables(c("freq", "n"))
