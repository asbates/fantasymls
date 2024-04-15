
box::use(
  scales[dollar]
) 

#' @export
prettify_cost <- function(cost) {
  dollar(cost, accuracy = 0.1, scale = 0.000001, suffix = "M") 
}
