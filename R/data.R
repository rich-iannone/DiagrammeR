#' ISO-4217 currency data.
#'
#' A dataset containing currency information from the ISO-4217 standard.
#'
#' @format A data frame with 171 rows and 4 variables:
#' \describe{
#'   \item{iso_4217_code}{the three-letter currency code according to the ISO-4217 standard}
#'   \item{curr_number}{the three-digit code number assigned to each currency under the ISO-4217 standard}
#'   \item{exponent}{the base 10 exponent of the minor currency unit in relation to the major currency unit (it can be assumed also to be number of decimal places that is commonly considered for the currency)}
#'   \item{currency_name}{the English name of the currency}
#' }
#' @source <https://en.wikipedia.org/wiki/ISO_4217>
"currencies"

#' US Dollar exchange rates.
#'
#' A dataset containing exchange rates from USD to all other currencies.
#'
#' @format A data frame with 196 rows and 3 variables:
#' \describe{
#'   \item{from_currency}{the currency from which units will be used to buy units in the alternate currency (this is always USD)}
#'   \item{to_currency}{the currency that is to be bought}
#'   \item{cost_unit}{the cost per unit of the currency to be bought}
#' }
"usd_exchange_rates"

#' Node list - Version 1.
#'
#' A very simple, 2-column data frame that can be used to generate graph nodes.
#'
#' @format A data frame with 10 rows and 2 variables:
#' \describe{
#'   \item{id}{a unique, monotonically increasing integer ID value}
#'   \item{label}{a unique label associated with each ID value}
#' }
"node_list_1"

#' Node list - Version 2.
#'
#' A simple, 5-column data frame that can be used to generate graph nodes.
#'
#' @format A data frame with 10 rows and 5 variables:
#' \describe{
#'   \item{id}{a unique, monotonically increasing integer ID value}
#'   \item{label}{a unique label associated with each ID value}
#'   \item{type}{a grouping variable of either x, y, or z}
#'   \item{value_1}{a randomized set of numeric values between 0 and 10}
#'   \item{value_2}{a randomized set of numeric values between 0 and 10}
#' }
"node_list_2"

#' Edge list - Version 1.
#'
#' A very simple, 2-column data frame that can be used to generate graph edges.
#'
#' @format A data frame with 19 rows and 2 variables:
#' \describe{
#'   \item{from}{integer values that state the node ID values where an edge starts}
#'   \item{to}{integer values that state the node ID values where an edge terminates}
#' }
"edge_list_1"

#' Edge list - Version 2.
#'
#' A simple, 5-column data frame that can be used to generate graph edges.
#'
#' @format A data frame with 19 rows and 5 variables:
#' \describe{
#'   \item{from}{integer values that state the node ID values where an edge starts}
#'   \item{to}{integer values that state the node ID values where an edge terminates}
#'   \item{rel}{a grouping variable of either a, b, or c}
#'   \item{value_1}{a randomized set of numeric values between 0 and 10}
#'   \item{value_2}{a randomized set of numeric values between 0 and 10}
#' }
"edge_list_2"
