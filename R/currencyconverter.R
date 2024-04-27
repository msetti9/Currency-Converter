#' Currency conversion function.
#'
#' @param amount The amount(s) to convert.
#' @param from_currency Currency/currencies to convert from.
#' @param to_currency Currency/currencies to convert to.
#' @param percentage_change Percentage change(s) in currency rate.
#' @param rounding_option Rounding option for converted amount.
#' @return Converted amount(s).
#' @export
currency_conversion <- function(amount, from_currency, to_currency, percentage_change = 0, rounding_option = "none") {
  class <- "currency_conversion"
  structure(
    list(
      amount = amount,
      from_currency = from_currency,
      to_currency = to_currency,
      percentage_change = percentage_change,
      rounding_option = rounding_option
    ),
    class = class
  )
}

#' Perform currency conversion.
#'
#' @param conversion_obj Currency conversion object.
#' @return Converted amount(s).
convertcurrency <- function(conversion_obj) {
  conversion_rates <- list(
    USD = 1,
    INR = 83.38,
    EUR = 0.93,
    PESO = 16.46
    )

  from_currency <- conversion_obj$from_currency
  to_currency <- conversion_obj$to_currency
  amount <- conversion_obj$amount
  percentage_change <- conversion_obj$percentage_change
  rounding_option <- conversion_obj$rounding_option

  # Convert currency names to uppercase
  from_currency <- toupper(from_currency)
  to_currency <- toupper(to_currency)

  # Check if currencies are available for conversion
  if (!(from_currency %in% names(conversion_rates)) || !(to_currency %in% names(conversion_rates))) {
    stop("Currency not available for conversion.")
  }

  # If percentage_change is a single value, convert it to a vector matching the length of from_currency and to_currency
  if (!is.vector(percentage_change)) {
    percentage_change <- rep(percentage_change, length(from_currency))
  }

  # Initialize vector to store converted amounts
  converted_amounts <- numeric(length(amount))

  # Iterate through each conversion
  for (i in seq_along(amount)) {
    conversion_rate <- conversion_rates[[to_currency[i]]] / conversion_rates[[from_currency[i]]]
    adjusted_rate <- conversion_rate * (1 + (percentage_change[i] / 100))

    # Perform conversionRound the converted amount based on rounding_option
    converted_amounts[i] <- amount[i] * adjusted_rate

    # Round the converted amount based on rounding_option
    if (rounding_option == "up") {
      converted_amounts[i] <- ceiling(converted_amounts[i])
    } else if (rounding_option == "down") {
      converted_amounts[i] <- floor(converted_amounts[i])
    } else if (rounding_option == "nearest") {
      converted_amounts[i] <- round(converted_amounts[i])
    }
  }
  return(converted_amounts)
}





