# ISO-4217 currency data.

A dataset containing currency information from the ISO-4217 standard.

## Usage

``` r
currencies
```

## Format

A data frame with 171 rows and 4 variables:

- iso_4217_code:

  the three-letter currency code according to the ISO-4217 standard

- curr_number:

  the three-digit code number assigned to each currency under the
  ISO-4217 standard

- exponent:

  the base 10 exponent of the minor currency unit in relation to the
  major currency unit (it can be assumed also to be number of decimal
  places that is commonly considered for the currency)

- currency_name:

  the English name of the currency

## Source

<https://en.wikipedia.org/wiki/ISO_4217>
