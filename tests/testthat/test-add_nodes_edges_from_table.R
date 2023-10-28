# Adding nodes and/or edges from a table to a graph

test_that("adding nodes from a table to a graph is possible", {

  # Create a data frame for graph nodes
  node_table <-
    dplyr::tribble(
      ~iso_4217_code, ~curr_number, ~exponent,
      "AED", 784,  2,
      "AFN", 971,  2,
      "ALL",   8,  2,
      "AMD",  51,  2,
      "ANG", 532,  2,
      "AOA", 973,  2,
      "ARS",  32,  2,
      "AUD",  36,  2,
      "AWG", 533,  2,
      "AZN", 944,  2,
      "BAM", 977,  2,
      "BBD",  52,  2,
      "BDT",  50,  2,
      "BGN", 975,  2,
      "BHD",  48,  3,
      "BIF", 108,  0,
      "BMD",  60,  2,
      "BND",  96,  2,
      "BOB",  68,  2,
      "BOV", 984,  2,
      "BRL", 986,  2,
      "BSD",  44,  2,
      "BTN",  64,  2,
      "BWP",  72,  2,
      "BYR", 974,  0,
      "BZD",  84,  2,
      "CAD", 124,  2,
      "CDF", 976,  2,
      "CHE", 947,  2,
      "CHF", 756,  2,
      "CHW", 948,  2,
      "CLF", 990,  4,
      "CLP", 152,  0,
      "CNY", 156,  2,
      "COP", 170,  2,
      "COU", 970,  2,
      "CRC", 188,  2,
      "CUC", 931,  2,
      "CUP", 192,  2,
      "CVE", 132,  0,
      "CZK", 203,  2,
      "DJF", 262,  0,
      "DKK", 208,  2,
      "DOP", 214,  2,
      "DZD",  12,  2,
      "EGP", 818,  2,
      "ERN", 232,  2,
      "ETB", 230,  2,
      "EUR", 978,  2,
      "FJD", 242,  2,
      "FKP", 238,  2,
      "GBP", 826,  2,
      "GEL", 981,  2,
      "GHS", 936,  2,
      "GIP", 292,  2,
      "GMD", 270,  2,
      "GNF", 324,  0,
      "GTQ", 320,  2,
      "GYD", 328,  2,
      "HKD", 344,  2,
      "HNL", 340,  2,
      "HRK", 191,  2,
      "HTG", 332,  2,
      "HUF", 348,  2,
      "IDR", 360,  2,
      "ILS", 376,  2,
      "INR", 356,  2,
      "IQD", 368,  3,
      "IRR", 364,  2,
      "ISK", 352,  0,
      "JMD", 388,  2,
      "JOD", 400,  3,
      "JPY", 392,  0,
      "KES", 404,  2,
      "KGS", 417,  2,
      "KHR", 116,  2,
      "KMF", 174,  0,
      "KPW", 408,  2,
      "KRW", 410,  0,
      "KWD", 414,  3,
      "KYD", 136,  2,
      "KZT", 398,  2,
      "LAK", 418,  2,
      "LBP", 422,  2,
      "LKR", 144,  2,
      "LRD", 430,  2,
      "LSL", 426,  2,
      "LYD", 434,  3,
      "MAD", 504,  2,
      "MDL", 498,  2,
      "MGA", 969,  1,
      "MKD", 807,  2,
      "MMK", 104,  2,
      "MNT", 496,  2,
      "MOP", 446,  1,
      "MRO", 478,  1,
      "MUR", 480,  2,
      "MVR", 462,  2,
      "MWK", 454,  2,
      "MXN", 484,  2,
      "MXV", 979,  2,
      "MYR", 458,  2,
      "MZN", 943,  2,
      "NAD", 516,  2,
      "NGN", 566,  2,
      "NIO", 558,  2,
      "NOK", 578,  2,
      "NPR", 524,  2,
      "NZD", 554,  2,
      "OMR", 512,  3,
      "PAB", 590,  2,
      "PEN", 604,  2,
      "PGK", 598,  2,
      "PHP", 608,  2,
      "PKR", 586,  2,
      "PLN", 985,  2,
      "PYG", 600,  0,
      "QAR", 634,  2,
      "RON", 946,  2,
      "RSD", 941,  2,
      "RUB", 643,  2,
      "RWF", 646,  0,
      "SAR", 682,  2,
      "SBD",  90,  2,
      "SCR", 690,  2,
      "SDG", 938,  2,
      "SEK", 752,  2,
      "SGD", 702,  2,
      "SHP", 654,  2,
      "SLL", 694,  2,
      "SOS", 706,  2,
      "SRD", 968,  2,
      "SSP", 728,  2,
      "STD", 678,  2,
      "SYP", 760,  2,
      "SZL", 748,  2,
      "THB", 764,  2,
      "TJS", 972,  2,
      "TMT", 934,  2,
      "TND", 788,  3,
      "TOP", 776,  2,
      "TRY", 949,  2,
      "TTD", 780,  2,
      "TWD", 901,  2,
      "TZS", 834,  2,
      "UAH", 980,  2,
      "UGX", 800,  0,
      "USD", 840,  2,
      "USN", 997,  2,
      "USS", 998,  2,
      "UYI", 940,  0,
      "UYU", 858,  2,
      "UZS", 860,  2,
      "VEF", 937,  2,
      "VND", 704,  0,
      "VUV", 548,  0,
      "WST", 882,  2,
      "XAF", 950,  0,
      "XAG", 961, NA,
      "XAU", 959, NA,
      "XCD", 951,  2,
      "XDR", 960, NA,
      "XOF", 952,  0,
      "XPD", 964, NA,
      "XPF", 953,  0,
      "XPT", 962, NA,
      "XSU", 994, NA,
      "XUA", 965, NA,
      "YER", 886,  2,
      "ZAR", 710,  2,
      "ZMW", 967,  2)

  # Convert to a data frame
  node_table <-
    as.data.frame(node_table, stringsAsFactors = FALSE)

  # Add nodes directly from the df, calling the
  # `add_nodes_from_table()` function with default
  # options
  graph_1_df <-
    create_graph() %>%
    add_nodes_from_table(table = node_table)

  # Expect that the graph has the same number of nodes
  # as there are rows in the CSV
  expect_equal(
    nrow(node_table), count_nodes(graph = graph_1_df))

  # Expect certain columns to exist in the graph's
  # node data frame
  expect_equal(
    colnames(graph_1_df$nodes_df),
    c("id", "type", "label", "iso_4217_code",
      "curr_number", "exponent"))

  # Add nodes from the df, but this time apply the
  # `curr_number` column to the graph's `label` attribute
  graph_2_df <-
    create_graph() %>%
    add_nodes_from_table(
      table = node_table,
      label_col = curr_number)

  # Expect that there aren't any NA values in the
  # graph's `label` column
  expect_false(
    anyNA(graph_2_df$nodes_df[, 3]))

  # Expect that the values in the `label` are
  # of the character class
  expect_type(graph_2_df$nodes_df[, 3], "character")

  # Add nodes from the df; also apply a static value
  # for `type` as `currency`
  graph_3_df <-
    create_graph() %>%
    add_nodes_from_table(
      table = node_table,
      set_type = "currency",
      label_col = curr_number)

  # Expect that all values set for the `type`
  # attribute are `currency`
  expect_in(graph_3_df$nodes_df[, 2], "currency")

  # Add nodes from the df; drop some of the
  # incoming columns
  graph_4_df <-
    create_graph() %>%
    add_nodes_from_table(
      table = node_table,
      set_type = "currency",
      label_col = curr_number,
      drop_cols = exponent & currency_name)

  # Expect that the node attributes `exponent`
  # and `currency_name` do not appear in the graph's
  # internal node data frame
  expect_false(
    all(c("exponent", "currency_name") %in%
           colnames(graph_4_df$nodes_df)))

  # Add nodes from the df; assign a table column
  # to the `type` atttribute
  graph_5_df <-
    create_graph() %>%
    add_nodes_from_table(
      table = node_table,
      set_type = "currency",
      label_col = curr_number,
      type_col = exponent)

  # Expect that all values set for the `type`
  # attribute are have certain values (including NA)
  expect_in(
    graph_5_df$nodes_df[, 2],
    c("0", "1", "2", "3", "4", NA))
})

test_that("adding edges from a table to a graph is possible", {

  # Create a data frame for graph nodes
  node_table <-
    dplyr::tribble(
      ~iso_4217_code, ~curr_number, ~exponent,
      "AED", 784,  2,
      "AFN", 971,  2,
      "ALL",   8,  2,
      "AMD",  51,  2,
      "ANG", 532,  2,
      "AOA", 973,  2,
      "ARS",  32,  2,
      "AUD",  36,  2,
      "AWG", 533,  2,
      "AZN", 944,  2,
      "BAM", 977,  2,
      "BBD",  52,  2,
      "BDT",  50,  2,
      "BGN", 975,  2,
      "BHD",  48,  3,
      "BIF", 108,  0,
      "BMD",  60,  2,
      "BND",  96,  2,
      "BOB",  68,  2,
      "BOV", 984,  2,
      "BRL", 986,  2,
      "BSD",  44,  2,
      "BTN",  64,  2,
      "BWP",  72,  2,
      "BYR", 974,  0,
      "BZD",  84,  2,
      "CAD", 124,  2,
      "CDF", 976,  2,
      "CHE", 947,  2,
      "CHF", 756,  2,
      "CHW", 948,  2,
      "CLF", 990,  4,
      "CLP", 152,  0,
      "CNY", 156,  2,
      "COP", 170,  2,
      "COU", 970,  2,
      "CRC", 188,  2,
      "CUC", 931,  2,
      "CUP", 192,  2,
      "CVE", 132,  0,
      "CZK", 203,  2,
      "DJF", 262,  0,
      "DKK", 208,  2,
      "DOP", 214,  2,
      "DZD",  12,  2,
      "EGP", 818,  2,
      "ERN", 232,  2,
      "ETB", 230,  2,
      "EUR", 978,  2,
      "FJD", 242,  2,
      "FKP", 238,  2,
      "GBP", 826,  2,
      "GEL", 981,  2,
      "GHS", 936,  2,
      "GIP", 292,  2,
      "GMD", 270,  2,
      "GNF", 324,  0,
      "GTQ", 320,  2,
      "GYD", 328,  2,
      "HKD", 344,  2,
      "HNL", 340,  2,
      "HRK", 191,  2,
      "HTG", 332,  2,
      "HUF", 348,  2,
      "IDR", 360,  2,
      "ILS", 376,  2,
      "INR", 356,  2,
      "IQD", 368,  3,
      "IRR", 364,  2,
      "ISK", 352,  0,
      "JMD", 388,  2,
      "JOD", 400,  3,
      "JPY", 392,  0,
      "KES", 404,  2,
      "KGS", 417,  2,
      "KHR", 116,  2,
      "KMF", 174,  0,
      "KPW", 408,  2,
      "KRW", 410,  0,
      "KWD", 414,  3,
      "KYD", 136,  2,
      "KZT", 398,  2,
      "LAK", 418,  2,
      "LBP", 422,  2,
      "LKR", 144,  2,
      "LRD", 430,  2,
      "LSL", 426,  2,
      "LYD", 434,  3,
      "MAD", 504,  2,
      "MDL", 498,  2,
      "MGA", 969,  1,
      "MKD", 807,  2,
      "MMK", 104,  2,
      "MNT", 496,  2,
      "MOP", 446,  1,
      "MRO", 478,  1,
      "MUR", 480,  2,
      "MVR", 462,  2,
      "MWK", 454,  2,
      "MXN", 484,  2,
      "MXV", 979,  2,
      "MYR", 458,  2,
      "MZN", 943,  2,
      "NAD", 516,  2,
      "NGN", 566,  2,
      "NIO", 558,  2,
      "NOK", 578,  2,
      "NPR", 524,  2,
      "NZD", 554,  2,
      "OMR", 512,  3,
      "PAB", 590,  2,
      "PEN", 604,  2,
      "PGK", 598,  2,
      "PHP", 608,  2,
      "PKR", 586,  2,
      "PLN", 985,  2,
      "PYG", 600,  0,
      "QAR", 634,  2,
      "RON", 946,  2,
      "RSD", 941,  2,
      "RUB", 643,  2,
      "RWF", 646,  0,
      "SAR", 682,  2,
      "SBD",  90,  2,
      "SCR", 690,  2,
      "SDG", 938,  2,
      "SEK", 752,  2,
      "SGD", 702,  2,
      "SHP", 654,  2,
      "SLL", 694,  2,
      "SOS", 706,  2,
      "SRD", 968,  2,
      "SSP", 728,  2,
      "STD", 678,  2,
      "SYP", 760,  2,
      "SZL", 748,  2,
      "THB", 764,  2,
      "TJS", 972,  2,
      "TMT", 934,  2,
      "TND", 788,  3,
      "TOP", 776,  2,
      "TRY", 949,  2,
      "TTD", 780,  2,
      "TWD", 901,  2,
      "TZS", 834,  2,
      "UAH", 980,  2,
      "UGX", 800,  0,
      "USD", 840,  2,
      "USN", 997,  2,
      "USS", 998,  2,
      "UYI", 940,  0,
      "UYU", 858,  2,
      "UZS", 860,  2,
      "VEF", 937,  2,
      "VND", 704,  0,
      "VUV", 548,  0,
      "WST", 882,  2,
      "XAF", 950,  0,
      "XAG", 961, NA,
      "XAU", 959, NA,
      "XCD", 951,  2,
      "XDR", 960, NA,
      "XOF", 952,  0,
      "XPD", 964, NA,
      "XPF", 953,  0,
      "XPT", 962, NA,
      "XSU", 994, NA,
      "XUA", 965, NA,
      "YER", 886,  2,
      "ZAR", 710,  2,
      "ZMW", 967,  2)

  # Convert to a data frame
  node_table <-
    as.data.frame(node_table, stringsAsFactors = FALSE)

  # Create a data frame for graph edges
  edge_table <-
    dplyr::tribble(
      ~from_currency, ~to_currency, ~cost_unit,
      "USD","ADF", 0.1672,
      "USD","ADP", 0.00659,
      "USD","AED", 0.2723,
      "USD","AFN", 0.01521,
      "USD","ALL", 0.008055,
      "USD","AMD", 0.002107,
      "USD","ANG", 0.565,
      "USD","AOA", 0.006058,
      "USD","AON", 0.006058,
      "USD","ARS", 0.06597,
      "USD","ATS", 0.07969,
      "USD","AUD", 0.7604,
      "USD","AWG", 0.5587,
      "USD","AZM", 0.0001221,
      "USD","AZN", 0.6105,
      "USD","BAM", 0.5607,
      "USD","BBD", 0.5,
      "USD","BDT", 0.01277,
      "USD","BEF", 0.02718,
      "USD","BGN", 0.5635,
      "USD","BHD", 2.6699,
      "USD","BIF", 0.000601,
      "USD","BMD", 1,
      "USD","BND", 0.7184,
      "USD","BOB", 0.1456,
      "USD","BRL", 0.3127,
      "USD","BSD", 1,
      "USD","BTN", 0.01496,
      "USD","BWP", 0.097,
      "USD","BYR", 4.995e-05,
      "USD","BZD", 0.5051,
      "USD","CAD", 0.7461,
      "USD","CDF", 0.0009891,
      "USD","CHF", 1.0116,
      "USD","CLP", 0.001533,
      "USD","CNY", 0.1476,
      "USD","COP", 0.0003334,
      "USD","CRC", 0.001827,
      "USD","CUC", 1,
      "USD","CUP", 0.045,
      "USD","CVE", 0.00995,
      "USD","CYP", 1.8735,
      "USD","CZK", 0.04061,
      "USD","DEM", 0.5606,
      "USD","DJF", 0.005654,
      "USD","DKK", 0.1474,
      "USD","DOP", 0.02173,
      "USD","DZD", 0.00912,
      "USD","ECS", 4.15e-05,
      "USD","EEK", 0.07008,
      "USD","EGP", 0.1129,
      "USD","ESP", 0.00659,
      "USD","ETB", 0.04515,
      "USD","EUR", 1.0965,
      "USD","FIM", 0.1844,
      "USD","FJD", 0.4929,
      "USD","FKP", 1.2236,
      "USD","FRF", 0.1672,
      "USD","GBP", 1.2191,
      "USD","GEL", 0.4151,
      "USD","GHC", 2.515e-05,
      "USD","GHS", 0.2515,
      "USD","GIP", 1.2236,
      "USD","GMD", 0.02365,
      "USD","GNF", 0.0001105,
      "USD","GRD", 0.003218,
      "USD","GTQ", 0.1331,
      "USD","GYD", 0.005083,
      "USD","HKD", 0.129,
      "USD","HNL", 0.04444,
      "USD","HRK", 0.1461,
      "USD","HTG", 0.01582,
      "USD","HUF", 0.003549,
      "USD","IDR", 7.74e-05,
      "USD","IEP", 1.3923,
      "USD","ILS", 0.2609,
      "USD","INR", 0.015,
      "USD","IQD", 0.0008729,
      "USD","IRR", 3.328e-05,
      "USD","ISK", 0.008882,
      "USD","ITL", 0.0005663,
      "USD","JMD", 0.007735,
      "USD","JOD", 1.413,
      "USD","JPY", 0.009536,
      "USD","KES", 0.01003,
      "USD","KGS", 0.01456,
      "USD","KHR", 0.0002483,
      "USD","KMF", 0.002281,
      "USD","KPW", 0.007407,
      "USD","KRW", 0.0008803,
      "USD","KWD", 3.3113,
      "USD","KYD", 1.2293,
      "USD","KZT", 0.002981,
      "USD","LAK", 0.0001229,
      "USD","LBP", 0.0006638,
      "USD","LKR", 0.006768,
      "USD","LRD", 0.01111,
      "USD","LSL", 0.073291,
      "USD","LTL", 0.3176,
      "USD","LUF", 0.02718,
      "USD","LVL", 1.5602,
      "USD","LYD", 0.7135,
      "USD","MAD", 0.1015,
      "USD","MDL", 0.05,
      "USD","MGA", 0.0003137,
      "USD","MGF", 0.0001093,
      "USD","MKD", 0.01786,
      "USD","MMK", 0.0007776,
      "USD","MNT", 0.0004239,
      "USD","MOP", 0.1285,
      "USD","MRO", 0.002817,
      "USD","MTL", 2.5541,
      "USD","MUR", 0.028,
      "USD","MVR", 0.06623,
      "USD","MWK", 0.001404,
      "USD","MXN", 0.05281,
      "USD","MYR", 0.2385,
      "USD","MZM", 1.299e-05,
      "USD","MZN", 0.01299,
      "USD","NAD", 0.073291,
      "USD","NGN", 0.003284,
      "USD","NIO", 0.03438,
      "USD","NLG", 0.4976,
      "USD","NOK", 0.1211,
      "USD","NPR", 0.009368,
      "USD","NZD", 0.7152,
      "USD","OMR", 2.5974,
      "USD","PAB", 1,
      "USD","PEN", 0.2974,
      "USD","PGK", 0.3222,
      "USD","PHP", 0.02064,
      "USD","PKR", 0.009545,
      "USD","PLN", 0.2536,
      "USD","PTE", 0.005469,
      "USD","PYG", 0.0001751,
      "USD","QAR", 0.2746,
      "USD","ROL", 2.436e-05,
      "USD","RON", 0.2436,
      "USD","RSD", 0.008938,
      "USD","RUB", 0.01587,
      "USD","RWF", 0.001231,
      "USD","SAR", 0.2668,
      "USD","SBD", 0.1275,
      "USD","SCR", 0.07586,
      "USD","SDD", 0.001576,
      "USD","SDG", 0.1576,
      "USD","SDP", 0.0004423,
      "USD","SEK", 0.1109,
      "USD","SGD", 0.7184,
      "USD","SHP", 1.8744,
      "USD","SIT", 0.004576,
      "USD","SKK", 0.0364,
      "USD","SLL", 0.0001786,
      "USD","SOS", 0.001799,
      "USD","SRD", 0.146,
      "USD","SRG", 0.000146,
      "USD","STD", 4.454e-05,
      "USD","SVC", 0.1147,
      "USD","SYP", 0.004661,
      "USD","SZL", 0.073291,
      "USD","THB", 0.0286,
      "USD","TJS", 0.127,
      "USD","TMM", 5.7e-05,
      "USD","TMT", 0.2857,
      "USD","TND", 0.4441,
      "USD","TOP", 0.4458,
      "USD","TRL", 3.222e-07,
      "USD","TRY", 0.3222,
      "USD","TTD", 0.1497,
      "USD","TWD", 0.03167,
      "USD","TZS", 0.0004587,
      "USD","UAH", 0.03922,
      "USD","UGX", 0.0002929,
      "USD","USD", 1,
      "USD","UYU", 0.03524,
      "USD","UZS", 0.0003225,
      "USD","VEB", 0.0001003,
      "USD","VEF", 0.1003,
      "USD","VND", 4.48e-05,
      "USD","VUV", 0.009526,
      "USD","WST", 0.3951,
      "USD","XAF", 0.001674,
      "USD","XAG", 17.8532,
      "USD","XAU", 1275.81,
      "USD","XCD", 0.372,
      "USD","XEU", 1.0965,
      "USD","XOF", 0.001669,
      "USD","XPD", 624,
      "USD","XPF", 0.009255,
      "USD","XPT", 983.5,
      "USD","YER", 0.004001,
      "USD","YUN", 0.008938,
      "USD","ZAR", 0.07329,
      "USD","ZMK", 0.0001932,
      "USD","ZMW", 0.1037,
      "USD","ZWD", 0.002679)

  # Convert to a data frame
  edge_table <-
    as.data.frame(edge_table, stringsAsFactors = FALSE)

  # Add nodes directly from the CSV file, calling the
  # `add_nodes_from_table()` function with default
  # options
  graph <-
    create_graph() %>%
    add_nodes_from_table(table = node_table)

  # Augment the graph by adding edges from a table
  # with the `add_edges_from_table()` function
  graph_nodes_edges <-
    graph %>%
    add_edges_from_table(
      table = edge_table,
      from_col = from_currency,
      to_col = to_currency,
      from_to_map = iso_4217_code)

  # Expect that the graph has a certain number of edges
  expect_equal(
    count_edges(graph = graph_nodes_edges), 157)

  # Expect certain columns to exist in the graph's
  # edge data frame
  expect_named(
    graph_nodes_edges$edges_df,
    c("id", "from", "to", "rel", "cost_unit"))

  # Expect an error if value for `from_col` or `to_col` or `from_to_map` is
  # not in the table
  expect_snapshot(error = TRUE, {
    graph %>%
      add_edges_from_table(
        edge_table,
        from_col = from,
        to_col = to_currency,
        from_to_map = iso_4217_code)

    graph %>%
      add_edges_from_table(
        edge_table,
        from_col = from_currency,
        to_col = to,
        from_to_map = iso_4217_code)

    graph %>%
      add_edges_from_table(
        edge_table,
        from_col = from_currency,
        to_col = to_currency,
        from_to_map = iso_4217)
  })

  # Augment the graph by first
  # adding edges from a table
  # with `add_edges_from_table()` and
  # then dropping a column
  graph_nodes_edges_drop <-
    graph %>%
    add_edges_from_table(
      table = edge_table,
      from_col = from_currency,
      to_col = to_currency,
      drop_cols = cost_unit,
      from_to_map = iso_4217_code)

  # Expect that the graph has a certain number of edges
  expect_equal(
    count_edges(graph = graph_nodes_edges_drop), 157)

  # Expect certain columns to exist in the graph's
  # edge data frame
  expect_equal(
    colnames(graph_nodes_edges_drop$edges_df),
    c("id", "from", "to", "rel"))

  # Augment the graph by first
  # adding edges from a table
  # with `add_edges_from_table()` and
  # then setting a static `rel` value
  graph_nodes_edges_set_rel <-
    graph %>%
    add_edges_from_table(
      table = edge_table,
      from_col = from_currency,
      to_col = to_currency,
      from_to_map = iso_4217_code,
      set_rel = "change_to")

  # Expect that the graph has a certain number of edges
  expect_equal(
    count_edges(graph = graph_nodes_edges_set_rel), 157)

  # Expect certain columns to exist in the graph's
  # edge data frame
  expect_named(
    graph_nodes_edges_set_rel$edges_df,
    c("id", "from", "to", "rel", "cost_unit"))

  # Expect the same value (repeated down)
  # for the `rel` edge attribute
  expect_in(
    graph_nodes_edges_set_rel$edges_df$rel,
    "change_to")
})

test_that("adding nodes from several table columns to a graph is possible", {

  # Create a simple graph
  graph <-
    create_graph() %>%
    add_path(n = 2)

  # Create a data frame from which several
  # columns have values designated as graph nodes
  df <-
    data.frame(
      col_1 = c("f", "p", "q"),
      col_2 = c("q", "x", "f"),
      col_3 = c(1, 5, 3),
      col_4 = c("a", "v", "h"),
      stringsAsFactors = FALSE)

  # Add nodes from columns `col_1` and `col_2`
  # from the data frame to the graph object
  graph <-
    graph %>%
    add_nodes_from_df_cols(
      df = df,
      columns = c("col_1", "col_2"))

  # Expect a certain sequence of node `label` values
  expect_equal(
    get_node_df(graph)$label,
    c("1", "2", "f", "p", "q", "x"))

  # Add new nodes from columns 3 and 4; we are here
  # specifying the columns by their numbers
  graph <-
    graph %>%
    add_nodes_from_df_cols(
      df = df,
      columns = 3:4)

  # Expect a certain sequence of node `label` values
  expect_equal(
    get_node_df(graph)$label,
    c("1", "2", "f", "p", "q", "x",
      "a", "v", "h"))

  # Add column 4's values as labels/nodes again
  graph <-
    graph %>%
    add_nodes_from_df_cols(
      df = df,
      columns = 4)

  # Expect no change in the graph
  expect_equal(
    get_node_df(graph)$label,
    c("1", "2", "f", "p", "q", "x",
      "a", "v", "h"))

  # Add column 4's values as labels/nodes except with
  # the `keep_duplicates` argument set to TRUE
  graph <-
    graph %>%
    add_nodes_from_df_cols(
      df = df,
      columns = 4,
      keep_duplicates = TRUE)

  # Expect duplicated labels in the graph
  expect_equal(
    get_node_df(graph)$label,
    c("1", "2", "f", "p", "q", "x",
      "a", "v", "h", "a", "v", "h"))

  # Add column 4's values as labels/nodes with
  # the `keep_duplicates = TRUE` and a `type` value
  # of `new`
  graph <-
    graph %>%
    add_nodes_from_df_cols(
      df = df,
      columns = 4,
      type = "new",
      keep_duplicates = TRUE)

  # Expect more duplicated labels in the graph
  expect_equal(
    get_node_df(graph)$label,
    c("1", "2", "f", "p", "q", "x",
      "a", "v", "h", "a", "v", "h",
      "a", "v", "h"))

  # Expect the `type` value of `new` to appear
  # for the last three nodes (others are not set)
  expect_equal(
    graph %>% get_node_df() %>% .$type,
    c(rep(NA_character_, 12),
      rep("new", 3)))
})
