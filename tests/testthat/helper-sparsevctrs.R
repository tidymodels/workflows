# ------------------------------------------------------------------------------
# For sparse tibble testing

sparse_hotel_rates <- function(tibble = FALSE) {
  if (!rlang::is_installed("modeldata")) {
    return()
  }

  # 99.2 sparsity
  hotel_rates <- modeldata::hotel_rates

  prefix_colnames <- function(x, prefix) {
    colnames(x) <- paste(colnames(x), prefix, sep = "_")
    x
  }

  dummies_country <- hardhat::fct_encode_one_hot(hotel_rates$country)
  dummies_company <- hardhat::fct_encode_one_hot(hotel_rates$company)
  dummies_agent <- hardhat::fct_encode_one_hot(hotel_rates$agent)

  res <- cbind(
    hotel_rates["avg_price_per_room"],
    prefix_colnames(dummies_country, "country"),
    prefix_colnames(dummies_company, "company"),
    prefix_colnames(dummies_agent, "agent")
  )

  res <- as.matrix(res)
  res <- Matrix::Matrix(res, sparse = TRUE)

  if (tibble) {
    res <- sparsevctrs::coerce_to_sparse_tibble(res)

    # materialize outcome
    withr::local_options("sparsevctrs.verbose_materialize" = NULL)
    res$avg_price_per_room <- res$avg_price_per_room[]
  }

  res
}
