#' Function to calculate margins for subgroups
#'
#' This function calculates marginal treatment effects for sub-populations, from
#' model objects compatible with \code{modmarg::marg}
#'
#' @param mod Any model object compatible with modmarg::marg (currently glm and
#'   ivreg)
#' @param subgroup character, name of the variable to group by
#' @param var_interest character,  name of the treatment variable
#' @param cofint numeric, desired confidence level, must be in (0, 1)
#' @param ... other arguments passed to modmarg::marg
#'
#' @examples
#' @export

sub_marg <- function(mod, subgroup,
                     var_interest = "treat",
                     cofint = 0.9,
                     ...){

  stopifnot(0 < cofint & cofint < 1)
  if(! subgroup %in% names(mod$model))
    stop("Subgroup not used in model")
  if(!is.factor(mod$model[[subgroup]])){
    warning(paste(
      sprintf(
        "\"%s\" is not a factor; converting to factor.",
        subgroup),
      "You should do this in advance to ensure",
      "your preferred ordering."))
    mod$model[[subgroup]] <- as.factor(mod$model[[subgroup]])
  }

  # Calculate margins for each subgroup
  res <- list()
  for(i in levels(mod$model[[subgroup]])) {
    res <- c(
      res,
      modmarg::marg(
        mod, var_interest = var_interest, cofint = cofint,
        data = mod$model[mod$model[[subgroup]] == i, ],
        ...)
    )
  }
  # Name results by subgroup
  names(res) <- levels(mod$model[[subgroup]])
  # Stack results
  res <- bind_rows(res, .id = sprintf("subgroup: %s", subgroup))
  # Maintain original ordering of subgroup
  res[[sprintf("subgroup: %s", subgroup)]] <- factor(
    res[[sprintf("subgroup: %s", subgroup)]],
    levels = levels(mod$model[[subgroup]]))

  res
}


psr <- function(mod) {
  # Pseudo-R2
  mod_null <- update(mod, ~ 1)

  #get log likelihoods for both models
  ll_full <- as.numeric(logLik(mod))
  ll_null <- as.numeric(logLik(mod_null))

  # get rank for full model to use in adjusted R2
  rank_full <- mod$rank

  # check that rank for null model is 1, otherwise throw an error
  if(mod_null$rank != 1){
    stop("null model created incorrectly")
  }

  # calculate mcfaddens r2 and adjusted r2
  psr <- c(
    "mcfaddens.R2" = 1 - (ll_full / ll_null),
    "mcfaddens.adj.R2" = 1 - ((ll_full - rank_full) / ll_null)
  )

  print(psr)

}
