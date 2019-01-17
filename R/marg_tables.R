
#' Function to produce tables of marginal levels / effects
#'
#' @param lev output of \code{modmarg::marg} with \code{type = "levels"}
#' @param eff output of \code{modmarg::marg} with \code{type = "effects"}
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' library(extrafont)
#'
#' data(mtcars)
#' dat <- mtcars %>%
#'   mutate(cyl = as.factor(cyl))
#' mod <- glm(mpg ~ disp + cyl, data = dat)
#' lev <- modmarg::marg(mod, var_interest = "cyl")
#' eff <- modmarg::marg(mod, var_interest = "cyl", type = "effects")
#'
#' marg_table(lev, eff)
#'
#' @export

marg_table <- function(lev, eff){
  lev <- bind_rows(lev)
  eff <- bind_rows(eff)

  var_interest <- unique(gsub(" = .*$", "", lev$Label))
  lev$Label <- gsub("^.* = ", "", lev$Label)
  eff$Label <- gsub("^.* = ", "", eff$Label)

  res <- lev[, 1:5] %>%
    select(Label, Level = Margin, Standard.Error) %>%
    left_join(eff[, 1:5], by = "Label") %>%
    rename(var_interest = Label,
           Effect = Margin,
           SE.Level = Standard.Error.x,
           SE.Effect = Standard.Error.y)

  names(res)[1] <- var_interest

  res
}

#' \code{marg_sub_table} handles output from \code{sub_marg}
#' @rdname marg_table
#' @examples
#'
#' data(mtcars)
#' dat <- mtcars %>%
#'   mutate_at(vars(cyl, am), as.factor)
#'
#' mod <- glm(mpg ~ disp + cyl * am, data = dat)
#' sub_marg(
#'   mod = mod,
#'   subgroup = "am",
#'   var_interest = "cyl",
#'   cofint = 0.9)


marg_sub_table <- function(lev, eff){
  lev <- bind_rows(lev)
  eff <- bind_rows(eff)

  var_interest <- unique(gsub(" = .*$", "", lev$Label))
  lev$Label <- gsub("^.* = ", "", lev$Label)
  eff$Label <- gsub("^.* = ", "", eff$Label)

  res <- lev[, 1:6] %>%
    select(1:2, Level = Margin, Standard.Error) %>%
    left_join(eff[, 1:6], by = names(lev)[1:2]) %>%
    rename(var_interest = Label,
           Effect = Margin,
           SE.Level = Standard.Error.x,
           SE.Effect = Standard.Error.y)

  names(res)[1] <- var_interest

  res
}
