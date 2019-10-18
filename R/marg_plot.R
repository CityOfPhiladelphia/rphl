
#' Function to plot marginal levels / effects
#'
#' In most situations, the plot will place treatments on the x axis, predicted
#' values (whether levels or effects) on the y axis, and will facet by subgroup,
#' if any. The only exception is when the input is marginal *effects* rather than
#' marginal *levels*, **and** there are only two treatment levels. In this case,
#' the x axis will display subgroup, since there would be only one treatment to
#' display in each facet.
#'
#' @param res Object output from \code{modmarg::marg}
#' @param dat Dataset used to estimate models (used to recover sample sizes)
#' @param y_scale Function that takes a numeric argument and returns a character
#'   for output labels
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' library(extrafont)
#'
#' data(mtcars)
#' dat <- mtcars %>%
#'   mutate_at(vars(cyl, am), as.factor)
#'
#' # Overall
#' mod <- glm(mpg ~ disp + cyl, data = dat)
#'
#' # Main levels
#' res <- modmarg::marg(mod, var_interest = "cyl")
#' marg_plot(
#'   res,
#'   dat = dat,
#'   y_scale = function(x) round(x, digits = 1)) +
#'   labs(x = "Cylinders", y = "Predicted miles per gallon")
#'
#' # Main effects
#' res <- modmarg::marg(mod, var_interest = "cyl", type = "effects")
#' marg_plot(
#'   res,
#'   dat = dat,
#'   y_scale = function(x) round(x, digits = 1)) +
#'   labs(x = "Cylinders", y = "Predicted miles per gallon")
#'
#' # Subgroups
#' mod_sub <- glm(mpg ~ disp + cyl * am, data = dat)
#'
#' # Levels
#' res <- sub_marg(mod_sub, var_interest = "cyl", subgroup = "am")
#' marg_plot(
#'   res,
#'   dat = dat,
#'   y_scale = function(x) round(x, digits = 1)) +
#'   labs(x = "Cylinders", y = "Predicted miles per gallon")
#'
#' # Effects
#' res <- sub_marg(mod_sub, var_interest = "cyl", subgroup = "am",
#'                 type = "effects")
#' marg_plot(
#'   res,
#'   dat = dat,
#'   y_scale = function(x) round(x, digits = 1)) +
#'   labs(x = "Cylinders", y = "Predicted miles per gallon")
#'
#' # Subgroup effects with two treatment levels
#' res <- sub_marg(
#'   mod_sub,
#'   var_interest = "am",
#'   subgroup = "cyl",
#'   type = "effects")
#'
#' marg_plot(
#' res, dat = dat, y_scale = function(x) round(x, digits = 1)) +
#' labs(x = "am", y = "Predicted miles per gallon")
#'
#' @export


marg_plot <- function(res, dat,
                      y_scale = function(x)
                        scales::percent(x, accuracy = 0.1),
                      color_fun = scale_color_phl,
                      label_size = 3){
  subgroup <- NULL
  res <- bind_rows(res)

  # clean up treatment variable
  var_interest <- unique(gsub(" = .*$", "", res$Label))
  names(res)[names(res) == "Label"] <- var_interest
  res[[var_interest]] <- forcats::as_factor(gsub("^.* = ", "", res[[var_interest]]))
  res$Label <- y_scale(res$Margin)

  # clean up CI
  ci_level <- as.numeric(gsub("Lower CI \\(|\\)", "", names(res[[1]][6])))
  names(res)[grepl("Lower|Upper CI", names(res))] <- c("lower_ci", "upper_ci")

  # get n size

  # Subgroups first
  if (any(grepl("^subgroup: ", names(res)))){
    subgroup <- names(res)[grepl("^subgroup: ", names(res))]
    subgroup <- gsub("^subgroup: ", "", subgroup)
    names(res)[grepl("^subgroup: ", names(res))] <- subgroup
    res$subgroup_lab <- res[[subgroup]] # sprintf("%s = %s", subgroup, res[[subgroup]])

    # If it's an effects plot, we need the full N size,
    # otherwise break it up by treatment

    res[[subgroup]] <- factor(res[[subgroup]], levels = levels(dat[[subgroup]]))

    if(any(is.na(res$P.Value))){
      res <- dat %>%
        count_(subgroup) %>%
        left_join(res, by = subgroup)
    } else {
      res <- dat %>%
        count_(c(var_interest, subgroup)) %>%
        left_join(res, by = c(var_interest, subgroup))
    }
    res[[subgroup]] <- forcats::fct_inorder(sprintf(
      "%s\nn = %s",
      res[[subgroup]],
      scales::comma(res$n)))

    if((length(unique(res[[var_interest]])) > 2 & !is.null(subgroup)) |
       !any(is.na(res$P.Value))){
      p <- filter(res, !is.na(P.Value)) %>%
        ggplot(aes_string(
          x = var_interest, y = "Margin",
          color = var_interest,
          label = "Label",
          ymin = "lower_ci",
          ymax = "upper_ci")) +
        facet_wrap(~ subgroup_lab)
    } else {
      p <- filter(res, !is.na(P.Value)) %>%
        ggplot(aes_string(
          x = subgroup, y = "Margin",
          color = var_interest,
          label = "Label",
          ymin = "lower_ci",
          ymax = "upper_ci"))
    }


  } else {
    # No subgroups
    res <- dat %>%
      count_(var_interest) %>%
      left_join(res, by = var_interest)

    res[[var_interest]] <- forcats::fct_inorder(sprintf(
      "%s\nn = %s",
      res[[var_interest]],
      scales::comma(res$n)))

    p <- filter(res, !is.na(P.Value)) %>%
      ggplot(aes_string(
        x = var_interest, y = "Margin",
        color = var_interest,
        label = "Label",
        ymin = "lower_ci",
        ymax = "upper_ci"))
  }

  # Effects
  if(any(is.na(res$P.Value))){
    p <- p +
      scale_y_continuous(labels = y_scale) +
      geom_hline(yintercept = 0) +
      color_fun()
  } else {
    # Effects
    p <- p +
      scale_y_continuous(
        labels = y_scale,
        expand = c(0, 0, 0.05, 0)) +
      expand_limits(y = 0) +
      color_fun()
  }

  p <- p +
    geom_pointrange(show.legend = F, size = 1.5, fatten = 2) +
    geom_text(
      aes(vjust = ifelse(Margin > 0, 0, 1)),
      color = "black", hjust = -0.5, family = "Montserrat",
      show.legend = F,
      size = label_size) +
    theme_phl() +
    labs(x = "", caption = sprintf("Bars represent %s confidence intervals", ci_level))

  p
}
