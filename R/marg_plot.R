
#' Function to plot marginal levels / effects
#'
#' @param res Object output from \code{modmarg::marg}
#' @param dat Dataset used to estimate models (used to recover sample sizes)
#' @param y_scale Function that takes a numeric argument and returns a character for output labels
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
#' mod <- glm(mpg ~ disp + cyl, data = dat)
#' res <- modmarg::marg(mod, var_interest = "cyl")
#' print(res)
#'
#' marg_plot(
#'   res,
#'   dat = dat,
#'   y_scale = function(x) round(x, digits = 1)) +
#'   labs(x = "Cylinders", y = "Predicted miles per gallon")
#'
#' mod_sub <- glm(mpg ~ disp + cyl * am, data = dat)
#' res <- sub_marg(mod_sub, var_interest = "cyl", subgroup = "am")
#' print(res)
#'
#' marg_plot(
#'   res,
#'   dat = dat,
#'   y_scale = function(x) round(x, digits = 1)) +
#'   labs(x = "Cylinders", y = "Predicted miles per gallon")
#'
#' res <- sub_marg(mod_sub, var_interest = "cyl", subgroup = "am",
#'                 type = "effects")
#' print(res)
#'
#' marg_plot(
#'   res,
#'   dat = dat,
#'   y_scale = function(x) round(x, digits = 1)) +
#'   labs(x = "Cylinders", y = "Predicted miles per gallon")
#'
#' @export


marg_plot <- function(res, dat,
                      y_scale = function(x)
                        scales::percent(x, accuracy = 0.1)){
  subgroup <- NULL
  res <- bind_rows(res)

  # clean up treatment variable
  var_interest <- unique(gsub(" = .*$", "", res$Label))
  names(res)[names(res) == "Label"] <- var_interest
  res[[var_interest]] <- gsub("^.* = ", "", res[[var_interest]])
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
    res$subgroup_lab <- sprintf("%s = %s", subgroup, res[[subgroup]])

    # If it's an effects plot, we need the full N size,
    # otherwise break it up by treatment

    if(any(is.na(res$P.Value))){
      res <- dat %>%
        count_(subgroup) %>%
        mutate_at(1, as.character) %>%
        left_join(res, by = c(subgroup))
    } else {
      res <- dat %>%
        count_(c(var_interest, subgroup)) %>%
        mutate_at(1, as.character) %>%
        left_join(res, by = c(var_interest, subgroup))
    }
    res[[subgroup]] <- forcats::fct_inorder(sprintf(
      "%s\nn = %s",
      res[[subgroup]],
      scales::comma(res$n)))

    if(length(unique(res[[var_interest]])) > 2 & !is.null(subgroup)){
      p <- filter(res, !is.na(P.Value)) %>%
        ggplot(aes_string(
          x = var_interest, y = "Margin",
          color =var_interest,
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
      mutate_at(1, as.character) %>%
      left_join(res, by = var_interest)

    res[[var_interest]] <- sprintf(
      "%s\nn = %s",
      res[[var_interest]],
      scales::comma(res$n))

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
      scale_color_phl()
  } else {
    # Effects
    p <- p +
      scale_y_continuous(
        labels = y_scale,
        expand = c(0, 0, 0.05, 0)) +
      expand_limits(y = 0) +
      scale_color_phl()
  }

  p <- p +
    geom_point(show.legend = F, size = 5) +
    geom_errorbar(show.legend = F, size = 1.5, width = 0.2) +
    geom_text(
      aes(vjust = ifelse(Margin > 0, 0, 1)),
      color = "black", hjust = -0.5, family = "Open Sans",
      show.legend = F) +
    theme_phl() +
    labs(x = "", caption = sprintf("Bars represent %s confidence intervals", ci_level))

  p
}
