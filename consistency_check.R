#' New consistency diagnostics
#' @param table - netsplit object
#' @param model_type - character - fixed or random
#' @param vertical - Boolean - If TRUE vertical CIs are plotted. Otherwise horizontal. Interpretation is the same.
#' @param alphalev - numeric - level of error type I for colour-displaying significant p-values
#' @param labelsize - numeric - size of comparison labels
#'

consistency_check <- function(table,
                              model_type = "fixed",
                              vertical = TRUE,
                              alphalev = 0.05,
                              labelsize = 5)
{

  # Extract direct and indirect estimates
  direct <- table[[paste("direct", model_type, sep = '.')]] %>%
    select(c("comparison", "TE")) %>%
    rename(direct.estimate = TE)

  indirect <- table[[paste("indirect", model_type, sep = '.')]] %>%
    select(c("comparison", "TE")) %>%
    rename(indirect.estimate = TE)


  # Merge estimates and keep only comparisons for which we
  # have both direct and indirect.
  trt.estimates <- inner_join(direct, indirect, by = "comparison") %>%

    filter(is.na(direct.estimate)   == F,
           is.na(indirect.estimate) == F)


  # Extract the confidence intervals for the difference (and p-value)
  diff.ci <- table[[paste("compare", model_type, sep = '.')]] %>%
    select(c("comparison", "TE", "lower", "upper", "p")) %>%
    rename("diff"      = "TE",
           "diff.low"  = "lower",
           "diff.up"   = "upper",
           "diff.pval" = "p")

  graph_df <- left_join(
    trt.estimates,
    diff.ci,
    by = 'comparison') %>%
    mutate(
      signif = if_else(diff.pval < alphalev,
                       "Y",
                       "N")) %>%
  {
    if (vertical)
      mutate(.,
             new.ci.low = direct.estimate - diff.low,
             new.ci.up = direct.estimate - diff.up)
    else
      mutate(.,
             new.ci.low = indirect.estimate + diff.low,
             new.ci.up = indirect.estimate + diff.up)
    }

  lines_df <- graph_df %>%
    select(comparison,
           direct.estimate,
           indirect.estimate,
           new.ci.low,
           new.ci.up,
           signif) %>%
    pivot_longer(cols = c(new.ci.low, new.ci.up),
                 names_to = "Bound",
                 values_to = "Value")

  graph_df$signif <- factor(graph_df$signif,
                            levels = c("N", "Y"))
  lines_df$signif <- factor(lines_df$signif,
                            levels = c("N", "Y"))

  axis.lim <- max(abs(lines_df$Value))
  axis.color <- "gray40"

  if (vertical)
  {
    maps <- aes(x = direct.estimate,
                y = Value,
                group = comparison,
                color = signif)
    mapslab <- aes(x = direct.estimate,
                y = Value,
                label = comparison,
                color = signif)
  } else {

    maps <- aes(x = Value,
                y = indirect.estimate,
                group = comparison,
                color = signif)
    mapslab <- aes(x = Value,
                y = indirect.estimate,
                label = comparison,
                color = signif)
  }

  ggplot() +
    geom_abline(slope = 1, intercept = 0) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +

    geom_line(data = lines_df,
              mapping = maps) +
    geom_point(data = graph_df,
               aes(x = direct.estimate,
                   y = indirect.estimate,
                   color = signif)) +
    geom_text_repel(data = lines_df %>%
                      filter(Bound == "new.ci.low"),
                    mapping = mapslab,
                    size = labelsize) +
    # Significance colors
    scale_color_manual(values = c("gray50", "red")) +
    coord_cartesian(xlim = c(-axis.lim, axis.lim),
                    ylim = c(-axis.lim, axis.lim)) +

    # Define theme
    theme(
      axis.title.x        = element_text(margin = margin(t = 15)),
      axis.title.y        = element_text(margin = margin(r = 10)),
      plot.margin         = margin(20, 40, 20, 20),
 #      panel.background    = element_rect(fill = alpha("white", 1)),
      axis.ticks          = element_line(color = axis.color),
      axis.line           = element_line(color = axis.color),
 #     panel.grid          = element_line(color = alpha("white", 0)),
      axis.text           = element_text(color = axis.color),
      axis.title          = element_text(size  = 9,
                                         color = axis.color),
      panel.background = element_rect(fill = "gray99"),
      panel.grid.major = element_line(color = "gray95"),
      panel.grid.minor = element_blank(),
      legend.position     = 'none',
      #plot.title.position = 'plot',
      #plot.title          = element_text(size = 12)
      ) +
    # theme(
    #   axis.title.x      = element_text(margin = margin(t = 12.5)),
    #   axis.title.y     = element_text(margin = margin(r = 12.5)),
    #   plot.margin      = margin(20, 20, 20, 20),
    #   panel.background = element_rect(fill = "gray99"),
    #   panel.grid.major = element_line(color = "gray95"),
    #   panel.grid.minor = element_blank(),
    #   axis.ticks       = element_line(color = "gray95"),
    #   legend.position     = 'none'
    # ) +
    # Add plot labels
    labs(title = "Direct Estimates against Indirect Estimates and 95% Confidence Intervals of their Differences",
         subtitle = paste(str_to_title(model_type), "Effects Model"),
         x     = "Direct Effect Estimate",
         y     = "Indirect Effect Estimate",
         caption = paste0(
           "Significant results (alpha < ",
           alphalev,
           ") are displayed in red"
         )
         )




}
