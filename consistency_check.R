#' New consistency diagnostics
#' @param table - netsplit object
#' @param model_type - character - fixed or random
#' @param vertical - Boolean - If TRUE vertical CIs are plotted. Otherwise horizontal. Interpretation is the same.
#' @param alphalev - numeric - level of error type I for colour-displaying significant p-values.
#' @param labelsize - numeric - size of comparison labels. It is also automatically determined if too many labels string exceeds 20 characters.
#' @param show_prop - Boolean - If TRUE, shows point size according to contributing percentage of indirect evidence in the mix.
#' @param show_prop_only_signif - Boolean - If TRUE and show_prop = TRUE, shows contributing percentage only for significantly different differences.
#' @param show_labels - Boolean - If TRUE, shows comparisons label.
#' @param show_labels_only_signif - Boolean - If TRUE and show_label = TRUE, only shows labels of significantly different comparisons.
#' @param max_overlap - Integer - Max number of overlapping labels allowed on plot.
#' @param square_plot -  Boolean - If TRUE, the plot is square and the 1:1 line exactly cut the plot in two diagonally. The ratio of the axis range over this ideal square-plot range is also calculated. If the former is less than 60% of the latter, then natural axes ranges are used (no square plot).
#' @param show_only_signif - Boolean - If TRUE shows only data from significanlty different comparisons.
#' @param xlims - ylims - Manual range for x and y axis.

consistency_check <- function(table,
                              model_type = "fixed",
                              vertical = TRUE,
                              alphalev = 0.05,
                              labelsize = 5,
                              mytitle = "Direct Estimates against Indirect Estimates and 95% Confidence Intervals of their Differences",
                              show_prop = FALSE,
                              show_prop_only_signif = FALSE,
                              show_labels = TRUE,
                              show_labels_only_signif = FALSE,
                              max_overlap = 10,
                              square_plot = TRUE,
                              show_only_signif = FALSE,
                              xlims = NULL,
                              ylims = NULL,
                              plottag = "B")
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
    # add proportion of indirect evidence
    left_join(
      data.frame(
        comparison = table$comparison,
        prop.indirect = (1-table[[paste("prop", model_type, sep = '.')]])*100
      ),
      by = "comparison"
    ) |>
    filter(is.na(direct.estimate)   == FALSE,
           is.na(indirect.estimate) == FALSE)


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
    } %>%
    # option to show dot size only for significant results
    {
      if (show_prop_only_signif)
          mutate(.,
            prop.indirect = ifelse(
              signif == "N",
              NA,
              prop.indirect
            )
          )
      else
        .
    } %>%
    # option to show only significant results if plot is too crowded
    {
      if (show_only_signif)
        filter(.,
               signif == "Y")
      else
        .
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
                 values_to = "Value") |>
    mutate(
      label_width = str_length(
        comparison
      )
    )

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

  if (show_prop)
    mapspoint <- aes(x = direct.estimate,
                     y = indirect.estimate,
                     color = signif,
                     size = prop.indirect)
  else
    mapspoint <- aes(x = direct.estimate,
                     y = indirect.estimate,
                     color = signif)




  p <- ggplot() +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed', color = "green4") +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +

    geom_line(data = lines_df,
              mapping = maps) +
    geom_point(data = graph_df,
               mapping = mapspoint
               ) +
    # Significance colors
    scale_color_manual(values = c(
      # if only significant data shown then switch to red
      ifelse(show_only_signif,
             "red",
             "gray50"),
      "red")
      ) +
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
      legend.position     = "bottom"
       ) +
    guides(colour = "none",
           size=guide_legend(title="Indirect (%)")) +
      # Add plot labels
    labs(title = mytitle,
         subtitle = paste(str_to_title(model_type), "Effects Model"),
         x     = "Direct Effect Estimate",
         y     = "Indirect Effect Estimate",
         caption = paste0(
           "Significant results (alpha < ",
           alphalev,
           ") are displayed in red"
         ),
         tag = plottag
         )

  # check if plane margins need being adjusted

  # calculate points range distance on both axes
  range_horizontal <- sqrt(diff(range(graph_df$direct.estimate))^2)
  range_vertical <- sqrt(diff(range(graph_df$indirect.estimate))^2)

  range_square <- sqrt(diff(c(-axis.lim, axis.lim))^2)

  # if point range is more than half the square range, then margins are likely too large and plot scale is too small
  dist_ratio <- ifelse(vertical,
                       range_vertical,
                       range_horizontal
                       )/range_square


  if (square_plot & dist_ratio >= 0.6)
    p <- p + coord_cartesian(xlim = c(-axis.lim, axis.lim),
                    ylim = c(-axis.lim, axis.lim))


  # check distribution of string widths
  p75w <- lines_df |>
    summarise(
      quantile(label_width, 0.75)
    ) |> pull()

  if (show_labels)
    p <-  p + geom_text_repel(data = lines_df %>%
                                filter(Bound == "new.ci.low") %>%
                                {
                                  if (show_labels_only_signif)
                                    filter(.,
                                           signif == "Y"
                                    )
                                  else
                                    .
                                    },
                              mapping = mapslab,
                              size = ifelse(
                                # if 3/4th of labels have width greater than 20 character than reduce cex
                                (p75w > 20) & (labelsize > p75w/14), # allow for smaller labelsize but not greater
                                p75w/14,
                                labelsize),
                              max.overlaps = max_overlap)

  if (!is.null(xlims))
    p <- p + xlim(xlims)
  if (!is.null(ylims))
    p <- p + ylim(ylims)

  return(p)


}
