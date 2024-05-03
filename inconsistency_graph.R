

# Read in libraries
library(netmeta)
library(ggrepel)
library(tidyverse)

# Read in data
data("Senn2013")


# Perform network meta-analysis
net <- netmeta(TE, seTE, treat1, treat2, studlab,
               data = Senn2013, sm = "MD",
               random = TRUE)

# Define inconsistency graph function
# Arguments:
#   netmeta_obj    = A netmeta object from the netmeta package
#   eff_type       = Effect type, either "fixed" (default) or "random"
#   alpha          = Significance threshold for the difference p-value (default is 0.05)
#   color_by       = Variable used for color, either "significance" (default) or "comparison"
#   include_labels = Logical, should text labels be included (TRUE is default)
#   vertical = Logical, should CIs be displayed vertically (TRUE) or horizontally ?


inconsistency_graph <- function(netmeta_obj,
                                eff_type = "fixed",
                                alpha = 0.05,
                                color_by = "significance",
                                include_labels = TRUE,
                                vertical = TRUE){

  netsplit_obj <- netsplit(netmeta_obj)

  # Extract direct and indirect effect estimates
  direct <- netsplit_obj[[paste("direct", eff_type, sep = ".")]] %>%
    select(comparison, TE) %>%
    rename(direct.estimate = TE)

  indirect <- netsplit_obj[[paste("indirect", eff_type, sep = ".")]] %>%
    select(comparison, TE) %>%
    rename(indirect.estimate = TE)

  # Can only plot comparisons that have a direct and indirect estimate
  estimates <- inner_join(direct, indirect, by = "comparison") %>%
    filter(!is.na(direct.estimate) & !is.na(indirect.estimate))

  # Extract the difference between the direct and indirect estimates
  diff <- netsplit_obj[[paste("compare", eff_type, sep = ".")]] %>%
    select(comparison, TE, lower, upper, p) %>%
    rename(diff = TE,
           diff.low = lower,
           diff.up  = upper,
           diff.pval = p)

  # Find the new coordinates of the confidence intervals
  graph_df <- left_join(estimates, diff, by = "comparison") %>%
    pivot_longer(cols = c(diff, diff.low, diff.up)) %>%
    filter(name != "diff") %>%
    mutate(significance = if_else(diff.pval <= alpha,
                                  "Significant",
                                  "Non-significant")
           ) %>%
    {
      if (vertical)
        mutate(.,
               x_coord = direct.estimate,
               y_coord = value + direct.estimate
               )
      else
        mutate(.,
               x_coord = value + indirect.estimate,
               y_coord = indirect.estimate
               )
    }

  # Find the max coordinate magnitude
  max_coord <- ceiling(
    max(abs(graph_df$x_coord),
        abs(graph_df$y_coord))
    )


  # Create text geom if required
  if (include_labels == TRUE){

    # plot comparison next to CI
    text_geom <- geom_text_repel(
      data = graph_df %>%
        filter(name == "diff.up"),
      aes(x = x_coord,
          y = y_coord,
          label = comparison,
          color = reorder(.data[[color_by]], -indirect.estimate)),
      seed = 1,
      min.segment.length = unit(0, 'lines'),
      show.legend = FALSE,
      size = 3.5)
  }
  else
    text_geom <- geom_blank()


  browser()
  # Graph code
  p <- ggplot() +

    geom_hline(yintercept = 0, color = "gray60") +
    geom_vline(xintercept = 0, color = "gray60") +
    geom_abline(slope = 1, intercept = 0, color = "gray60") +

    geom_point(data = graph_df,
               aes(x = direct.estimate,
                   y = indirect.estimate,
                   color = reorder(.data[[color_by]], -indirect.estimate)),
               size = 2) +

    # plot CIs
    geom_line(data = graph_df,
              aes(x = x_coord,
                  y = y_coord,
                  group = comparison,
                  color = reorder(.data[[color_by]], -indirect.estimate))) +

    text_geom +

    coord_cartesian(xlim = c(-max_coord, max_coord),
                    ylim = c(-max_coord, max_coord),
                    expand = F) +

    theme(axis.title.x      = element_text(margin = margin(t = 12.5)),
          axis.title.y     = element_text(margin = margin(r = 12.5)),
          plot.margin      = margin(20, 20, 20, 20),
          panel.background = element_rect(fill = "gray99"),
          panel.grid.major = element_line(color = "gray95"),
          panel.grid.minor = element_blank(),
          axis.ticks       = element_line(color = "gray95"),
          legend.position= ifelse(include_labels,
                                  "none",
                                  "bottom")) +

    labs(title = "Direct Estimates against Indirect Estimates with 95% Confidence Intervals of their Differences",
         subtitle = paste(str_to_title(eff_type), "Effects"),
         x     = "Direct Effect Estimate",
         y     = "Indirect Effect Estimate",
         color = if_else(color_by == "significance",
                         "Difference Type",
                         "Comparison"))
  return(p)
}


inconsistency_graph(netmeta_obj = net,
                    eff_type = "fixed",
                    alpha = 0.05,
                    color_by = "comparison",
                    include_labels = TRUE,
                    vertical = FALSE)


