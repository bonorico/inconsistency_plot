# graphical explanation of the method

library(netmeta)
library(ggrepel)
library(tidyverse)

# pick one point on the plane
d <- 1.148291
i <- 1.466528

ggplot(
  data = tibble(dir = c(d, d, i),
                ind = c(i, d, i),
                labels = c("(d,i)",
                          "(d,d)",
                          "(i,i)")),
  aes(dir, ind, label = labels)
  ) +
  geom_vline(aes(xintercept = 0)) +
  geom_hline(aes(yintercept = 0)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  # draw radius and "wrong" dotted line
  geom_segment(aes(x = c(d, i, d),
                   y = c(i, i, d),
                   xend = c((d+i)/2, d, d),
                   yend = c((d+i)/2, i, i) ),
               color = c('black', 'red', 'red'),
               linetype=c("dotted", "solid", "solid")) +

  geom_point() +
  geom_text_repel(
    family = "Times New Roman",
    fontface = "italic",
    # direction = "both",
    # hjust = c(-0.3, 0, 0.3),
    # vjust = c(0, -0.3, 0)
  ) +

  annotate("path",
           x=d+(d-i)*cos(seq(0,2*pi,length.out=100)),
           y=i+(d-i)*sin(seq(0,2*pi,length.out=100))) +

  labs(x = "direct",
       y = "indirect") +
  theme(panel.background = element_rect(fill = "white") ) +
coord_cartesian(xlim = c(0, 2),
                ylim = c(0, 2),
                expand = TRUE
)



# Example Continuous Outcome  ---------------------------------------------------------


data("Senn2013")
net <- netmeta(TE, seTE, treat1, treat2, studlab,
               data = Senn2013, sm = "MD",
               random = TRUE)

table <- netsplit(net)


consistency_check(table)


input_model_type <- "fixed"

# Extract direct and indirect estimates
direct <- table[[paste("direct", input_model_type, sep = '.')]] %>%
  select(c("comparison", "TE")) %>%
  rename(direct.estimate = TE)

indirect <- table[[paste("indirect", input_model_type, sep = '.')]] %>%
  select(c("comparison", "TE")) %>%
  rename(indirect.estimate = TE)


# Merge estimates and keep only comparisons for which we
# have both direct and indirect.
trt.estimates <- inner_join(direct, indirect, by = "comparison") %>%

  filter(is.na(direct.estimate)   == F,
         is.na(indirect.estimate) == F)


# Extract the confidence intervals for the difference (and p-value)
diff.ci <- table[[paste("compare", input_model_type, sep = '.')]] %>%
  select(c("comparison", "TE", "lower", "upper", "p")) %>%
  rename("diff"      = "TE",
         "diff.low"  = "lower",
         "diff.up"   = "upper",
         "diff.pval" = "p")

graph_df <- left_join(trt.estimates, diff.ci, by = 'comparison') %>%
  mutate(new.ci.low = direct.estimate - diff.low,
         new.ci.up = direct.estimate - diff.up,
         signif = if_else(diff.pval < 0.05,
                          "Y",
                          "N"))

lines_df <- graph_df %>%
  select(comparison, direct.estimate, new.ci.low, new.ci.up, signif) %>%
  pivot_longer(cols = c(new.ci.low, new.ci.up),
               names_to = "Bound",
               values_to = "Value")

graph_df$signif <- factor(graph_df$signif,
                          levels = c("N", "Y"))
lines_df$signif <- factor(lines_df$signif,
                          levels = c("N", "Y"))

axis.lim <- max(abs(lines_df$Value))
axis.color <- "gray40"

ggplot() +

  geom_abline(slope = 1, intercept = 0) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +

  geom_line(data = lines_df,
            aes(x = direct.estimate,
                y = Value,
                group = comparison,
                color = signif)) +
  geom_point(data = graph_df,
             aes(x = direct.estimate,
                 y = indirect.estimate,
                 color = signif)) +
  geom_text_repel(data = lines_df %>% filter(Bound == "new.ci.low"),
                  aes(x = direct.estimate,
                      y = Value,
                      color = signif,
                      label = comparison),
                  size = 3) +
  # Significance colors
  scale_color_manual(values = c("gray50", "red")) +
  coord_cartesian(xlim = c(-axis.lim, axis.lim),
                  ylim = c(-axis.lim, axis.lim)) +

  # Define theme
  theme(panel.background    = element_rect(fill = alpha("white", 1)),
        axis.line           = element_line(color = axis.color),
        panel.grid          = element_line(color = alpha("white", 0)),
        axis.ticks          = element_line(color = axis.color),
        axis.text           = element_text(color = axis.color),
        plot.margin         = margin(20, 40, 20, 20),
        axis.title          = element_text(size  = 9,
                                           color = axis.color),
        axis.title.x        = element_text(margin = margin(t = 15)),
        axis.title.y        = element_text(margin = margin(r = 10)),
        legend.position     = 'none',
        plot.title.position = 'plot',
        plot.title          = element_text(size = 12)) +
  # Add plot labels
  labs(x = "Direct Estimate",
       y = "Indirect Estimate")

