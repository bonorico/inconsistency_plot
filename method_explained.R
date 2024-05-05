# graphical explanation of the method

library(netmeta)
library(ggrepel)
library(tidyverse)

source("./consistency_check.R")

# pick one point on the plane
d <- 1.148291
i <- 1.466528

plot1 <- ggplot(
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

  annotate("text", x = 0.6, y = 0.5,
           label = "1:1 line") +
  annotate("path",
           x=d+(d-i)*cos(seq(0,2*pi,length.out=100)),
           y=i+(d-i)*sin(seq(0,2*pi,length.out=100))) +

  labs(
    title = "A",
    x = "direct",
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

netmeta:::forest.netmeta(net)
netmeta::netrank(net)
netmeta::netgraph(net)

netmeta::netheat(net)

plot2 <- consistency_check(netsplit(net), mytitle = "B")

figure <- gridExtra::arrangeGrob(plot1, plot2, nrow=2)

ggsave("Figure 1.ps", figure,
       device=cairo_ps, dpi=800,
       width = 15.0, height = 15.0, units="in")
