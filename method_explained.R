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

# add filling of half triangle
   annotate(
     geom = "polygon",
     x = c(d, d, (d+i)/2),
     y = c(i, d, (d+i)/2),
     fill = "blue",
     alpha = 0.1
   ) +
  geom_text(
    x = (d+i)/2,
    y = (d+i)/2,
    label = "B",
    nudge_x = 0.3
  ) +
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
    tag = "A",
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

Q <- netmeta::decomp.design(net)

netmeta:::forest.netmeta(net)
netmeta::netrank(net)
netmeta::netgraph(net)

netmeta::netheat(net)

net$TE.direct.common
net$TE.indirect.common

# back-calculation
res <- netmeta::netsplit(net)
# node-splitting
# res2 <- netmeta::netsplit(net, method = "SIDDE")

res$method

prop <- res$compare.common |>
  left_join(
    data.frame(
      comparison = res$comparison,
      prop.direct = res$prop.common
    ),
    by = "comparison"
  )


netmeta:::forest.netsplit(
  res
)

plot2 <- consistency_check(res, mytitle = " ", show_prop = TRUE)

figure <- gridExtra::arrangeGrob(plot1, plot2, nrow=2)

ggsave("Figure 1.ps", figure,
       device=cairo_ps, dpi=800,
       width = 8.0, height = 16.0, units="in")

ggsave("Figure 1.png", figure,
       device = "png", dpi=800,
       width = 8.0, height = 16.0, units="in")




# Example Cig Data Cipriani 2018  ---------------------------------------------------------

cipriani_raw <- readxl::read_xlsx(
  "./Cipriani et al_GRISELDA_Lancet 2018_Open data.xlsx",
  na = c("*"),
  skip = 2
)


cipriani <- netmeta::pairwise(
  treat = Drug,
  event = Responders,
  n = No_randomised,
  studlab = StudyID,
  data = cipriani_raw
)

cip_res <- netmeta::netmeta(cipriani, sm = "OR", common = TRUE, prediction = FALSE, random = FALSE)

png("cipriani_net.png", width = 1500, height = 1000)
netmeta::netgraph(
  cip_res,
)
dev.off()


cip_split <- netmeta::netsplit(
  cip_res
)

png("cipriani_heat2.png", width = 500, height = 500)
netmeta::netheat(cip_res)
dev.off()

png("cipriani_forest.png", width = 600, height = 3000)
netmeta:::forest.netsplit(
  cip_split
)
dev.off()


plot3 <- consistency_check(cip_split,
                           mytitle = " ",
                           show_labels_only_signif = TRUE,
                           ylims = c(-4, 4),
                           plottag = "A")

plot4 <- consistency_check(cip_split,
                           mytitle = " ",
                           show_only_signif = TRUE,
                           show_prop = TRUE,
                           plottag = "B")


ggsave("Figure 2.png",
       figure <- gridExtra::arrangeGrob(
         plot3,
         plot4,
         nrow=2),
       device = "png", dpi=800,
       width = 12.0, height = 16.0, units="in")


