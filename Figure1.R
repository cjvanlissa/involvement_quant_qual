library(gridExtra)
source("Figure1between.R")
source("Figure1within.R")
p <- ggplot()+geom_vline(xintercept = 1, linetype = 3)+theme_void()
p <- grid.arrange(p_within, p, p_between, nrow = 1, widths = c(10,1,10))

Fig1 <- p
ggsave("Figure1.png", p)

ggsave("Figure1.pdf", p)
ggsave("Figure1.eps", p, device = "eps")
