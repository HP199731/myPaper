library(ggplot2, help, pos = 2, lib.loc = NULL)
d <- read.csv("figure/Adsorption/111.csv", header=T, encoding="UTF-8")
names(d) <- c("P", "Q", "L", "F")

d[d$F=="MicroPore",]$L <- d[d$F=="MicroPore",]$L/69.2116
d[d$F=="MacroPore20",]$L <- d[d$F=="MacroPore20",]$L/54.416
d[d$F=="MacroPore60",]$L <- d[d$F=="MacroPore60",]$L/31.2525/4


# MM <- d[d$F=="MicroPore",]
# M20 <- d[d$F=="MacroPore20",]
# M60 <- d[d$F=="MacroPore60",]


pdf("figure/Adsorption/111.pdf")
p1 <- ggplot() + geom_line(data=d, aes(x=L, y=Q, color=F), size=1.5) + geom_point(data=d, aes(x=L, y=Q, shape=F, color=F), size=4) + ylab("Isosteric heats\n(kcal/mol)") + xlab("Average loading(mmol/g)")
p1 + theme(plot.title = element_text(hjust = 0.5), text = element_text(size=16), legend.position = "bottom")+ scale_color_manual(values=c("#2074A9", "#149E53", "#C69523", "#BB4737", "#74777C"))
dev.off()
