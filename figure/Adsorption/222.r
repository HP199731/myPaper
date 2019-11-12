library(ggplot2, help, pos = 2, lib.loc = NULL)
d <- read.csv("figure/Adsorption/222.csv", header=T, encoding="UTF-8")
names(d) <- c("T", "P", "L", "F", "Q")

d[d$F=="MicroPore",]$L <- d[d$F=="MicroPore",]$L/69.2116
d[d$F=="MacroPore20",]$L <- d[d$F=="MacroPore20",]$L/54.416
d[d$F=="MacroPore60",]$L <- d[d$F=="MacroPore60",]$L/31.2525


 MM <- d[d$F=="MicroPore",]
 M20 <- d[d$F=="MacroPore20",]
 M60 <- d[d$F=="MacroPore60",]
 M60$L <- M60$L/4
MM$T <- factor(MM$T)
M20$T <- factor(M20$T)
M60$T <- factor(M60$T)

pdf("figure/Adsorption/222M20.pdf")
p1 <- ggplot() + geom_line(data=M20, aes(x=L, y=Q, color=T), size=1.5)+geom_point(data=M20, aes(x=L, y=Q, shape=T, color=T), size=4) + ylab("Isosteric heats\n(kcal/mol)") + xlab("Average loading(mmol/g)")
p1 + theme(plot.title = element_text(hjust = 0.5), text = element_text(size=16), legend.position = "bottom")+ scale_color_manual(values=c("#2074A9", "#149E53", "#C69523", "#BB4737", "#74777C"))
dev.off()
