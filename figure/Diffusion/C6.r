library(ggplot2, help, pos = 2, lib.loc = NULL)
d <- read.csv("figure/Diffusion/C6.csv", header=T, encoding="UTF-8")
names(d) <- c("T", "Time", "MSD", "F")
d$F <- factor(d$F)
d$T <- factor(d$T)


M300 <- d[d$F=="MacroPore60",][d$T==300,][d$Time>20,]
M400 <- d[d$F=="MacroPore60",][d$T==400,][d$Time>20,]
M20 <- d[d$F=="MacroPore20",]



results300 <- summary(lm(M300$MSD~M300$Time))
a300 <- results300$coefficients[1, 1]
b300 <- results300$coefficients[2, 1]
p300 <- seq(20, 100, 1)
pp300 <- b300*p300+a300

results400 <- summary(lm(M400$MSD~M400$Time))
a400 <- results400$coefficients[1, 1]
b400 <- results400$coefficients[2, 1]
p400 <- seq(20, 100, 1)
pp400 <- b400*p400+a400


pdf("figure/Diffusion/C6.pdf")
p1 <- ggplot() + geom_point(data=d, aes(x=Time, y=MSD, color=T), size=4) + ylab("MSD(Å^2)") + xlab("Time(ps)") + geom_line(aes(x=p300, y=pp300), size=1.5) + geom_line(aes(x=p400, y=pp400), size=1.5)
p2 <- p1 + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size=16), text = element_text(size=16), legend.position = "bottom")
p2 + scale_color_manual(values=c("#2074A9", "#149E53", "#C69523", "#BB4737", "#74777C"))
dev.off()
