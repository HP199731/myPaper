library(ggplot2, help, pos = 2, lib.loc = NULL)
d <- read.csv("figure/Diffusion/diffusion.csv", header=T, encoding="UTF-8")
names(d) <- c("Time", "MSD", "F")
d$F <- factor(d$F)

MM <- d[d$F=="MicroPore",][1:30,]
M20 <- d[d$F=="MacroPore20",]
M60 <- d[d$F=="MacroPore60",]

M20$MSD <- M20$MSD*10
aaa <- MM

results <- summary(lm(aaa$MSD~aaa$Time))
a <- results$coefficients[1, 1]
b <- results$coefficients[2, 1]
p <- seq(1, 150, 1)
pp <- b*p+a

pdf("figure/Diffusion/MSDMM.pdf")
p1 <- ggplot() + geom_point(data=aaa, aes(x=Time, y=MSD), color="#2074A9", size=4) + ylab("MSD(Å^2)") + xlab("Time(ps)")+geom_line(aes(x=p, y=pp), size=2)
p1 + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size=16), text = element_text(size=16))
dev.off()
