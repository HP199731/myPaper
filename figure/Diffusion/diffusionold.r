library(ggplot2, help, pos = 2, lib.loc = NULL)
d <- read.csv("figure/Diffusion/diffusionold.csv", header=T, encoding="UTF-8")
names(d) <- c("Time", "MSD", "F")
d$F <- factor(d$F)

# MM <- d[d$F=="MicroPore",]
# M20 <- d[d$F=="MacroPore20",]
# M60 <- d[d$F=="MacroPore60",]

# aaa <- M20

# results <- summary(lm(aaa$MSD~aaa$Time))
# a <- results$coefficients[1, 1]
# b <- results$coefficients[2, 1]
# p <- seq(1, 400, 1)
# pp <- b*p+a

pdf("figure/Diffusion/MSD.pdf")
p1 <- ggplot() + geom_point(data=d, aes(x=Time, y=MSD, color=F, shape=F), size=4) + ylab("MSD(Angstrom^2)") + xlab("Time(ps)") + ggtitle("MSD vs. Time")
#+ geom_line(aes(x=p, y=pp), size=1.5)
p1 + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size=16), text = element_text(size=16), legend.position = "bottom")+ scale_color_manual(values=c("#2074A9", "#149E53", "#C69523", "#BB4737", "#74777C"))
dev.off()
