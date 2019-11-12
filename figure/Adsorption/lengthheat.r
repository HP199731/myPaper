library(ggplot2, help, pos = 2, lib.loc = NULL)
d <- read.csv("figure/Adsorption/length.csv", header=T, encoding="UTF-8")
names(d) <- c("P", "L", "F", "Q")
d$F <- factor(d$F)
d$L <- d$L*0.014448
d <- d[d$P!=0,]

C2 <- d[d$F=="C2",]
C3 <- d[d$F=="C3",]
C4 <- d[d$F=="C4",]
C5 <- d[d$F=="C5",]


results2 <- summary(lm(C2$Q~C2$L))
a2 <- results2$coefficients[1, 1]
b2 <- results2$coefficients[2, 1]
p2 <- seq(0, 1, 0.1)
pp2 <- b2*p2+a2

results3 <- summary(lm(C3$Q~C3$L))
a3 <- results3$coefficients[1, 1]
b3 <- results3$coefficients[2, 1]
p3 <- seq(0.2, 1.1, 0.1)
pp3 <- b3*p3+a3

results4 <- summary(lm(C4$Q~C4$L))
a4 <- results4$coefficients[1, 1]
b4 <- results4$coefficients[2, 1]
p4 <- seq(0.4, 1.1, 0.1)
pp4 <- b4*p4+a4

results5 <- summary(lm(C5$Q~C5$L))
a5 <- results5$coefficients[1, 1]
b5 <- results5$coefficients[2, 1]
p5 <- seq(0.5, 0.9, 0.1)
pp5 <- b5*p5+a5

pdf("figure/Adsorption/lengthheat.pdf")
p1 <- ggplot() + geom_line(aes(p2, pp2), size=1.5) + geom_line(aes(p3, pp3), size=1.5) + geom_line(aes(p4, pp4), size=1.5)+ geom_line(aes(p5, pp5), size=1.5)+ geom_point(data=d, aes(x=L, y=Q, color=F, shape=F), size=4) + ylab("Isosteric heats\n(kcal/mol)") + xlab("Average loading\n(mmol/g)") + ggtitle("Length")
p1 + theme(plot.title = element_text(hjust = 0.5), text = element_text(size=16), legend.position = "bottom")
dev.off()
