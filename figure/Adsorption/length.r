library(ggplot2, help, pos = 2, lib.loc = NULL)
d <- read.csv("figure/Adsorption/length.csv", header=T, encoding="UTF-8")
names(d) <- c("P", "L", "F","Q")
d$F <- factor(d$F)

C2 <- d[d$F=="C2",]
C3 <- d[d$F=="C3",]
C4 <- d[d$F=="C4",]
C5 <- d[d$F=="C5",]
L <- function(P, V, b) {V*(b*P)/(1+b*P)}
resultC2 <- summary(nls(C2$L~L(C2$P, V, b), data=C2, start=list(V=50, b=0.1)))$coefficients
resultC3 <- summary(nls(C3$L~L(C3$P, V, b), data=C3, start=list(V=60, b=0.1)))$coefficients
resultC4 <- summary(nls(C4$L~L(C4$P, V, b), data=C4, start=list(V=70, b=0.1)))$coefficients
resultC5 <- summary(nls(C5$L~L(C5$P, V, b), data=C5, start=list(V=80, b=0.1)))$coefficients
VC2 <- resultC2[1, 1]
bC2 <- resultC2[2, 1]
VC3 <- resultC3[1, 1]
bC3 <- resultC3[2, 1]
VC4 <- resultC4[1, 1]
bC4 <- resultC4[2, 1]
VC5 <- resultC5[1, 1]
bC5 <- resultC5[2, 1]


P <- seq(0, 500, 1)

LC2 <- L(P, VC2, bC2)
LC3 <- L(P, VC3, bC3)
LC4 <- L(P, VC4, bC4)
LC5 <- L(P, VC5, bC5)

pdf("figure/Adsorption/length.pdf")
p1 <- ggplot() + geom_line(aes(P, LC2), size=1.5) + geom_line(aes(P, LC3), size=1.5) + geom_line(aes(P, LC4), size=1.5) + geom_line(aes(P, LC5), size=1.5) + geom_point(data=d, aes(x=P, y=L, color=F, shape=F), size=4) + ylab("Average loading\n(molecular/cell)") + xlab("Pressure(kPa)")
p1 + theme(plot.title = element_text(hjust = 0.5), text = element_text(size=16), legend.position = "bottom")+ scale_color_manual(values=c("#2074A9", "#149E53", "#C69523", "#BB4737", "#74777C"))
dev.off()
