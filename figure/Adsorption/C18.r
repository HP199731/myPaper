library(ggplot2, help, pos = 2, lib.loc = NULL)
d <- read.csv("figure/Adsorption/C18.csv", header=T, encoding="UTF-8")
names(d) <- c("P", "L", "F")


trans <- d[d$F=="trans",]
cis <- d[d$F=="cis",]


L <- function(P, V, b) {V*(b*P)/(1+b*P)}
result1 <- summary(nls(cis$L~L(cis$P, V, b), data=cis, start=list(V=3, b=2)))$coefficients
V1 <- result1[1, 1]
b1 <- result1[2, 1]
result2 <- summary(nls(trans$L~L(trans$P, V, b), data=trans, start=list(V=3, b=2)))$coefficients
V2 <- result2[1, 1]
b2 <- result2[2, 1]
P <- seq(0, 290, 1)

L1 <- L(P, V1, b1)
L2 <- L(P, V2, b2)


pdf("figure/Adsorption/C18.pdf")
p1 <- ggplot() + geom_line(aes(P, L1), size=1.5) + geom_line(aes(P, L2), size=1.5) + geom_point(data=d, aes(x=P, y=L, color=F, shape=F), size=4) + ylab("Average loading\n(molecular/cell)") + xlab("Pressure(kPa)") + ggtitle("ME and MO")
p1 + theme(plot.title = element_text(hjust = 0.5), text = element_text(size=16), legend.position = "bottom")
dev.off()
