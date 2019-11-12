library(ggplot2)
library(ggthemes)
d <- read.csv("figure/Building/COMPASS2andCVFF.csv", header=T, encoding="UTF-8")
names(d) <- c("T", "P", "L", "F")
CVFF <- d[d$F=="CVFF",]
COMPASS2 <- d[d$F=="COMPASS2",]
d$T <- factor(d$T)
COMPASS2$T <- factor(COMPASS2$T)
CVFF$T <- factor(CVFF$T)
d <- CVFF

T623 <- d[d$T==623,]
T673 <- d[d$T==673,]
T723 <- d[d$T==723,]
T773 <- d[d$T==773,]


L <- function(P, V, b) {V*(b*P)/(1+b*P)}
result623 <- summary(nls(T623$L~L(T623$P, V, b), data=T623, start=list(V=50, b=0.1)))$coefficients
result673 <- summary(nls(T673$L~L(T673$P, V, b), data=T673, start=list(V=60, b=0.1)))$coefficients
result723 <- summary(nls(T723$L~L(T723$P, V, b), data=T723, start=list(V=70, b=0.1)))$coefficients
result773 <- summary(nls(T773$L~L(T773$P, V, b), data=T773, start=list(V=80, b=0.1)))$coefficients
V623 <- result623[1, 1]
b623 <- result623[2, 1]
V673 <- result673[1, 1]
b673 <- result673[2, 1]
V723 <- result723[1, 1]
b723 <- result723[2, 1]
V773 <- result773[1, 1]
b773 <- result773[2, 1]


P <- seq(0, 500, 1)

L623 <- L(P, V623, b623)
L673 <- L(P, V673, b673)
L723 <- L(P, V723, b723)
L773 <- L(P, V773, b773)

pdf("figure/Building/CVFF.pdf")
p1 <- ggplot() + geom_line(aes(P, L623), size=1.5) + geom_line(aes(P, L673), size=1.5) + geom_line(aes(P, L723), size=1.5) + geom_line(aes(P, L773), size=1.5) + geom_point(data=CVFF, aes(x=P, y=L, color=T, shape=T), size=4) + ylab("Average loading\n(molecular/cell)") + xlab("Pressure(kPa)") + ggtitle("CVFF")
p1 + theme(plot.title = element_text(hjust = 0.5), text = element_text(size=16), legend.position = "bottom")
dev.off()
