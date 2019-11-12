library(ggplot2, help, pos = 2, lib.loc = NULL)
d <- read.csv("figure/Adsorption/sAM.csv", header=T, encoding="UTF-8")
names(d) <- c("T", "P", "L", "F")
f <- read.csv("figure/Adsorption/uAM.csv", header=T, encoding="UTF-8")
names(f) <- c("T", "P", "L", "F")
d$F <- "saturated"
f$F <- "unsaturated"

aa <- rbind(d, f, names = F)

aa$T <- factor(aa$T)
T623 <- aa[aa$T==623,]
T673 <- aa[aa$T==673,]
T723 <- aa[aa$T==723,]
T773 <- aa[aa$T==773,]
L <- function(P, V, b) {V*(b*P)/(1+b*P)}
qqq <- T623

sC4 <- qqq[qqq$F=="saturated",]
uC4 <- qqq[qqq$F=="unsaturated",]

resultsC4 <- summary(nls(sC4$L~L(sC4$P, V, b), data=sC4, start=list(V=60, b=0.1)))$coefficients
resultuC4 <- summary(nls(uC4$L~L(uC4$P, V, b), data=uC4, start=list(V=60, b=0.1)))$coefficients
VsC4 <- resultsC4[1, 1]
bsC4 <- resultsC4[2, 1]
VuC4 <- resultuC4[1, 1]
buC4 <- resultuC4[2, 1]


P <- seq(0, 500, 1)
LsC4 <- L(P, VsC4, bsC4)
LuC4 <- L(P, VuC4, buC4)


pdf("figure/Adsorption/sAndu623.pdf")
p1 <- ggplot() + geom_line(aes(P, LsC4), size=1.5) + geom_line(aes(P, LuC4), size=1.5) + geom_point(data=qqq, aes(x=P, y=L, color=F, shape=F), size=4) + ylab("Average loading\n(molecular/cell)") + xlab("Pressure(kPa)") + ggtitle("623K")
p1 + theme(plot.title = element_text(hjust = 0.5), text = element_text(size=16), legend.position = "bottom")

dev.off()
