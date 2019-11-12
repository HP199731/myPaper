library(ggplot2, help, pos = 2, lib.loc = NULL)
dM <- read.csv("figure/Adsorption/heatMM.csv", header=T, encoding="UTF-8")
d20 <- read.csv("figure/Adsorption/heatM20.csv", header=T, encoding="UTF-8")
d60 <- read.csv("figure/Adsorption/heatM60.csv", header=T, encoding="UTF-8")
d <- rbind(dM, d20, d60)

names(d) <- c("T", "P", "Q", "F")
d$T <- factor(d$T)
dM$T <- factor(dM$T)
d20$T <- factor(d20$T)
d60$T <- factor(d60$T)

d623 <- d[d$T==623,]
d673 <- d[d$T==673,]
d723 <- d[d$T==723,]
d773 <- d[d$T==773,]
a <- c(100,100)
b <- c(19,22)


pdf("figure/Adsorption/MicroPoreH.pdf")

p <- ggplot() +geom_line(data=dM, aes(P, Q, color=T), size=1.5)+geom_point(data=dM, aes(P, Q, color=T, shape=T), size=4)+ ylab("Isosteric heats\n(kcal/mol)") + xlab("Pressure(kPa)")
#pdf("figure/Adsorption/Heat673.pdf")
#p <- ggplot() + geom_line(data=d673, aes(P, Q, color=F), size=1.5)+geom_point(data=d673, aes(P, Q, color=F, shape=F), size=4)+ ylab("Isosteric heats\n(kcal/mol)") + xlab("Pressure(kPa)")
p + theme(plot.title = element_text(hjust = 0.5), text = element_text(size=16), legend.position = "bottom")+ scale_color_manual(values=c("#2074A9", "#149E53", "#C69523", "#BB4737", "#74777C"))
dev.off()
