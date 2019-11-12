library(ggplot2, help, pos = 2, lib.loc = NULL)
d <- read.csv("figure/Diffusion/Temperature.csv", header=T, encoding="UTF-8")
names(d) <- c("x", "y")
d$y <- d$y+147.5

pdf("figure/Diffusion/Temperature.pdf", width=10, height=5)
p1 <- ggplot() + geom_line(data=d, aes(x=x, y=y), color="#2074A9") + ylab("Temperature(K)") + xlab("Time(ps)") + geom_line(aes(x=c(-30, 700), y=c(300, 300)), size=2)
#+ geom_line(aes(x=p, y=pp), size=1.5)
p1 + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size=16), text = element_text(size=16), legend.position = "bottom")
dev.off()
