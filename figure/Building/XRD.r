library(ggplot2)
d <- read.csv("figure/Building/XRD.csv", header=F, encoding="UTF-8")
s <- read.csv("figure/Building/XRDs.csv", header=F, encoding="UTF-8")
names(d) <- c("x", "y")
names(s) <- c("x", "y")
d$F <- "simulation"
s$F <- "standard"
aa <- rbind(s, d)
pdf("figure/Building/XRD.pdf", width=10, height=5)
ggplot(data=aa) + geom_line(mapping=aes(x, y, color=F, lty=F), size=0.8) + xlab("2-theta") + ylab("Intensity") + theme(text = element_text(size=16), legend.position = "bottom")+ scale_color_manual(values=c("#2074A9","#BB4737", "#74777C"))
dev.off()
