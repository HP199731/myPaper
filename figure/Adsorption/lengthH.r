library(ggplot2, help, pos = 2, lib.loc = NULL)
a <- c(15.53147, 18.70869, 22.06311, 25.21984)
b <- c(2, 3, 4, 5)

results <- summary(lm(a~b))
a2 <- results$coefficients[1, 1]
b2 <- results$coefficients[2, 1]

n <- seq(2, 5, 1)
p <- b2*n+a2

pdf("figure/Adsorption/lengthH.pdf")
p1 <- ggplot() + geom_line(aes(x=n, y=p), size=2) + geom_point(aes(b, a), color="red", size=4) + ylab("Isosteric heats\n(kcal/mol)") + xlab("Length") + ggtitle("Isosteric heats vs. Length")
p1 + theme(plot.title = element_text(hjust = 0.5), text = element_text(size=16), legend.position = "bottom")
dev.off()
