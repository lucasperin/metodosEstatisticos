library(pwr)

poder_do_teste = function(p, p0) {
	h = ES.h(p, p0)
	poder = pwr.p.test(h, n=200, sig.level=0.01, power=NULL, alternative="greater")
	poder[[4]]
} 

poder = c(
poder_do_teste(0.20, 0.20),
poder_do_teste(0.21, 0.20),
poder_do_teste(0.22, 0.20),
poder_do_teste(0.23, 0.20),
poder_do_teste(0.24, 0.20),
poder_do_teste(0.25, 0.20),
poder_do_teste(0.26, 0.20),
poder_do_teste(0.27, 0.20)
)
print(poder)

png("poderDoTesteProporcao2.png")
plot(poder, type="o", col="blue", ylim=c(0,1), axes=FALSE, ann=FALSE)

axis(1, at=1:8, lab=c(0.2, 0.21, 0.22, 0.23, 0.24, 0.25, 0.26, 0.27))
title(xlab="Média", col.lab="black")

axis(2, las=1, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
title(ylab="Poder do teste", col.lab="black")

