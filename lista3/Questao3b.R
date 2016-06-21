library(pwr)

poder_do_teste = function(p, p0) {
	h = ES.h(p, p0)
	poder = pwr.p.test(h, n=200, sig.level=0.05, power=NULL, alternative="less")
	poder[[4]]
} 

poder = c(
poder_do_teste(0.28, 0.35),
poder_do_teste(0.29, 0.35),
poder_do_teste(0.3, 0.35),
poder_do_teste(0.31, 0.35),
poder_do_teste(0.32, 0.35),
poder_do_teste(0.33, 0.35),
poder_do_teste(0.34, 0.35),
poder_do_teste(0.35, 0.35)
)

pdf("podeDoTesteProporcao.pdf")
plot(poder, type="o", col="blue", ylim=c(0,1), axes=FALSE, ann=FALSE)

axis(1, at=1:8, lab=c(2.8, 2.9, 3, 3.1, 3.2, 3.3, 3.4, 3.5))
title(xlab="Média", col.lab="black")

axis(2, las=1, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
title(ylab="Poder do teste", col.lab="black")

