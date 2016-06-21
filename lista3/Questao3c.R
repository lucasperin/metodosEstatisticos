library(pwr)

poder_do_teste = function(p, p0) {
	h = ES.h(p, p0)
	poder = pwr.p.test(h, n=NULL, sig.level=0.05, power=0.99, alternative="less")
	poder
} 

poder_do_teste(0.31, 0.35)

