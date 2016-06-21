library(pwr)

poder_do_teste = function(p, p0) {
	h = ES.h(p, p0)
	poder = pwr.p.test(h, n=NULL, sig.level=0.01, power=0.99, alternative="greater")
	poder
} 

poder_do_teste(0.23, 0.20)

