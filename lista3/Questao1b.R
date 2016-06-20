library(pwr)

poder_do_teste = function(delta, sdev){
  power.t.test(n = 20,d = delta, sd = sdev, sig.level = 0.05, power = NULL, type = "one.sample", alternative = "one.sided", strict = FALSE)
}

h0 = 27
sdev = 1.8238

poder_do_teste(28-h0, sdev)
poder_do_teste(30-h0, sdev)
poder_do_teste(31-h0, sdev)
poder_do_teste(32-h0, sdev)
poder_do_teste(33-h0, sdev)
poder_do_teste(34-h0, sdev)
poder_do_teste(35-h0, sdev)
