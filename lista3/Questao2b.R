library(pwr)

poder_do_teste = function(delta, sdev){
  power.t.test(n = 20,d = delta, sd = sdev, sig.level = 0.05, power = NULL, type = "one.sample", alternative = "one.sided", strict = FALSE)
}

h0 = 4.5
sdev = 0.77

poder_do_teste(abs(3.50 - h0), sdev)
poder_do_teste(abs(3.75 - h0), sdev)
poder_do_teste(abs(3.85 - h0), sdev)
poder_do_teste(abs(3.95 - h0), sdev)
poder_do_teste(abs(4.15 - h0), sdev)
poder_do_teste(abs(4.25 - h0), sdev)
