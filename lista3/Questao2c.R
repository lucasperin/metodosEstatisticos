library(pwr)

poder_do_teste = function(delta, sdev){
  power.t.test(n = n,d = delta, sd = sdev, sig.level = 0.01, power = NULL, type = "one.sample", alternative = "one.sided", strict = FALSE)
}

h0 = 4.5
sdev = 0.77
n = 40

poder_do_teste(abs(3.50 - h0), sdev, n)
poder_do_teste(abs(3.75 - h0), sdev, n)
poder_do_teste(abs(3.85 - h0), sdev, n)
poder_do_teste(abs(3.95 - h0), sdev, n)
poder_do_teste(abs(4.15 - h0), sdev, n)
poder_do_teste(abs(4.25 - h0), sdev, n)
poder_do_teste(abs(4.35 - h0), sdev, n)

n = 50
poder_do_teste(abs(3.50 - h0), sdev, n)
poder_do_teste(abs(3.75 - h0), sdev, n)
poder_do_teste(abs(3.85 - h0), sdev, n)
poder_do_teste(abs(3.95 - h0), sdev, n)
poder_do_teste(abs(4.15 - h0), sdev, n)
poder_do_teste(abs(4.25 - h0), sdev, n)
poder_do_teste(abs(4.35 - h0), sdev, n)

n=60
poder_do_teste(abs(3.50 - h0), sdev, n)
poder_do_teste(abs(3.75 - h0), sdev, n)
poder_do_teste(abs(3.85 - h0), sdev, n)
poder_do_teste(abs(3.95 - h0), sdev, n)
poder_do_teste(abs(4.15 - h0), sdev, n)
poder_do_teste(abs(4.25 - h0), sdev, n)
poder_do_teste(abs(4.35 - h0), sdev, n)

n=70
poder_do_teste(abs(3.50 - h0), sdev, n)
poder_do_teste(abs(3.75 - h0), sdev, n)
poder_do_teste(abs(3.85 - h0), sdev, n)
poder_do_teste(abs(3.95 - h0), sdev, n)
poder_do_teste(abs(4.15 - h0), sdev, n)
poder_do_teste(abs(4.25 - h0), sdev, n)
poder_do_teste(abs(4.35 - h0), sdev, n)
