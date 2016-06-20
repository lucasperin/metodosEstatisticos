library(pwr)

poder_do_teste = function(poder){
  pwr.t.test(n = 20,d = poder,sig.level = 0.05,power = NULL,type = "one.sample",alternative = "greater")
}

poder_do_teste((28-27)/1.8238)
poder_do_teste((30-27)/1.8238)
poder_do_teste((31-27)/1.8238)
poder_do_teste((32-27)/1.8238)
poder_do_teste((33-27)/1.8238)
poder_do_teste((34-27)/1.8238)
poder_do_teste((35-27)/1.8238)
