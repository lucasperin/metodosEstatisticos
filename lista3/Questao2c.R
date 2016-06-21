#!/usr/bin/Rscript

poder_do_teste = function(delta, sdev, n){
	poder = power.t.test(n = n,d = delta, sd = sdev, sig.level = 0.01, power = NULL, type = "one.sample", alternative = "one.sided", strict = FALSE)
	poder[[5]]
}

h0 = 4.5
sdev = 0.77
n = 40
message(">>>> n = ", n)
poder40 <- c(
poder_do_teste(abs(3.50 - h0), sdev, n),
poder_do_teste(abs(3.75 - h0), sdev, n),
poder_do_teste(abs(3.85 - h0), sdev, n),
poder_do_teste(abs(3.95 - h0), sdev, n),
poder_do_teste(abs(4.15 - h0), sdev, n),
poder_do_teste(abs(4.25 - h0), sdev, n),
poder_do_teste(abs(4.35 - h0), sdev, n),
poder_do_teste(abs(4.5 - h0), sdev, n)
)

n = 50
message(">>>> n = ", n)
poder50 <- c(
poder_do_teste(abs(3.50 - h0), sdev, n),
poder_do_teste(abs(3.75 - h0), sdev, n),
poder_do_teste(abs(3.85 - h0), sdev, n),
poder_do_teste(abs(3.95 - h0), sdev, n),
poder_do_teste(abs(4.15 - h0), sdev, n),
poder_do_teste(abs(4.25 - h0), sdev, n),
poder_do_teste(abs(4.35 - h0), sdev, n),
poder_do_teste(abs(4.5 - h0), sdev, n)
)

n=60
message(">>>> n = ", n)
poder60 <- c(
poder_do_teste(abs(3.50 - h0), sdev, n),
poder_do_teste(abs(3.75 - h0), sdev, n),
poder_do_teste(abs(3.85 - h0), sdev, n),
poder_do_teste(abs(3.95 - h0), sdev, n),
poder_do_teste(abs(4.15 - h0), sdev, n),
poder_do_teste(abs(4.25 - h0), sdev, n),
poder_do_teste(abs(4.35 - h0), sdev, n),
poder_do_teste(abs(4.5 - h0), sdev, n)
)

n=70
message(">>>> n = ", n)
poder70 <- c(
poder_do_teste(abs(3.50 - h0), sdev, n),
poder_do_teste(abs(3.75 - h0), sdev, n),
poder_do_teste(abs(3.85 - h0), sdev, n),
poder_do_teste(abs(3.95 - h0), sdev, n),
poder_do_teste(abs(4.15 - h0), sdev, n),
poder_do_teste(abs(4.25 - h0), sdev, n),
poder_do_teste(abs(4.35 - h0), sdev, n),
poder_do_teste(abs(4.5 - h0), sdev, n)
)

plot(poder40, type="o", col="blue", ylim=c(0, 1), axes=FALSE, ann=FALSE)
lines(poder50, type="o", pch=22, col="red")
lines(poder60, type="o", pch=23, col="green")
lines(poder70, type="o", pch=24, col="black")

axis(1, at=1:8, lab=c(3.5, 3.75, 3.85, 3.95, 4.15, 4.25, 4.35, 4.5))
title(xlab="Média", col.lab="black")

axis(2, las=1, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
title(ylab="Poder do teste", col.lab="black")

legend(1, 1, c("n=40", "n=50", "n=60", "n=70"), cex=0.8, col=c("blue", "red", "green", "black"), pch=21:24, lty=1)

#message("Press Return To Continue")
#invisible(readLines("stdin", n=1))
