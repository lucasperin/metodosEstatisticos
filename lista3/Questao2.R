#le os dados 
data = read.csv(file = "amostra2.csv")

#imprime um sumario dos dados
summary(data$x)
library(e1071)
kurtosis(data$X)
skewness(data$x)

#retorna a quantidade de elementos da amostra
n = length(data$x)

#hipotese nula
H0 = 4.5

#nivel de significancia
alpha = 0.01

#hipotese a ser testada
xbar = mean(data$x)
xbar

#desvio padrao
s = sd(data$x)
s

#graus de liberdade t student
df = n-1
df

#cálculo de t(n-1) grais de liberdade
t = ((xbar-H0)*sqrt(n))/s 
t

#valor de probabilidade P(t(n-1)>t) distribuição central
p1 = pt(t, df, lower.tail = FALSE, log.p = FALSE)
p1

#valor de probabilidade P(t(n-1)>t) distribuição NAO central
ncp = (H0-xbar)*sqrt(n)/s
ncp
p2 = pt(t, df, ncp, lower.tail = FALSE, log.p = FALSE)
p2

if (p1>alpha){print("Aceita-se H0")}else{print("rejeita-se H0")}

if (p2>alpha){print("Aceita-se H0")}else{print("rejeita-se H0")}


