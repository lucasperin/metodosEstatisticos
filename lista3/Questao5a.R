readTableWithoutNA = function(){
  na.omit(read.csv(file="amostraRendaPagamento.csv", head=TRUE, sep=",", na.strings=c("", "NA")))
}


data = readTableWithoutNA()
summary(data)

print("Sumário da renda para Incentivos federais")
renda1 = data[ which(data$PagamentoC == "Incentivos federais"), ]$Renda
summary(renda1)
print("Variância para Incentivos federais")
print(var(renda1))
print("Desvio Padrão para Incentivos federais")
print(sd(renda1))

#summary(data[which(data$PagamentoC = "Incentivos federais"),])

print("Sumário da renda para Outras formas de pagamento")
renda2 = data[ which(data$PagamentoC != "Incentivos federais"), ]$Renda
summary(renda2)
print("Variância para Outras formas de pagamento")
print(var(renda2))
print("Desvio Padrão para Outras formas de pagamento")
print(sd(renda2))


