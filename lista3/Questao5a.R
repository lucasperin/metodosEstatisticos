readTableWithoutNA = function(){
  na.omit(read.csv(file="amostraRendaPagamento.csv", head=TRUE, sep=",", na.strings=c("", "NA")))
}


data = readTableWithoutNA()
print("Sumário da renda para Incentivos federais")
renda1 = data[ which(data$PagamentoC == "Incentivos federais"), ]$Renda
summary(renda1)
print(var(renda1))

#summary(data[which(data$PagamentoC = "Incentivos federais"),])

print("Sumário da renda para Outras formas de pagamento")
renda2 = data[ which(data$PagamentoC != "Incentivos federais"), ]$Renda
summary(renda2)
print(var(renda2))


