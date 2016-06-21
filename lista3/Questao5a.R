readTableWithoutNA = function(){
  na.omit(read.csv(file="amostraRendaPagamento.csv", head=TRUE, sep=",", na.strings=c("", "NA")))
}


data = readTableWithoutNA()
print("Sumário da renda para Incentivos federais")
summary(data[ which(data$PagamentoC == "Incentivos federais"), ]$Renda)

#summary(data[which(data$PagamentoC = "Incentivos federais"),])

print("Sumário da renda para Outras formas de pagamento")
summary(data[ which(data$PagamentoC != "Incentivos federais"), ]$Renda)


