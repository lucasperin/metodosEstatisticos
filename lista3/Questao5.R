readTableWithoutNA = function(){
  na.omit(read.csv(file="../planilha.csv", head=TRUE, sep=",", na.strings=c("", "NA")))
}

data = readTableWithoutNA()

smpl = data[sample(1:nrow(data), 80, replace=TRUE),]


smpl$Pagamento = as.character(smpl$Pagamento)
smpl$Pagamento[smpl$Pagamento != "Incentivos federais"] <- "Outras formas de pagamento"

RowNames = c("Renda", "PagamentoC")
newData = smpl[,c("Renda", "Pagamento")]

write.csv(newData, file = "amostraRendaPagamento.csv")
