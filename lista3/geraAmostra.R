readTableWithoutNA = function(){
  na.omit(read.csv(file="planilha.csv", head=TRUE, sep=",", na.strings=c("", "NA")))
}

samplefy = function(table, size){
  
  s = double(size)
  sample = sample(table, size, replace=TRUE)
  write.csv(sample, file = "amostra2.csv")
  sample
}

data = readTableWithoutNA()
print("Sumário dos dados originais:")
summary(data$Renda)

print("Variância")

print(var(data$Renda))
print("====AMOSTRAS=====")

samplefy(data$Renda, 20)

