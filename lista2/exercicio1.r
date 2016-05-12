readTableWithoutNA = function(){
	na.omit(read.csv(file="planilha.csv", head=TRUE, sep=",", na.strings=c("", "NA")))
}

samplefy = function(table, size, numOfSamples){
	
	s = double(numOfSamples*size)
	for (i in 1:numOfSamples) {
		currentSample = sample(table, size, replace=TRUE)
		for (j in 1:size) {
			index = (i-1)*size + j
			s[index] = currentSample[j]
		}
	}
	dim(s) = c(size, numOfSamples)
	media = double(numOfSamples)
	for (i in 1:numOfSamples) {
		media[i] = mean(s[,i])
	}
	media
}

printSample = function(sample, size, title, originalData) {
	qqnorm(sample, main=title)
	qqline(sample, col=2)
	hist(sample, main=title)
	print("==============================================")
	print(title)
	print(sample)
	print("Sumário das amostras: ")
	print(summary(sample))
	print("Média esperada: ")
	print(mean(originalData))
	print("Variância: ")
	print(var(sample))
	print("Variância esperada (original/n)")
	print(var(originalData)/size)
	print("Desvio Padrão")
	print(sd(sample))
	print("Desvio Padrão esperado")
	print(sd(originalData)/sqrt(size))
}


data = readTableWithoutNA()
print("Sumário dos dados originais:")
summary(data$Renda)
print("Variância")
print(var(data$Renda))
print("====AMOSTRAS=====")

printSample(samplefy(data$Renda, 4, 200), 4, "Média de 200 amostras com 4 dados", data$Renda)
printSample(samplefy(data$Renda, 8, 200), 8, "Média de 200 amostras com 8 dados", data$Renda)
printSample(samplefy(data$Renda, 16, 200), 16, "Média de 200 amostras com 16 dados", data$Renda)
printSample(samplefy(data$Renda, 30, 200), 30, "Média de 200 amostras com 30 dados", data$Renda)
printSample(samplefy(data$Renda, 100, 200), 100, "Média de 200 amostras com 100 dados", data$Renda)
printSample(samplefy(data$Renda, 200, 200), 200, "Média de 200 amostras com 200 dados", data$Renda)
