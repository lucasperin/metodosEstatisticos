readTableWithoutNA = function(){
	na.omit(read.csv(file="planilha.csv", head=TRUE, sep=",", na.strings=c("", "NA")))
}

samplefy = function(table, field, size, numOfSamples){
	
	s = double(numOfSamples*size)
	for (i in 1:numOfSamples) {
		currentSample = sample(table, size)
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
	message("<<", numOfSamples, " amostras de tamanho ", size, " do campo ", field, ">>")
	message("Media das amostras: ")
	print(media)
	message("Sumário das amostras: ")
	media
}

printSample = function(sample, title) {
	qqnorm(sample, main=title)
	qqline(sample, col=2)
}


nha = readTableWithoutNA()
summary(nha$Renda)
printSample(samplefy(nha$Renda, "Renda", 4, 200), "Média de 200 amostras de 4 dados aleatórios")
printSample(samplefy(nha$Renda, "Renda", 8, 200), "Média de 200 amostras de 8 dados aleatórios")
printSample(samplefy(nha$Renda, "Renda", 16, 200), "Média de 200 amostras de 16 dados aleatórios")
printSample(samplefy(nha$Renda, "Renda", 30, 200), "Média de 200 amostras de 30 dados aleatórios")
printSample(samplefy(nha$Renda, "Renda", 100, 200), "Média de 200 amostras de 100 dados aleatórios")
