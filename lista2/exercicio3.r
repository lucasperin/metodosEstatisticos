readTableWithoutNA = function(){
	na.omit(read.csv(file="../planilha.csv", head=TRUE, sep=",", na.strings=c("", "NA")))
}

samplefy = function(table, size){
	
	s = double(size)
	sample = sample(table, size, replace=TRUE)
	sample
}

printSample = function(sample, size, title, originalData) {
	print("==============================================")
	print(title)
	print("Média")
	print(mean(sample))
	print("Média esperada: ")
	print(mean(originalData))

	n = size
	s = sd(sample)
	x = mean(sample)
	sx = s / sqrt(n)
	print("Desvio padrão:")
	print(s)
	print("Intervalo com 95% de confiança:")
	print(2.093 * sx)
	print("Intervalo com 99% de confiança:")
	print(2.861 * sx)
	message("Para amostra com ", size, " elementos, o intervalo com 95% de confiança para a 
	média populacional está entre ", x - 2.093*sx, " e ", x + 2.093*sx)
	message("Para amostra com ", size, " elementos, o intervalo com 99% de confiança para a 
	média populacional está entre ", x - 2.861*sx, " e ", x + 2.861*sx)
	e = 1.5
	message("Para que se obtenha nível de confiança de 99% com erro máximo de ", e,
	" é necessário uma amostra com no mínimo ", (2.861*s/e)^2, " elementos.")
}


data = readTableWithoutNA()
print("Sumário dos dados originais:")
summary(data$Renda)
print("Variância")
print(var(data$Renda))
print("====AMOSTRAS=====")

printSample(samplefy(data$Renda, 20), 20, "Média de uma amostra com 20 dados", data$Renda)
