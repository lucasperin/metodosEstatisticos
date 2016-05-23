readTableWithoutNA = function(){
	na.omit(read.csv(file="planilha.csv", head=TRUE, sep=",", na.strings=c("", "NA")))
}

samplefy = function(table, size){
	
	s = double(size)
	sample = sample(table, size, replace=TRUE)
	sample
}

printSample = function(sample, size, title, originalData) {
	print("==============================================")
	print(title)
	print(summary(sample))
	t = table(sample)
	freq1 = t[names(t)=="Insatisfeito"]
	print("Frequência original:")
	print(freq1)
	freq2 = t[names(t)=="Muito insatisfeito"]
	print("Frequência original:")
	print(freq2)
	freq = freq1 + freq2
	print("Proporcao p:")
	p = freq/size
	print(p)

	n = size
	N = length(originalData)
	sp = sqrt(p*(2-p)/n) * sqrt((N-n)/(N-1))
	print("Intervalo:")
	print(1.96*sp)
	message("Para amostra com ", size, " elementos, o intervalo de 95% de confianca para p
	está entre ", p - 1.96*sp, " e ", p + 1.96*sp)
	
	e0 = 0.02
	n0 = (((1.96)^2) * p * (1-p))/e0^2

	message("Para que se obtenha nível de confiança de 95% com erro máximo de ", e0,
	" é necessário uma amostra com no mínimo ", N*n0/(N+n0), " elementos (corrigido).
	valor de n0 original = ", n0)
}


data = readTableWithoutNA()$Opinião
print("Sumário dos dados originais:")
summary(data)
to = table(data)
freqo1 = to[names(to)=="Insatisfeito"]
	print("Frequência original:")
print(freqo1)
freqo2 = to[names(to)=="Muito insatisfeito"]
	print("Frequência original:")
print(freqo2)
freqo = freqo1 + freqo2
print("Proporcao original p:")
po = freqo/length(data)
print(po)

print("====AMOSTRAS=====")

printSample(samplefy(data, 200), 200, "Amostra de \"Pagamento\" com 200 dados", data)