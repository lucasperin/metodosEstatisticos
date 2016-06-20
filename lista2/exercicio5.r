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
	t = table(sample)
	print("Frequência da amostra:")
	print(t)
	print("Proporcao da amostra p:")
	print(t/size)
	freq1 = t[names(t)=="Insatisfeito"]
	freq2 = t[names(t)=="Muito insatisfeito"]
	freq = freq1 + freq2
	p = freq/size

	n = size
	N = length(originalData)
	sp = sqrt(p*(2-p)/n)# * sqrt((N-n)/(N-1))
	print("Intervalo:")
	print(1.96*sp)
	message("Para amostra com ", size, " elementos, o intervalo de 95% de confianca para p está entre ", p - 1.96*sp, " e ", p + 1.96*sp)
	
	e0 = 0.02
	n0 = (((1.96)^2) * p * (1-p))/e0^2

	message("Para que se obtenha nível de confiança de 95% com erro máximo de ", e0," é necessário uma amostra com no mínimo ", N*n0/(N+n0), " elementos (corrigido). valor de n0 original = ", n0)

	po = 0.50
	no = (((1.96)^2) * po * (1-po))/e0^2
	message("Para que se obtenha nível de confiança de 95% com erro máximo de ", e0," é necessário uma amostra com no mínimo ", N*no/(N+no), " elementos (corrigido). valor de n0 original = ", no, "e proporcao estimada = ", po)
print("==============================================")
}


data = readTableWithoutNA()$Opinião
print("==============================================")
print("=======Sumário dos dados originais============")
print("==============================================")
to = table(data)
print(to)
print("proporcoes originais:")
print(to/length(data))

print("==============================================")
print("=================AMOSTRAS=====================")

printSample(samplefy(data, 200), 200, "Amostra de Opiniao com 200 dados", data)
