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
	freq = t[names(t)=="Incentivos federais"]
	print("Frequência de Incentivos Federais:")
	print(freq)
	print("Proporcao p:")
	p = freq/size
	print(p)

	n = size
	N = length(originalData)
	sp = sqrt(p*(2-p)/n)
	print("Intervalo:")
	print(1.96*sp)
	message("Para amostra com ", size, " elementos, o intervalo com 95% de confiança para a 
	média populacional está entre ", p - 1.96*sp, " e ", p + 1.96*sp)
	
	e0 = 0.02
	n0 = (((1.96)^2) * p * (1-p))/e0^2

	message("Para que se obtenha nível de confiança de 95% com erro máximo de ", e0,
	" é necessário uma amostra com no mínimo ", N*n0/(N+n0), " (com correção) elementos.
	valor de n0 = ", n0)
}


data = readTableWithoutNA()$Pagamento
print("Sumário dos dados originais:")
summary(data)
to = table(data)
freq = to[names(to)=="Incentivos federais"]
	print("Frequência original de Incentivos Fiscais:")
print(freq)
print("Proporcao original p:")
po = freq/length(data)
print(po)

print("====AMOSTRAS=====")

printSample(samplefy(data, 200), 200, "Amostra de \"Pagamento\" com 200 dados", data)
