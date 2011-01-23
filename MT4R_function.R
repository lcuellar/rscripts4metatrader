library(XML)
library(fSeries)
library(zoo)
library(fCalendar)
library(chron)

MT4_StratRep_Analyzer = function(...)
# USAGE:    output = MT4_StratRep_Analyzer("gbpusd_set3.htm","EurJpy_Set1.htm", "gbpusd_set2.htm")
#		MT4_DisplaySRAnalysis(output[[3,1]], output[[1,1]]) - See input arguments below for understanding
#		output[[3,x]] will get the TimeSerie for the x-th StrategyReport analyzed
{
	#print(nargs())
	dargs = list(...)

	# Checking Arguments: Only String/Character are accepted (= name of the StrategyReport(s) file(s))
	for (i in 1:length(dargs)) 
	{	
		if (withCallingHandlers(is.na(as.numeric(dargs[i])), warning = function(w) invokeRestart("muffleWarning")))
		{
			
			name = as.character(dargs[i])
			SR_Parsed = MT4_ParseStrategyReport(name) # loading report in memory
			SR_TimeSerie = MT4_CreateTimeSeries_FromStratRep(SR_Parsed);
			MT4_DisplaySRAnalysis(SR_TimeSerie, name);
			
			temp = list(name=name, SR_Parsed=SR_Parsed,SR_TimeSerie=SR_TimeSerie)
			if(i==1){output=temp}
			else{output = cbind(output, temp)}	
		}
		else
		{cat(" MT4 Strategy Report Analyzer receives only string names as argument", "\n",
		 "Argument ", i, " is not of type String/Character. \n Function Exit here. \n\n\n"); stop()}
	}

	SR_CumulBalance = MT4_Aggregate_Reports(output)

	temp = list(name="CumulativeBalance", SR_Parsed=0, SR_TimeSerie=SR_CumulBalance)
	output = cbind(output, temp)

	return(output)
}

MT4_ParseStrategyReport = function(SRname)
{
	x = readHTMLTable(SRname)
	x = x[[2]]
	nms = as.vector(apply(x[1, ], 2, paste, collapse = ''))

	x = x[-(1), ]
	x[2] = gsub('\\.','-',x[[2]])
	names(x) = nms
	return(x)
}

MT4_CreateTimeSeries_FromStratRep= function(x)
# USAGE: output =  CreateTimeSeries (x)
#        zoo(as.numeric(output$Balance), output$timeBalance) to create nonlinear time serie
{
	assign("t", x[ ,2])     # time
	assign("o", x[ ,3])     # order identifier
	assign("lot", x[, 5])   # lot size
	assign("B", x[ ,10])    # balance
	assign("P", x[ ,9])     # relative profit

	P = removeNA(P)
	P = as.numeric(P)

	lot = lot[-which(is.na(P))]
	B = B[-which(is.na(P))]
	t = t[-which(is.na(P))]
	P = removeNA(P)

	lot = as.numeric(as.character(lot))
	P = as.numeric(as.character(P))


	# We identify similar times which corresponds to multiple orders open at the same time and actually
	# add the potential benefits at closing in a single position
		# major reason is that time[i] needs to be unique in a timeSerie
	for (i in 1:(length(t)-1))
	{if (t[i] == t[i+1])
 		{B[i] = NA; P[i+1] = P[i] + P[i+1]; P[i] = 0; lot[i+1] = lot[i]+lot[i+1]; lot[i] = 0}
	}
	
	P[P==0]<-NA
	lot[lot==0]<-NA

	tB  = t[-which(is.na(P))]
	tP  = t[-which(is.na(P))]

	P = removeNA(P)
	B = removeNA(B)
	lot = removeNA(lot)

	list(Balance=B, timeBalance=tB, Profit=P, timeProfit=tP, Lot=lot)
}

MT4_DisplaySRAnalysis = function (TS_asList, ReportName)
{
	BalanceTS = zoo(as.numeric(as.character(TS_asList$Balance)), as.chron(TS_asList$timeBalance) )
	ProfitTS = zoo(as.numeric(as.character(TS_asList$Profit)), as.chron(TS_asList$timeBalance) )

	windows(title = ReportName)
	plot.new()
	nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE), c(3,1), c(1,3), TRUE) #layout.show(nf)
	MinTime = min(as.chron(TS_asList$timeBalance), as.chron(TS_asList$timeProfit))
	MaxTime = max(as.chron(TS_asList$timeBalance), as.chron(TS_asList$timeProfit))
	plot(BalanceTS, ylim=c(min(ProfitTS[,1]), max(BalanceTS[,1])),xlim=c(MinTime, MaxTime) )

	par(new=TRUE)
	plot(ProfitTS, col=3, ylim=c(min(ProfitTS[,1]), max(BalanceTS[,1])),xlim=c(MinTime, MaxTime) )
	legend(5,max(BalanceTS[,1]), legend=c('Balance', 'Relative Profits'), lty=1, col=c("black","green"))
}

MT4_Aggregate_Reports = function (oo)
{
	NbReports = length(oo[1,])
	for (i in 1:NbReports)
	{
		if(i==1){AccProfit = zoo(oo[[3,i]]$Profit, as.chron(oo[[3,i]]$timeBalance))}
		else {AccProfit = merge(AccProfit, i=zoo(oo[[3,i]]$Profit, as.chron(oo[[3,i]]$timeBalance)), fill=0) }	
	}
	
	# Summing every profit with respect to tie stamp
	temp = AccProfit[,2]+AccProfit[,1]+AccProfit[,3] # temp = as.numeric(AccProfit[,1])

	# Constructing the Accrual of the reports.
	CumulativeBalance  = c(2500) + as.numeric(temp[1]);
	for (i in 2:length(temp))
		{CumulativeBalance[i] = CumulativeBalance[i-1] + as.numeric(temp[i])}
	CumulativeBalance = zoo(CumulativeBalance, as.chron(time(temp)) )
	CumulativeBalance = merge(CumulativeBalance, temp);

	windows(title = "Cumulative Reports")
	plot.new()
	plot(CumulativeBalance)

	return (CumulativeBalance);

}

#par(mar=c(3,3,1,1)) 
#plot(Balance, main = "Balance", ylab = "Unit")
#den <- density(Profit)
#par(new = TRUE)
#plot(den, xlab = "", yaxt = "n", ylab = "", main = "", col = "red", axes=TRUE)
#par(mar=c(0,3,1,1))
#barplot(LotSize, main = "Lot Size")
#par(mar=c(3,0,1,1)) 
#plot(Profit, main = "Relative Profit")

