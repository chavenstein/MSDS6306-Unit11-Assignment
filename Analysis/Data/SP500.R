#Install the tseries package
#install.packages("tseries")

#load the packages
library(tseries)
library(zoo)


#Download the data
SNPdata <- get.hist.quote('^gspc',quote="Close")

#calculate the log returns
SNPret <- log(lag(SNPdata)) - log(SNPdata)

#calculate the volitily measure
SNPvol <- sd(SNPret) * sqrt(250) * 100


## volatility
getVol <- function(d, logrets)
{

	var = 0

	lam = 0

	varlist <- c()

	for (r in logrets) {

		lam = lam*(1 - 1/d) + 1
	
	var = (1 - 1/lam)*var + (1/lam)*r^2

		varlist <- c(varlist, var)

	}

	sqrt(varlist)
}


# Recreate Figure 6.12 in the text on page 155

volest <- getVol(10,SNPret)
volest <- zoo(volest, order.by = index(SNPdata))

volest2 <- getVol(30,SNPret)
volest2 <- zoo(volest2, order.by = index(SNPdata))

volest3 <- getVol(100,SNPret)
volest3 <- zoo(volest3, order.by = index(SNPdata))

plot(volest,type="l", col="blue", main="Volatility in the S&P with different decay factors", xlab="year")

lines(volest2,type="l",col="springgreen3")

lines(volest3, type = "l", col="red2")

legend("topright",c("decay factor 0.90","decay factor 0.97","decay factor 0.99"), lty=1, col = c("blue","springgreen3","red2"),bty='n', cex=.70)
