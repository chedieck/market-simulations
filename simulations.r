# global imports
require("gplots")


# bitcoin 8h last 3 years generator
opens = read.csv('db/prices/BU-8h.csv', sep='\t')$OPEN

# logreturn function to compute logreturns
logreturn <- function(P1, P2){
    return (log(P2/P1))
} 

# dataset with the logreturns
LR = mapply(logreturn, P1=opens[1:length(opens) -1], P2=opens[2:length(opens)])

# class to create simulation using a preset fee and a set
# from which the logreturns will be sampled.
# H is the interval between trades, in hours
Simulation <- function(fee=0.00075, sample_set=LR, H=8) {
    value <- list(fee=fee, saldo=1, bought=0, sample_set=LR, H=H) 
    class(value) <- "Simulation"
    value
}


# Simulate fee payment.
toggleFee <- function(s,...) UseMethod("toggleFee")
toggleFee.Simulation <- function(s, newside, loglucro=0) {
    if (newside != s$bought){
        s$saldo = s$saldo * (1 - s$fee)
    s$saldo = s$saldo * exp(loglucro)
    s$bought = newside
    }
    return (s)
}

# Run an online market prediction.
run <- function(s, ...) UseMethod("run")
run.Simulation <- function(s, prob){
    logreturnSample = sample(s$sample_set, 1)
    dice = runif(1)
    # prediction was right
    if (dice <= prob) {
        if (logreturnSample <= 0) { #  price went down
            s = toggleFee(s, 0)     #  sold
        }
        else {                      #  price went up
            s = toggleFee(s, 1, loglucro=logreturnSample) #  bought
        }
    }

    # prediction was wrong
    else { 
        if (logreturnSample >= 0) { #  prince went up
            s = toggleFee(s, 0)     #  sold
        }
        else {                      #  price went down
            s = toggleFee(s, 1, loglucro=logreturnSample) #  bought
        }
    }
    return (s)
}

# Run multiple market predictions for --days
longRun <- function(s,...) UseMethod("longRun")
longRun.Simulation <- function(s, prob, days=31) {
        s$saldo = 1
        s$bought = 0
        i = 0
        d = s$H/24
        while (i < days){
            s = run(s, prob)
            i = i + d
        }
        return (s$saldo)
}


# Simulate many longRuns to get ditribution of performance.
montecarlo <- function(s, ...) UseMethod("montecarlo")
montecarlo.Simulation <- function(s, prob, days=31, n_iter=1000) {
    outvec = NULL
    for (i in 1:n_iter){
        trial = longRun(s, prob, days=days)
        outvec = c(outvec, trial)
    }
    return (outvec)
}


plotHeat <- function(s,...) UseMethod("plotHeat")
plotHeat <- function(s, problist, days=31, n_iter=1000) {
    probResults = NULL
    for (p in problist) {
        probResults = cbind(probResults, montecarlo(s, p, days=days, n_iter=n_iter))
    }
    m = NULL
    least = min(probResults)
    last = max(probResults)
    cat('last is ', last, ', least is ', least)
    breaks = seq(floor(least * 20)/ 20, ceiling(last * 20) / 20, by=0.05)
    cat('breaks is ', breaks)
    for (col in 1:ncol(probResults)) {
        to_append =  hist(probResults[,col], plot=FALSE, breaks=breaks)$count
        print(to_append)
        m = cbind(m,to_append)
    }
    colnames(m) <- problist
    rownames(m) <- (breaks[1:length(breaks) - 1] - 1) * 100
    m <- m[nrow(m):1,]
    return (list(m=m, res=probResults))
}
    


# run simulations

main <- function(days=15,n_iter=1000){
    s = Simulation()
    r = plotHeat(s, seq(0.54, 0.65, by=0.005), days=days, n_iter=n_iter)

    png(filename="temp.png")
    heatmap.2(r$m,
              main = paste("Market profits after", days, "days,\neach x simulated", n_iter, "times."),
              xlab = "A.I. accuracy",
              ylab = "Profit (%)",
              dendrogram='none',
              Rowv=FALSE,
              key = FALSE,
              Colv=FALSE,
              sepwidth=c(0,0.05),
              rowsep=1:nrow(r$m),
              sepcolor='black',
              trace='none')
    dev.off()
}



