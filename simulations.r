# global imports
require("gplots")


# bitcoin 8h last 3 years generator
price_file='BTCUSDT-8h'
filename=paste('db/prices/', price_file, '.csv', sep="")
opens = read.csv(filename, sep='\t')$OPEN

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
        cat('at', p)
        probResults = cbind(probResults, montecarlo(s, p, days=days, n_iter=n_iter))
    }
    heat = NULL
    least = min(probResults)
    last = max(probResults)
    breaks = seq(floor(least * 20)/ 20, ceiling(last * 20) / 20, by=0.05)
    cat('breaks is ', breaks, '\n')
    for (col in 1:ncol(probResults)) {
        to_append =  hist(probResults[,col], plot=FALSE, breaks=breaks)$count
        heat = cbind(heat,to_append)
    }
    colnames(heat) <- problist
    rownames(heat) <- round((breaks[1:length(breaks) - 1] - 1) * 100)
    heat <- heat[nrow(heat):1,]
    
    #also return the averages
    avg = round((colMeans(probResults) - 1) * 100)
    return (list(heat=heat, avg=avg, axisx=problist))
}
    


# run simulations
main <- function(days=30,n_iter=1000){
    s = Simulation()
    r = plotHeat(s, seq(0.50, 0.82, by=0.005), days=days, n_iter=n_iter)

    # save heatmap
    png(filename=paste("db/plots/hm", price_file, ".png", sep=""),
        height=1200,
        width=1200,
        units='px',
        res=250,
        bg="transparent")

    finalheat = heatmap.2(r$heat,
              xlab = "A.I. accuracy",
              ylab = "Profit (%)",
              dendrogram='none',
              Rowv=FALSE,
              key = FALSE,
              col=bluered,
              Colv=FALSE,
              sepwidth=c(0,0.05),
              rowsep=1:nrow(r$heat),
              sepcolor='black',
              trace='none')
    dev.off()
    xaxis = as.numeric(rownames(finalheat$carpet))
    yaxis = as.numeric(colnames(finalheat$carpet))


    # save average plot
    png(filename=paste("db/plots/avg", price_file, ".png", sep=""),
        height=1200,
        width=1200,
        units='px',
        res=250,
        bg="transparent")

    op <- par(mar = c(5,4,4,4) + 0.1)
    lineplot = plot(r$axisx,
                    r$avg,
                    type='l',
                    xlab = "A.I. accuracy",
                    ylab = "",
                    axes=FALSE,
    )
    print(yaxis)
    axis(side=4, at=yaxis, tick=FALSE)
    mtext("Profit (%)", side=4, line=3)
    axis(side=1, at=xaxis, las=2, tick=FALSE, labels=rownames(finalheat$carpet))
    grid(col="lightgray")
    par(op)
    dev.off()
    return (list(heat=finalheat, avg=lineplot, r=r))
}



