# R does not have any include statement, so let's copy paste that
random_average = function(count) {
    mu = c()

    for(i in 1:1000) {
        x = rnorm(count, -0.3, 1.2)
        s = mean(x)
        mu = c(mu, s)
    }

    return(mu)
}

f_x = function(x) {
    return(1/sqrt(x))
}

plot_functions = function() {
    main_title = "Question 2"
    xlab = "# observations"
    ylab = "Estimator's volatility"
    plot(f_x, main=main_title, col="red", lwd=4, col.main="blue", ylab=ylab, xlab=xlab, xlim=c(0, 10000), ylim=c(0.0, .2))
    i = 0
    legend(x="right", y=.92,
        legend=c("Estimated volatility of the estimator", "1/sqrt(N)"), cex=.8, lty=c(NA, 1),
        col=c("black", "red"), pch=c(3, NA), box.lty=0)
    while (i < 10000){
        points(i, sd(random_average(i)), pch=3)
        i = i+300 # Time to buy a new computer?
    }
}


main = function(){
    plot_functions()
}


main()