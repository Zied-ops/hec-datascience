random_average = function(count) {
    mu = c()

    for(i in 1:1000) {
        x = rnorm(count, -0.3, 1.2)
        s = mean(x)
        mu = c(mu, s)
    }

    return(mu)
}

plot_random_densities = function(counts, colors){
    cond = FALSE
    for(i in 1:length(counts)){ # In R we count from 1 for some reason
        randomized_average = random_average(counts[i])
        dens = density(randomized_average)
        if(!cond){
            plot(dens, main="Question 1", col.main="blue", ylab="Density", xlim=c(-1, .5), ylim=c(0, 35), col=colors[i])
            cond = TRUE
        }
        else{
            lines(dens, col=colors[i])
        }
    }
    legend(x="right", y=.92,
        legend=paste(counts, "observations"), cex=.8, lty=1,
        col=colors, box.lty=0)
}

main = function(){
    counts = c(10000, 1000, 100, 30)
    colors = c("red", "green", "blue", "yellow")
    plot_random_densities(counts, colors)
}


main()
