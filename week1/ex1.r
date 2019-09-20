random_average = function(count) {
    mu = c()

    for(i in 1:1000) {
        x = rnorm(count, -0.3, 1.2)
        s = mean(x)
        mu = c(mu, s)
    }

    return(mu)
}

plot_random_densities = function(){
    cond = FALSE
    for(i in c(30, 100, 1000, 10000)){
        randomized_average = random_average(i)
        dens = density(randomized_average)
        #cbind(dens, deviation)
        if(!cond){
            plot(dens)
            cond = TRUE
        }
        else{
            lines(dens)
        }
    }
}

main = function(){
    plot_random_densities()
}


main()