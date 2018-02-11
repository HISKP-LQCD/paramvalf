# Some sample data in the format that we need.
two <- list(param = data.frame(L = c(24),
                               shift = c(TRUE, FALSE)),
            value = data.frame(mass_two = c(3.0, 5.4)))

four <- list(param = data.frame(L = c(24),
                                vacuum = c(TRUE, FALSE)),
             value = data.frame(mass_four = c(-33.1, 40),
                                other_four = c('A', 'B')))

# A function that generates a new data point.
delta_E <- function(param, value) {
    result = value$mass_two + value$mass_four

    if (param$vacuum) {
        result <- 2 * result
    }

    data.frame(delta_E = result,
               other = 3.1)
}


res <- call.pvcontainer(delta_E, two, four)
print(res)
