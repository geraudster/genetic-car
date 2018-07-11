team <- 'RED'

## Generate population
randomVecteur <- function () {
    (runif(16) + 0.1) * c(1,0,1,1,0,1,-1,1,-1,0,-1,-1,0,-1,1,-1)
}

randomChassis <- function() {
    c(randomVecteur(), runif(1) * 270 + 30)
}

randomWheel <- function() {
    c(runif(1) * 0.3 + 0.2,
      runif(1) * 60 + 40,
      round(runif(1) * 7))
}

randomCar <- function() {
    c(randomChassis(),
      randomWheel(),
      randomWheel())
}

carView <- function(carVector) {
    list(chassis = list(
             vecteurs = carVector[1:16],
             densite = carVector[17]),
         wheel1 = list(
             radius = carVector[18],
             density = carVector[19],
             vertex = carVector[20]),
         wheel2 = list(
             radius = carVector[21],
             density = carVector[22],
             vertex = carVector[23]))
}

library(rjson)
cat(toJSON(lapply(1:20, function(x) carView(randomCar()))))

## Initial population
cars <- lapply(1:20, function(x) carView(randomCar()))

## Evaluate population
library(httr)

evaluate <- function(cars) {
    #appUrl <- 'http://genetic-car.herokuapp.com'
    appUrl <- 'http://localhost:8080'
    r <- POST(paste(appUrl, 'simulation/evaluate', team, sep = '/'),
              body = cars,
              encode = 'json',
              verbose = TRUE)
    content(r)
}

maxByScore <- function(car1, car2) {
    if(car1$score > car2$score)
        car1
    else
        car2
}

library(ggplot2)


initialCars <- cars
numIter <- 100
scoreHistory <- rep(NA, numIter)
cars <- initialCars

for(i in 1:numIter) {
    evaluations <- evaluate(cars)
    carScores <- Filter(function(x) {x$score > 0}, evaluations)
    champion <- Reduce(maxByScore, carScores)
    scoreHistory[i] <- champion$score
    print(ggplot(data.frame(x=1:numIter, y=scoreHistory), aes(x=x, y=y)) + geom_line())

## Individual selection

    selection <- lapply(1:10, function (x) {
        list(firstParent = champion,
             secondParent = Reduce(maxByScore, sample(carScores, 3)),
             thirdParent = Reduce(maxByScore, sample(carScores, 3)))
    })
    

    ## Reproduction
    split1 <- 6
    split2 <- 17

    newGen <- lapply(selection, function(pair) {
        first <- unlist(pair$firstParent, use.names = FALSE)
        second <- unlist(pair$secondParent, use.names = FALSE)
        third <- unlist(pair$thirdParent, use.names = FALSE)
        list(firstBaby <- carView(c(first[1:split1], second[(split1+1):split2], first[(split2+1):23])),
             secondBaby <- carView(c(third[1:split1], first[(split1+1):split2], third[(split2+1):23])),
             thirdBaby <- carView(c(second[1:split1], third[(split1+1):split2], second[(split2+1):23])))
    })
    cars <- sample(unlist(newGen, recursive = FALSE), 20)
    cars[[1]] <- champion$car


    ## Mutation
    mutant <- unlist(champion, use.names = FALSE)
    mutant <- c(randomChassis(), mutant[18:23])
    cars[[2]] <- carView(mutant)
    mutant <- unlist(champion, use.names = FALSE)
    mutant <- c(mutant[1:17], randomWheel(), mutant[21:23])
    cars[[3]] <- carView(mutant)
    mutant <- unlist(champion, use.names = FALSE)
    mutant <- c(mutant[1:20], randomWheel())
    cars[[4]] <- carView(mutant)
}


