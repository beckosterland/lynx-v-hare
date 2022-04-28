# GEOG 264 Lab 4 Becket Osterland
# Estimating Canadian lynx and snowshoe hare populations over time


#create flag = TRUE
playflag <- TRUE

#while loops if playflag is TRUE
#initial prompt to run the simulation
while(playflag == TRUE){
  dummy4 <- readline(prompt = "If you want to run simulation, type '1', to quit type '0' : ")
  response <- as.integer(dummy4)
  #inner while loop runs if simulation prompt is 1
  #breaks if invalid number of years is prompted (eg. 0, -3)
  #prompts population parameters using readline as numeric values
  while ((response != 0)&(response == 1)) {
    dummy <- readline(prompt = "Input the number of years: ")
    n_years <- as.numeric(dummy)
    if(n_years < 1){
      break()
    }
    dummy2 <- readline(prompt = "Input the current lynx population: ")
    n_lynx <- as.numeric(dummy2)
    dummy3 <- readline(prompt = "Input the current snowshoe hare population: ")
    n_hare <- as.numeric(dummy3)
    #print header for values each time simulation is run
    cat("Year", " ", "n_Hare"," ", "n_Lynx"," ", "babyH"," ", "babyL"," ", "deadH"," ", "deadL"," ", "eatenH", "\n")
    #create empty vectors to eventually create plot with later
    lynx_vect <- c()
    hare_vect <- c()
    
    #for loop that runs iteratively through each year and calculates values
    for (Year in 1:n_years) {
      babyh <- round(n_hare * 0.75)
      babyl <- round(n_lynx * 0.15)
      deadh <- round(n_hare *0.01)
      eatenh <- round(n_lynx * n_hare * 0.025)
      #if statement to determine whether normal or starvation year
      if(n_lynx/n_hare <= 0.20){
        deadl <- round(n_lynx * 0.02)
      } else{
        deadl <- round(n_lynx * 0.5)
      }
      #prints values each year to align with header
      cat(" ",Year," ",n_hare," ",n_lynx," ",babyh," ",babyl," ",deadh," ",deadl," ",eatenh, '\n')
      #compute new total populations for next year
      n_hare <- n_hare + babyh - deadh - eatenh
      n_lynx <- n_lynx + babyl - deadl
      lynx_vect <- append(lynx_vect,n_lynx)
      hare_vect <- append(hare_vect,n_hare)
      #if/break statement assuring at least two of each species is present for population regeneration
      if((n_hare < 2)|(n_lynx < 2)){
        cat("The lynx/hare population has dropped below two, the population cannot be sustained")
        break()
      }
    }
    #make plot for each simulation taking the vector of each species and plotting it with years
    #make y axis limit to the maximum number of hare so the limits are always correct
    #add points and legend for visual clarity
    plot(1:n_years,lynx_vect, main = "Canadian Snowshoe Hare and Lynx Population Simulation", type =
           "l", xlab = "Year", ylab = "Population", lwd = 2, col = "gray25", ylim =c(0,max(hare_vect)))
    points(1:n_years,lynx_vect, pch = 17, col = "gray25")
    points(1:n_years,hare_vect, type = "l", lwd = 2, col = "goldenrod2")
    points(1:n_years,hare_vect, pch = 16, col = "goldenrod2")
    legend("topright", legend=c("Lynx Population","Hare Population"),
           pch = c(17,16), col = c("gray25","goldenrod2"))
    #flag becomes false so when another simulation is not wanted/years input is less than 1
    #all loops are broken out of
    playflag <- FALSE
  }
}
