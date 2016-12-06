playerProfile <- function(data){
  player <- list()
  x <- 1:10
  player$idsubj <- data$idsubj[1]
  player$idtyp <- data$idtyp[1]
  
  player$contribution <- data$contribution
  player$contribMean <- mean(data$contribution)
  player$contiribReg <- lm(player$contribution ~ x)
  player$contiribSlop <- player$contiribReg$coefficients[[2]]
  
  
  player$belief <- data$belief
  player$beliefMean <- mean(data$belief)
  player$beliefReg <- lm(player$belief ~ x)
  player$beliefSlop <- player$beliefReg$coefficients[[2]]
  
  x <- 0:20
  y <- data[1, ]
  player$initial <- c(y$b0, y$b1, y$b2, y$b3, y$b4, y$b5, 
                      y$b6, y$b7, y$b8, y$b9, y$b10, 
                      y$b11, y$b12, y$b13, y$b14, y$b15, 
                      y$b16, y$b17, y$b18, y$b19, y$b20)
  
  player$initialMean <- mean(player$initial)
  player$initialReg <- lm(player$initial ~ x)
  player$initialSlop <- player$initialReg$coefficients[[2]]
  
  player$initialDeviation <- data$initialDeviation
  player$initialDeviationMean <- data$meanInitialDeviation[1]
  player$lableAccuracy <- data$lableAccuracy[1]
  
  player$predictionAccuracy <- data$predictionAccuracy
  player$predictionAccuracySD <- data$SDpredictionAccuracy[1]
  
  player$predictedcontribution <- data$predictedcontribution
  
  player$payoff <- data$payoff
  
  player$newClass <- 7
  
  player
}

plotPlayerProfile <- function(player){
  s <- player$idsubj
  png(paste0('P',s ,'.png'), width=800, height=400)
  par(mfrow= c(1, 2))
  
  # Plot 
  plot(0:20, player$initial, main=paste0("predisposition for contribution\n class = ",player$idtyp ), 
       ylim=c(0,20), xlim=c(0,20), ylab="claiming Contribution", 
       xlab=paste0("Assuming Contribution of other players\n",
                   "Claim contribution (mean = ", round(player$initialMean, 3),
                   ", reg = ", round(player$initialSlop, 3), ")"))
  lines(0:20, player$initial)
  
  abline(player$initialReg)
  
  # plot beleif 
  plot(1:10, player$belief, xlim=c(1, 10), 
       xlab= paste("Time\nMean of (contrib=", round(player$contribMean, 3), 
                   " ,belief=", round(player$beliefMean, 3), ")"),
       ylab="player Belief", ylim=c(0, 20), col="blue")
  lines(1:10, player$belief, col="blue")
  
  abline(player$beliefReg, col="blue")
  
  # plot Contributions 
  
  
  points(1:10, player$contribution, xlim=c(1, 10), 
         ylab="player Belief", ylim=c(0, 20), 
         main=paste0("player #", s, "'s beleif during the game rounds"), col="red")
  lines(1:10, player$contribution, col="red", lty=2, type="o", pch=22)
  abline(player$contiribReg, col="red", lty=2)
  
  legend(7, 20.5, c("Beleif","Contribution"), cex=0.8, 
         col=c("blue","red"), pch=21:22, lty=1:2);
  
  title(paste0("\nPlayer ", s), outer=TRUE)
  title (main=paste0("\nContrbution Reg =",
                     round(player$contiribSlop, 3), ", Belief Reg =", round(player$beliefSlop, 3), "\n newClass = ", player$newClass))
  
  
  dev.off()
  par(mfrow= c(1, 1))
}
createProfiles <- function(data){
  setwd('C:/Users/pqf/Google Drive/PhD/Codes/RuleBased_Classification/playerProfiles/')
  data <- read.csv('C:/Users/pqf/Google Drive/PhD/Data/game/repeated.csv')
  subjects <- unique(data$idsubj)
  for(s in subjects){
    player <- playerProfile(data[which(data$idsubj == s), ])
    plotPlayerProfile(player)
  }
}

createProfiles()
