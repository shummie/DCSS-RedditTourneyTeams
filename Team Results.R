# Script to output tournament results:

library(XML)
library(openxlsx)

#directory <- "C:/Users/Rshuo/Documents/R/dcss/"
directory <- "C:/Users/Shummie/Documents/DCSS/Reddit Tournament/0.19/"
speciesShortTable <- read.xlsx(paste0(directory, "abbrev.xlsx"))
backgroundShortTable <- read.xlsx(paste0(directory, "abbrev.xlsx"), sheet=2)
godShortTable <- read.xlsx(paste0(directory, "abbrev.xlsx"), sheet=3)


#directory <- "C:/Users/Rshuo/Documents/R/dcss/clans/"
directory <- "http://dobrazupa.org/tournament/0.18/clans/"
teamName <- "TeamSplat"

teamData <- data.table(name = character(0),
                       points = numeric(0),
                       rank = numeric(0), 
                       wins = numeric(0), 
                       players = character(0))

playerData <- data.table(name = character(0), 
                         points = numeric(0), 
                         rank = numeric(0), 
                         wins = numeric(0), 
                         runes = numeric(0),
                         winList = character(0))

readTeam <- function(teamName, displayName) {
  
  # Read in the file
  url <- paste0(directory, teamName, ".html")
  
  teamTable <- readHTMLTable(url)
  
  # Table 1:
  # Tourney Points Total, Rank, Games Won/Played (data is transposed)
  # Table 2: Wins
  # Table 3: Recent Games
  # Table 4: Uniques Slain/Left
  # Table 5-7: Score Breakdown (Lots of detail, 3 combined tables)
  
  summaryTable <- data.table((teamTable[[1]]))
  teamPoints <- as.numeric(colnames(summaryTable)[2])
  teamRank <- as.numeric(trimws(strsplit(as.character(summaryTable[1][[2]])[1], "/")[[1]][1]))
  teamWins <- as.numeric(strsplit(as.character(summaryTable[2][[2]])[1], " ")[[1]][1])
  teamPlayers <- as.character(teamTable[[5]]$V1[2:(match("", teamTable[[5]]$V1)-2)])
  
  rowData <- data.table(displayName, teamPoints, teamRank, teamWins, teamPlayers = paste(teamPlayers[order(teamPlayers)], collapse = ", "))
  teamData <<- rbind(teamData, rowData, use.names = FALSE)
  
}

readTeam("mandevil", "Teamsplat")
readTeam("walkerboh", "AWBW")
#readTeam("alatreon")

# Get a list of all players
playerList <- unlist(strsplit(paste(teamData$players, collapse = ", "), ", "))


readPlayerData <- function(playerName) {
  
  #directory <- "C:/Users/Rshuo/Documents/R/dcss/players/"
  directory <- "http://dobrazupa.org/tournament/0.18/players/"
  url <- paste0(directory, playerName, ".html")
  playerTable <- readHTMLTable(url)
  
  # Table 1 is always player summary
  playerSummary <- playerTable[[1]]
  playerPoints <- as.numeric(colnames(playerSummary)[2])
  playerRank <- as.numeric(trimws(strsplit(as.character(playerSummary[2][[1]])[1], "/")[[1]][1]))
  playerWinCount <- as.numeric(strsplit(as.character(playerSummary[2][[1]])[3], "/")[[1]][1])
  
  # Table 2 is always win table
  playerWinTable <- playerTable[[2]]
  
  # Next table, if it exists, is the streak table
  tableNumber <- 3
  if ("Death" %in% colnames(playerTable[[tableNumber]])) {
    playerStreakTable <- playerTable[[tableNumber]]
    tableNumber <- tableNumber + 1
  } else {
    playerStreakTable <- data.table()
  }
  
  # Look for Score Breakdown tables
  while (!("Source" %in% colnames(playerTable[[tableNumber]]))) {
    tableNumber <- tableNumber + 1
  }
  getRuneTable <- data.table(playerTable[[tableNumber + 1]])
  playerRunes <- as.numeric(as.character(getRuneTable[Source == "rune", N]))
  
  if (length(playerWinTable) > 0) {
    # See if we can get a list of the wins...
    playerWinTable <- merge(playerWinTable, speciesShortTable, by.x = "Species", by.y = "SpeciesLong", all.x = TRUE, all.y = FALSE)
    playerWinTable <- merge(playerWinTable, backgroundShortTable, by.x = "Background", by.y = "BackLong", all.x = TRUE, all.y = FALSE)
    playerWinTable <- merge(playerWinTable, godShortTable, by.x = "God", by.y = "GodLong", all.x = TRUE, all.y = FALSE)
    playerWinTable <- data.table(playerWinTable)
    playerWinTable[is.na(GodShort), GodShort := "None"]
    playerWinTable[, combo := paste0(SpeciesShort, BackShort, "^", GodShort, "(", Runes, ")")]
    playerWinList <- paste(playerWinTable[, combo], collapse = ", ")
  } else {
    playerWinList <- ""
  }
  
  
  rowData <- data.table(playerName, playerPoints, playerRank, playerWinCount, playerRunes, playerWinList)  
  playerData <<- rbind(playerData, rowData, use.names = FALSE)
  
}


for (name in playerList) {
  readPlayerData(tolower(name))
}

teamData[, outText := paste(name, rank, points, wins, players, sep = "|")]
teamData <- teamData[order(rank)]

outHeader <- c("Team|Rank|Points|Wins|Players")
alignHeader <- c(":--:|:--:|:---:|:--:|:--")
title <- "###Reddit Team Standings"
time <- date()

writeLines(c(title, time, "", outHeader, alignHeader, teamData$outText))


playerData[, outText := paste(name, rank, points, wins, runes, winList, sep = "|")]
playerData <- playerData[order(rank)]

outHeader <- c("Player|Rank|Points|Wins|Runes|Win List")
alignHeader <- c(":--:|:--:|:--:|:--:|:--:|:---")
title <- "###Reddit Player Standings"
time <- date()

writeLines(c(title, time, "", outHeader, alignHeader, playerData$outText))






