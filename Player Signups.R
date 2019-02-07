library(XML)
library(data.table)
library(openxlsx)

#combodata <- "C:/Users/Rshuo/Documents/R/dcss/abbrevPoints.xlsx"
# combodata <- "C:/Users/Rshuo/Documents/Ray/dcss/0.21"
combodata <- "C:/Users/Shummie/Documents/DCSS/Reddit Tournament/0.22/abbrevPoints.xlsx"
spPts <- data.table(read.xlsx(combodata, sheet = 1))
bgPts <- data.table(read.xlsx(combodata, sheet = 2))
godPts <- data.table(read.xlsx(combodata, sheet = 3))

url <- "http://crawl.akrasiac.org/scoring/players/shummie.html"

comboLog <- function(x) {
  return (800 * log2(1 + x/800))
}

# Creation of main database
mainPlayerData <- data.table(name = character(0), 
                             redditName = character(0),
                             totalScore = numeric(0), 
                             gamesPlayed = numeric(0),
                             gamesWon = numeric(0),
                             winPercent = numeric(0),
                             longestStreak = numeric(0))
tournamentData <- data.table(name = character(0),
                             version = character(0),
                             tournPoints = numeric(0),
                             gamesWon = numeric(0),
                             gamesPlayed = numeric(0))
winTable <- NULL


readTournament <- function(playerName, version) {
  
  tournamentUrl <- paste0("http://dobrazupa.org/tournament/", version, "/players/", playerName, ".html")
  
  tournamentTable <- tryCatch( {readHTMLTable(tournamentUrl)}, 
                               warning = function (w) {print ("WARNING")},
                               error = function(e) {print(paste0("Tournament ", version ," data not found for ", playerName))
                                 data.table()}, 
                               finally = {})
  
  if (length(tournamentTable) != 0) {
    tournTotalPoints <- rownames(t(tournamentTable[[1]]))[2]
    tournGamesWonTxt <- t(tournamentTable[[1]])[4]
    tournGamesWon <- as.numeric(strsplit(tournGamesWonTxt, "/")[[1]][1])
    tournGamesPlayed <- as.numeric(strsplit(tournGamesWonTxt, "/")[[1]][1])
    tournWinPercent <- tournGamesWon / tournGamesPlayed
    
    tournInfo <- data.table(playerName, version, tournTotalPoints, tournGamesWon, tournGamesPlayed)
    tournamentData <<- rbind(tournamentData, tournInfo, use.names = FALSE)
    
  }
}


# Read main scoring page information

readPlayer <- function(crawlName, redditName = "") {
  name = tolower(crawlName)
  if (redditName == "") {
    redditName <- name
  }
  url <- paste0("http://crawl.akrasiac.org/scoring/players/", name, ".html")
  #url <- paste0("C:/Users/Rshuo/Documents/R/dcss/", name, ".html")
  mainTables <- readHTMLTable(url)
  
  
  tableNumber = 1
  
  # The first table is the Overall Stats table
  overallStats <- data.table(mainTables[[tableNumber]])
  # Extract the relevant information
  # overallScore will have commas, extract them out.
  totalScore <- as.numeric(gsub(",", "", as.character(overallStats$`Total Score`)))
  totalGames <- as.numeric(as.character(overallStats$Games))
  totalWins <- as.numeric(as.character(overallStats$Wins))
  totalWinPercent <- as.numeric(gsub("%", "", as.character(overallStats$`Win %`)))/100 # stored as a decimal
  longestStreak <- 0
  
  tableNumber = tableNumber + 1
  
  # Check for Ongoing game (cao)
  tableCheck <- data.table(mainTables[[tableNumber]])
  if (colnames(tableCheck)[1] == "Character") {
    tableNumber = tableNumber + 1
  }
  
  
  # Check for win data
  playerWinTable <- data.table(mainTables[[tableNumber]])
  
  if (colnames(playerWinTable)[2] == "Score") {
    # Then we have a win table
    
    playerWinTable[, player := name]
    playerWinTable[, V1 := NULL]
    
    if (is.null(winTable)) {
      winTable <<- playerWinTable
    } else {
      winTable <<- rbind(winTable, playerWinTable, use.names = FALSE)
    }
    
    # Check for streak data
    
    playerStreakTable <- data.table(mainTables[[tableNumber+1]])
    if (colnames(playerStreakTable)[2] == "Wins") {
      longestStreak <- as.numeric(as.character(playerStreakTable[1]$Wins))
      
    }
    
  }
  
  playerInfo <- data.table(name, redditName, totalScore, totalGames, totalWins, totalWinPercent, longestStreak)
  mainPlayerData <<- rbind(mainPlayerData, playerInfo, use.names = FALSE)
  
  
  # Now to read in the tournament information.
  # We'll go back 3 tournaments
  
  # readTournament(name, "0.18")
  # readTournament(name, "0.17")
  # readTournament(name, "0.16")
  
}


##### Process Win Tables

createWinTable <- function() {
    
  # Convert relevant columns to characters
  winTable[, `:=`(Score = as.character(Score), Character = as.character(Character), 
                  Turns = as.character(Turns), Duration = as.character(Duration), 
                  God = as.character(God), Runes = as.character(Runes), Version = as.character(Version))]
  # Remove columns from characters
  winTable[, `:=`(Score = as.numeric(gsub(",", "", Score)), Turns = as.numeric(gsub(",", "", Turns)), Runes = as.numeric(Runes))]
  # Major Version
  winTable[, MajorVersion := substr(Version, 1, 4)]
  # Convert duration to seconds
  durationToSec <- function(durationString) {
    seconds <- 0
    if (nchar(durationString) > 8) {
      # We are in DAYS
      hms <- substr(durationString, nchar(durationString) - 7, nchar(durationString))
      days <- as.numeric(substr(durationString, 1, regexpr(",", durationString)[1]-1))
      seconds <- days * 24 * 60 * 60
    } else {
      hms <- durationString
    }
    
    
    hours <- as.numeric(substr(hms, 1, 2))
    mins <- as.numeric(substr(hms, 4, 5))
    sec <- as.numeric(substr(hms, 7, 8))
    
    seconds <- seconds + (hours * 60 * 60) + (mins * 60) + sec
    seconds
  }
  winTable[, seconds := as.numeric(lapply(Duration, durationToSec))]
  # Species/Background split
  winTable[, Species := substr(Character, 1, 2)]
  winTable[, Background := substr(Character, 3, 4)]
  
  winTable[God == "Ukayaw", God := "Uskayaw"]
  
  winTable <- merge(winTable, spPts[, .(SpAvg = max(Avg)), by = SpeciesShort], by.x = "Species", by.y = "SpeciesShort", all.x = TRUE, all.y = FALSE)
  winTable <- merge(winTable, bgPts[, .(BgAvg = max(Avg)), by = BackShort], by.x = "Background", by.y = "BackShort", all.x = TRUE, all.y = FALSE)
  winTable <- merge(winTable, godPts[, .(GodAvg = max(Avg)), by = GodLong], by.x = "God", by.y = "GodLong", all.x = TRUE, all.y = FALSE)
  winTable[God == "", GodAvg := 57]
  
  winTable[is.na(BgAvg), BgAvg := 30]
  
  
  # Filter to version or greater
  versionFilter <- "0.16"
  recentWinTable <- winTable[MajorVersion >= versionFilter,]

  summaryWinTable <- recentWinTable[, .(fastestTurn = min(Turns), 
                     fastestTime = min(seconds), 
                     highestScore = max(Score), 
                     winCount = .N), 
                 by = player]
  
  summaryWinTable[, scoreTurnCount := 5000000 / fastestTurn]
  summaryWinTable[, scoreHighScore := highestScore / 120000]
  summaryWinTable[, scoreTime := 1250000 / fastestTime]
  summaryWinTable[, recordPoints := scoreTurnCount + scoreHighScore + scoreTime]
  
  
  summaryWinTable <- merge(summaryWinTable, recentWinTable[, .(max(GodAvg)), by=c("player", "God")][, .(godPoints = sum(V1)), by=player], by = "player")
  summaryWinTable <- merge(summaryWinTable, recentWinTable[, .(max(SpAvg)), by=c("player", "Species")][, .(spPoints = sum(V1)), by=player], by = "player")
  summaryWinTable <- merge(summaryWinTable, recentWinTable[, .(max(BgAvg)), by=c("player", "Background")][, .(bgPoints = sum(V1)), by=player], by = "player")
  summaryWinTable[, comboPoints := godPoints + spPoints + bgPoints]
  summaryWinTable[, logComboPoints := 800*log2(1+comboPoints/800)]
  
  summaryWinTable[, ratingPoints := logComboPoints + recordPoints]
  return (summaryWinTable[order(-recordPoints)])

}

### Code to print out reddit formatting markdown code

# readPlayer("crawl name", "reddit name")

# Team A
readPlayer("TypeAskee", "TypeAskee")
readPlayer("cerulescent", "cerulescent")
readPlayer("jiveturtle", "Jiveturtle")
readPlayer("Melvinkitnick", "melvinkitnick")
readPlayer("Werew", "WerewNC")

# Team B
readPlayer("Likado", "LikadoDCSS")
readPlayer("Jynri", "SilentWizrd")

# Team C
readPlayer("Datul", "Datul") # check all members of team are here.
readPlayer("Shadowmage952", "shadowmage952")
readPlayer("Techfiend", "techfiend")
readPlayer("VictoriousLee", "V1ct0r10usL33")
readPlayer("Poulshu", "poulshu")


# Team D
readPlayer("drenzyme", "GameDesignerMan")
readPlayer("busb", "busb")

# Unassigned
readPlayer("G3Kappa", "G3Kappa")
readPlayer("TheMacoje", "maxdaman11")
readPlayer("EnegeticOcto", "Glista_iz_oluka")
readPlayer("Ketsa", "Ketsa")
readPlayer("Nino", "Ephine")
readPlayer("Sysice", "Sysice")
readPlayer("SriBri", "SriBri")
readPlayer("moogleknight", "moogleknight")
readPlayer("floatboth", "floatboth")
readPlayer("adept", "dastapov")
readPlayer("Jacory", "JAHCOREYG")
readPlayer("PantsMacKenzie", "PantsMacKenzie")
readPlayer("Masnef", "Masnef")
readPlayer("miscthings", "miscthings")
readPlayer("hollowman", "shooth")
readPlayer("Leszczynek", "Leszczynek") # Might be really inactive
readPlayer("fww", "waterboarding")
readPlayer("MauveAvengr", "ThomasPDX")
readPlayer("rentonl", "djangodjango") # casual team
readPlayer("wjchen", "iamwjchen")
readPlayer("Verendum", "Verenduum")
readPlayer("Uummannaq", "Uummannaq6")
readPlayer("ddubois", "ddubois1972")
readPlayer("SnubMonkey", "OriginalSnub")
readPlayer("Loudmushroom", "Loudmushroom")
readPlayer("Nib", "Bjarnevk")

readPlayer("Ofeo", "Ominous_Nom")


readPlayer("Nomi", "Noveno_Colono") # Wants to be on a team w/ console and greaterplayers....







# readPlayer("shummie")
# readPlayer("manman")
# Set default information

mainPlayerData[, redditName := paste0("/u/", redditName)]
mainPlayerData[, crawlName := paste0("[", name, "](http://crawl.akrasiac.org/scoring/players/", name, ".html)")]
mainPlayerData[, team := "-"]
mainPlayerData[, captain := "-"]
mainPlayerData[, diverse := "N"]
mainPlayerData[,iscaptain := ""]



# Who wants a diverse team?
# Stopped asking in 0.22
# mainPlayerData[name %in% c(), diverse := "Y"]

# Who are willing captains?
# crawl names?
mainPlayerData[name %in% tolower(c("jiveturtle", "TheMacoje", "EnegeticOcto", "Nino", "Nomi", "Sysice", "PantsMacKenzie", "Datul", "drenzyme")), captain := "Y"]

# Team definitions
mainPlayerData[name %in% tolower(c('TypeAskee', 'cerulescent', 'jiveturtle', 'Melvinkitnick', 'Werew', 'Ofeo')), team := 'A']
mainPlayerData[name %in% tolower(c('Likado', 'Jynri', 'nib', 'snubmonkey', 'enegeticocto', 'nomi')), team := 'B']
mainPlayerData[name %in% tolower(c('Datul', 'Shadowmage952', 'Techfiend', 'VictoriousLee', 'Poulshu', 'sysice')), team := 'C']
mainPlayerData[name %in% tolower(c('drenzyme', 'busb', 'themacoje', 'sribri', 'ddubois', 'miscthings')), team := 'D']
mainPlayerData[name %in% tolower(c('ketsa', 'floatboth', 'wjchen', 'fww', 'g3kappa', 'nino')), team := 'E']
mainPlayerData[name %in% tolower(c('moogleknight', 'verendum', 'pantsmackenzie', 'jacory', 'uummannaq', 'leszczynek')), team := 'F']
mainPlayerData[name %in% tolower(c('adept', 'masnef', 'hollowman', 'mauveavengr', 'rentonl', 'loudmushroom')), team := 'G']

# Assign Captains
mainPlayerData[name %in% tolower(c('jiveturtle', 'enegeticocto', 'datul', 'themacoje', 'nino', 'pantsmackenzie', 'adept')), iscaptain := "Y"]


# Create table
summaryWinTable <- createWinTable()
summaryWinTable[order(-ratingPoints), .(player, winCount, recordPoints, comboPoints, ratingPoints)]


# Create reddit table output

mainPlayerData[, outText := paste(team, redditName, crawlName, gamesWon, iscaptain, sep = "|")]
mainPlayerData <- mainPlayerData[order(-team, -iscaptain),]

outHeader <- c("Team|Reddit Name|Crawl Name|Games Won|Capt?")
alignHeader <- c(":--:|:---|:---|:--:|:--:")

playerListTitle <- "##Player List"
time <- date()

writeLines(c(playerListTitle, time, "", outHeader, alignHeader, mainPlayerData$outText))


