retro.lineup <- function(today){
  
  #INPUT - today (format 'yyyymmdd')
  #Remove bbrefID==NA from Lahman Master
  library(Lahman)
  master<-Master[!is.na(Master$bbrefID),]
  
  #Crunchtime Player Database
  crunch <- read.csv('http://crunchtimebaseball.com/master.csv',header=T)
  
  #Game Text
  team.html <- readLines(paste0('https://www.baseball-reference.com/boxes/?year=',substr(today,1,4),
                                '&month=',as.numeric(substr(today,5,6)),'&day=',as.numeric(substr(today,7,8))))
  game.str <- grep(today,team.html)
  game.n <- length(game.str)
  
  #Visiting & Home Teams
  away.str <- team.html[game.str-3]
  away.a <- regexpr('/teams/',away.str)[1:game.n]
  team.v <- substr(away.str,away.a+7,away.a+9)
  home.str <- team.html[game.str]
  home.a <- regexpr('/boxes/',home.str)[1:game.n]
  team.h <- substr(home.str,home.a+7,home.a+9)
  site.h <- substr(home.str,home.a,home.a+28)
  
  #Convert Team Names
  team.v <- ifelse(team.v=='ANA','LAA',team.v); team.h <- ifelse(team.h=='ANA','LAA',team.h)
  team.v <- ifelse(team.v=='CHA','CHW',team.v); team.h <- ifelse(team.h=='CHA','CHW',team.h)
  team.v <- ifelse(team.v=='CHN','CHC',team.v); team.h <- ifelse(team.h=='CHN','CHC',team.h)
  team.v <- ifelse(team.v=='KCR','KC',team.v); team.h <- ifelse(team.h=='KCR','KC',team.h)
  team.v <- ifelse(team.v=='KCA','KC',team.v); team.h <- ifelse(team.h=='KCA','KC',team.h)
  team.v <- ifelse(team.v=='LAN','LAD',team.v); team.h <- ifelse(team.h=='LAN','LAD',team.h)
  team.v <- ifelse(team.v=='NYN','NYM',team.v); team.h <- ifelse(team.h=='NYN','NYM',team.h)
  team.v <- ifelse(team.v=='NYA','NYY',team.v); team.h <- ifelse(team.h=='NYA','NYY',team.h)
  team.v <- ifelse(team.v=='SDP','SD',team.v); team.h <- ifelse(team.h=='SDP','SD',team.h)
  team.v <- ifelse(team.v=='SDN','SD',team.v); team.h <- ifelse(team.h=='SDN','SD',team.h)
  team.v <- ifelse(team.v=='SFG','SF',team.v); team.h <- ifelse(team.h=='SFG','SF',team.h)
  team.v <- ifelse(team.v=='SFN','SF',team.v); team.h <- ifelse(team.h=='SFN','SF',team.h)
  team.v <- ifelse(team.v=='SLN','STL',team.v); team.h <- ifelse(team.h=='SLN','STL',team.h)
  team.v <- ifelse(team.v=='TBR','TB',team.v); team.h <- ifelse(team.h=='TBR','TB',team.h)
  team.v <- ifelse(team.v=='TBA','TB',team.v); team.h <- ifelse(team.h=='TBA','TB',team.h)
  team.v <- ifelse(team.v=='WAS','WSH',team.v); team.h <- ifelse(team.h=='WAS','WSH',team.h)
  team.v <- ifelse(team.v=='WSN','WSH',team.v); team.h <- ifelse(team.h=='WSN','WSH',team.h)
  
  #Lineup Matrices
  line.v <- matrix(0,game.n,10); line.h <- matrix(0,game.n,10)
  
  #Starting Pitchers & Lineups
  for(j in 1:game.n){
    #Game Text
    temp.html <- readLines(paste0('https://www.baseball-reference.com',site.h[j]))
    pit.temp <- temp.html[which(regexpr('Pitching',temp.html)>0)[1]:which(regexpr('Balks',temp.html)>0)[1]]
    
    #Visiting Starting Pitcher
    pit.v <- pit.temp[which(regexpr('Innings Pitched',pit.temp)>0)[1]:which(regexpr('Innings Pitched',pit.temp)>0)[2]] 
    posn.j <- which(regexpr('player',pit.v)>0)[1]
    bref.j <- substring(pit.v[posn.j],regexpr('players/',pit.v[posn.j])[1]+10,
                        regexpr('.shtml',pit.v[posn.j])[1]-1)
    line.v[j,10] <- ifelse(nrow(crunch[crunch$bref_id==bref.j,])==0,paste0(bref.j,"*"),
                           as.character(crunch[crunch$bref_id==bref.j,'retro_id']))
    
    #Home Starting Pitcher
    pit.h <- pit.temp[which(regexpr('Innings Pitched',pit.temp)>0)[2]:length(pit.temp)]
    posn.j <- which(regexpr('player',pit.h)>0)[1]
    bref.j <- substring(pit.h[posn.j],regexpr('players/',pit.h[posn.j])[1]+10,
                        regexpr('.shtml',pit.h[posn.j])[1]-1)
    line.h[j,10] <- ifelse(nrow(crunch[crunch$bref_id==bref.j,])==0,paste0(bref.j,"*"),
                           as.character(crunch[crunch$bref_id==bref.j,'retro_id']))
    
    #Starting Lineups
    bat.v <- temp.html[which(regexpr('lineups_1',temp.html)>0):which(regexpr('lineups_2',temp.html)>0)]
    bat.h <- temp.html[which(regexpr('lineups_2',temp.html)>0):(which(regexpr('lineups_2',temp.html)>0)+100)]
    for(k in 1:9){
      #Visting Lineup
      posn.k <- which(regexpr(paste0('<td>',k,'</td>'),bat.v)>0)+1
      bref.k <- substring(bat.v[posn.k],regexpr('players/',bat.v[posn.k])[1]+10,
                          regexpr('.shtml',bat.v[posn.k])[1]-1)
      line.v[j,k] <- ifelse(nrow(crunch[crunch$bref_id==bref.k,])==0,paste0(bref.k,"*"),
                            as.character(crunch[crunch$bref_id==bref.k,'retro_id']))
      
      #Home Lineup
      posn.k <- which(regexpr(paste0('<td>',k,'</td>'),bat.h)>0)+1
      bref.k <- substring(bat.h[posn.k],regexpr('players/',bat.h[posn.k])[1]+10,
                          regexpr('.shtml',bat.h[posn.k])[1]-1)
      line.h[j,k] <- ifelse(nrow(crunch[crunch$bref_id==bref.k,])==0,paste0(bref.k,"*"),
                            as.character(crunch[crunch$bref_id==bref.k,'retro_id']))
    }
  }
  
  #Output - Visiting & Home Lineups
  return(if(game.n>1){list(inn.top=data.frame(team.h,pit.h=line.h[,10],team.v,line.v[,-10]),
                           inn.bot=data.frame(team.v,pit.v=line.v[,10],team.h,line.h[,-10]))} else {
                             list(inn.top=data.frame(team.h,line.h[,10],team.v,t(line.v[,-10])),
                                  inn.bot=data.frame(team.v,line.v[,10],team.h,t(line.h[,-10])))})
}
