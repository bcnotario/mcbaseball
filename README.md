# Markov Chain Baseball Simulation Model
## Transition Probability Matrix Functions
These functions prepare the transition probability matrices (TPMs) by player or league average lineup position
### retro.full
Prepares the event and gamelog details, the generalized linear mixed model, and the base running, outs, and stolen base splits by lineup position for a given season
```
#Ex: 2017 MLB Season
retro2017 <- retro.full(2017)
```
### tpm.mlb
Returns the TPMs by average lineup position for a given season and league
```
#Ex: 2017 American League, 2017 National League
tpm.2017AL.list <- tpm.mlb(2017,"AL")
tpm.2017NL.list <- tpm.mlb(2017,"NL")
```
### tpm.player
Returns the TPM by batter against a specific pitcher, in a specific lineup position, for a given season 
```
#Ex: B. Posey, C. Kershaw, Cleanup, 2017 MLB Season
tpm.player("poseb001","kersc001",4,retro2017$event,retro2017$glmm)
```
### tpm.convert
Converts the original 25x25 TPM to a 85x85 TPM used for run tracking
```
#Ex: 2017 American League average leadoff position
tpm.convert(tpm.2017AL.list[[1]])
```
### OBP.glmm
Returns the On-Base Percentage rescale factor for a player's generalized batting statistics with respect to the pitcher faced, total inning outs, and base runner situation 
```
#Ex: C. Kershaw, 2 Outs, Runners on 1st/3rd, 2017 MLB Season
OBP.glmm("kersc001","2","101",retro2017$glmm)
```
## Run Projection Functions
These functions are combined to project the earned runs in a 9-inning game
### tpm.lineup.lead
Returns the probability of leading off an inning for each hitter in a lineup for a given lineup
```
#Ex: 2017 National League average lineup
tpm.lineup.lead(tpm.2017NL.list)
```
Returns the expected earned runs in an inning given a specific leadoff hitter for each potential leadoff hitter of a given lineup
### tpm.lineup.runs
```
#Ex: 2017 National League average lineup
tpm.lineup.runs(tpm.2017NL.list)
```
## Simulation Functions
These functions simulate games and return a season's box scores and simulationed games
### act.season
Returns the actual box score statistics of both starting pitchers for all games in a given season
```
#Ex: 2017 MLB Season
act.season(2017)
```
### sim.ngame
Simulates the earned runs in 9 innings for *n* games for a given lineup
```
#Ex: 100 games, 2017 American League average lineup
sim.ngame(100,tpm.2017AL.list)
```

### sim.runopt
Returns the optimal batting order that would score the most earned runs for a given lineup
```
#Ex: 2017 American League average lineup
sim.runopt(tpm.2017AL.list)
```
### sim.season
Returns the simulated and expected earned runs in 9 innings from the given and optimal starting lineups, for both teams, for all games in a given season
```
#Ex: 2017 MLB Season
sim.season(2017)
```
## Game Summary Using Remaining Inning Simulations
### tpm.label
Labels for TPM entries
### rem.9inn 
Simulation of the game remainder (up to 9 innings) using the fixed starting lineup
### rem.xinn
Simulation of the game in extra innings using the fixed starting lineup
### rem.ngame
Game simulation (n-times) using the starting lineups, starting pitchers, and functions (rem.9inn, rem.xinn)
### rem.pred
Game summary plot (using rem.ngame) that tracks game events, the home team's probability of winning the game, each team's expected remaining innings runs per at-bat
Situation Predictor per Batter per Game (5 min per game)
```
rem.pred(2019,'SDN',5)
```
