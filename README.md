# Baseball Simulation Model
## Transition Probability Matrix Functions
These functions prepare the transition probability matrices (TPMs) by season and player or season league average lineup position to be used to track leadoffs and runs
### retro.full
Prepares the event and gamelog details, the generalized linear mixed model, and the base running, outs, and stolen base splits by lineup position for a given season (ex: 2017)
```
retro2017 <- retro.full(2017)
```
### tpm.mlb
Returns the TPMs by lineup position for a given season and league (ex: 2017 American & National Leagues)
```
tpm.2017AL.list <- tpm.mlb(2017,"AL"); tpm.2017NL.list <- tpm.mlb(2017,"NL")
```
### tpm.player
Returns the TPM by batter against a specific pitcher, lineup position, and season (ex: B. Posey, C. Kershaw, Cleanup, 2017)
```
tpm.player("poseb001","kersc001",4,retro2017$event,retro2017$glmm)
```
### tpm.convert
Converts the original 25x25 TPM to a 85x85 TPM used for run tracking (ex: 2017 AL average leadoff position)
```
tpm.convert(tpm.2017AL.list[[1]])
```
### OBP.glmm
Rescale factor for a player's generalized batting statistics according to the pitcher faced, total inning outs, and base runner situation (ex: C. Kershaw, 2 Outs, Runners on 1st/3rd, 2017)
```
OBP.glmm("kersc001","2","101",retro2017$glmm)
```
## Run Projection Functions
### tpm.lineup.lead
Probability of leading off an inning by lineup position for given lineup (ex: 2017 NL average lineup)
```
tpm.lineup.lead(tpm.2017NL.list)
```
Expected runs in an inning by lineup position for a given lineup (ex: 2017 NL average lineup)
### tpm.lineup.runs

```
tpm.lineup.runs(tpm.2017NL.list)
```
## Simulation Functions
### act.season
Returns the box score statistics of both starting pitchers for all games in a given season (ex: 2017)
```
act.season(2017)
```
### sim.ngame
Simulates the earned runs in 9 innings for *n* games for a given lineup (ex: 2017 AL average lineup)
```
sim.ngame(100,tpm.2017AL.list)
```

### sim.runopt
Returns the optimal batting order that would score the most runs for a given lineup (ex: 2017 AL average lineup)
```
sim.runopt(tpm.2017AL.list)
```

### sim.season
Returns the simulated runs, simulated runs from an optimal lineup, expected runs, and expected runs from an optimal lineup in 9 innings from each starting lineup for a given season (ex: 2017)
```
sim.season(2017)
```
