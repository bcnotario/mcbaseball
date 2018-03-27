# Baseball Simulation Model

### Transition Probability Matrix Functions
These functions prepare the transition probability matrices (TPMs) by season and player or season league average lineup position to be used to track leadoffs and runs
```
retro2017 <- retro.full(2017)
```
Prepares the event and gamelog details, the generalized linear mixed model, and the base running, outs, and stolen base splits by lineup position for a given season (ex: 2017)
```
tpm.2017AL.list <- tpm.mlb(2017,"AL"); tpm.2017NL.list <- tpm.mlb(2017,"NL")
```
Returns the TPMs by lineup position for a given season and league (ex: 2017 American & National Leagues)
```
tpm.player("poseb001","kersc001",4,retro2017$event,retro2017$glmm)
```
Returns the TPM by batter against a specific pitcher, lineup position, and season (ex: B. Posey, C. Kershaw, Cleanup, 2017)
```
tpm.convert(tpm.2017AL.list[[1]])
```
Converts the original 25x25 TPM to a 85x85 TPM used for run tracking (ex: 2017 AL average leadoff position)
```
OBP.glmm("kersc001","2","101",retro2017$glmm)
```
Used in the tpm.player() function to rescale a player's generalized batting statistics according to the pitcher faced, total inning outs, and base runner situation (ex: C. Kershaw, 2 Outs, Runners on 1st/3rd, 2017)
### Run Projection Functions

```
tpm.lineup.lead()
```

```
tpm.lineup.runs()
```
### Simulation Functions

```
act.season(yyyy)
```

```
sim.ngame(n,tpm.list)
```

```
sim.runopt(tpm.list)
```

```
sim.season(yyyy)
```
