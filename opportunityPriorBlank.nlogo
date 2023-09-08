breed [people person]
breed [thieves thief]
breed [wallets wallet]
people-own [target targeter value goal-store ticks-init lost shirt-color lost-wallet seen]
thieves-own [target predestined-target shirt-color goal-store ticks-init stolenSuccessful targeter seen]
wallets-own [owner]
breed [stores store]

globals[
  suspect-type
  crimescene-x
  crimescene-y
  crimetime
  smalln
  idsA
  crimesceneSeen
  suspect
  potential-target
  n_people
  n_thiefs
  heterdaad
  areaCovered
]


to setup
  clear-all

  set-default-shape stores "building store"
  set-default-shape wallets "circle"

  set crimescene-x 0
  set crimescene-y 0
  set crimetime 0
  set smalln -1
  set idsA 0
  set heterdaad 0

  create-stores 8
  ask stores [
    setxy random-xcor random-ycor
    set pcolor grey
    set color white
    set size 2
  ]

   create-thieves 1[                   ;; thief 8
    set color red
    set size 2
    face one-of neighbors4             ;; face N, E, S, or W
    set goal-store one-of stores
    set ticks-init 0
    set stolenSuccessful -1
    set targeter nobody
    set shirt-color 0
    set seen 0
    pen-down                           ;; have it draw its path
  ]

  create-people 200[                     ;; create 200 agents that do not steal
    setxy random-xcor random-ycor
    set color grey
    face one-of neighbors4              ;; face N, E, S, or W
    set value 1
    set lost-wallet 0
    set goal-store one-of stores
    face goal-store
    set ticks-init 0
    set targeter nobody
    set shirt-color one-of (list 0 0);(list 1 2 3 4)
    if random 10 > 8[
      set shirt-color 0
    ]
    set seen 0
    set lost -1
  ]

  ask person 11[     ;; person 11 is always the victim and is the only one who initially has the wallet.
    set value 20
    set color yellow
    set size 2
    pen-down
  ]

  ask thieves [
    setxy random-xcor random-ycor                         ;; initial thief position is not random but in the middle
    set predestined-target person 11     ;; the thief can only target person 11 (the designated victim
    set potential-target predestined-target
  ]

  set suspect person 9  ;; person 9 is always the designated suspect
  ask suspect[
    set size 2
    set color sky
    set shirt-color 0
    pen-down
  ]

  create-wallets 1[
    set owner person 11        ;; wallet is always initially owned by victim
    set color 48
    move-to owner
    ]

  ask stores[
     ask patches in-cone vision-range vision-angle[
         set pcolor 7
    ]
  ]

  let sumOfCoverage 0
  ask patches[
     if pcolor = 7[
         set sumOfCoverage sumOfCoverage + 1

     ]
  ]

  set areaCovered sumOfCoverage / ((1 + (2 * max-pxcor)) * ((max-pycor * 2) + 1))


  reset-ticks
end



to target-agent                                                                                      ;; how the thief targets the victim
  set target one-of other people in-cone thief-vision-range thief-vision-angle with [value >= 19]    ;; see if the agent can see the victim, if so, set the victim as target
  let targtingThief self
  if target != nobody [                                                                              ;; if the thief has found the target (=person 11)
      ask target [
          set targeter targtingThief                                                                 ;; ask the victim to set the thief as targeter for bookkeeping
      ]
  ]
end


to steal
  if target = nobody[
    stop
  ]
  face target
  ifelse distance target > 1[
      fd 1.4 ;; movement speed of agent is 1.4 (change to param)
  ][

      let victimHasValue -1
      let targetedVictim target
      ask wallets [
            ;; the targeted victim must at the point of stealing be the owner of the wallet
          ifelse owner = targetedVictim[
              set victimHasValue 1
          ][
              set victimHasValue 0    ;; the target does not own the wallet (victim doesn't have value)
          ]
      ]
      ifelse victimHasValue = 1[

          set crimescene-x xcor
          set crimescene-y ycor
          set crimetime ticks
          set stolenSuccessful True

          if random 100 >= 99 [
            set heterdaad 1
          ]


          set crimesceneSeen seenThief

          ask patch crimescene-x crimescene-y[

              let x [who] of people-here
              let y [who] of thieves-here
              set idsA sentence (x) (y)
              let v  0
              if member? 11 x[
                  set v  -1
              ]

              let z count people-here + count thieves-here + v
              set smalln z
               ;;set smalln list ([who] people-here [who] thieves-here [who] stores-here)
          ]

          ask patch crimescene-x crimescene-y[
              set pcolor green
          ]
          ;ask patch crimescene-x crimescene-y[
           ;   set pcolor green
          ;]
          set size 3
          ask target[
              set value 0
              set lost-wallet 1
          ]
          ask wallets [
              if owner = person 11[
                  set owner thief 8     ;; the thief is now in possession of the wallet

              ]
              ;;show owner
          ]
     ][  ;; this is the case where the target lost the wallet before the thief could steal it
         set stolenSuccessful False
         ask self [
             set target "no valuable target"
         ]
     ]
 ]

end

to throw-away-wallet
  if crimetime > 0[
         if ticks = crimetime + 1 + random 2[
             set owner nobody ;; throw wallet away
         ]
  ]
end

to check-pockets
  ask person 11 [
      if lost-wallet = 1[
          set lost True
      ]
  ]
end

to wander
  let sum-x 0
  let sum-y 0
  let num-people count people
  ask people [
     set sum-x sum-x + xcor
     set sum-y sum-y + ycor
  ]
    ;; getting the average position of all people
  let avg-x sum-x / num-people
  let avg-y sum-y / num-people
    ;; difference between thief and average position
  let dxa avg-x - xcor
  let dya avg-y - ycor
    ;; calculate heading to go to new position
  let new-heading atan dxa dya
  set heading new-heading
  forward 1
end

to walk-to-store
     ;; turtles wander around from store to store
   face goal-store
   if distance goal-store = 0[
      if targeter != nobody[ ;; targeter loses the trace when an agent goes inside a store
          ask targeter[
              if target  != "no valuable target"[
                  set target nobody
              ]
          ]
      ]
      ;; when we're in the store, stay in the store for some time
      if ticks-init = 0 [
      ;; if we just arrived, initialze the counter
          set ticks-init ticks
      ]
      if ticks - ticks-init > (2 + random 8)[  ;; if we stayed in the store for some time, we set a new store as our goal, check pockets and then leave
          set ticks-init 0
          set goal-store one-of stores
          if self = person 11[
                  check-pockets
          ]
      ]
  ]
  ifelse distance  goal-store < 1[  ;; we move towards the store location
      move-to goal-store
  ][
      fd 1
  ]
end

to prior-suspect   ;; prior probability N opportunity for the thief
    ask potential-target[
      let d distance thief 8
      set n_people count people in-radius d
      set n_thiefs count thieves in-radius d
      let N n_people + n_thiefs
  ]
end

to prior-innocent   ;; prior probability N opportunity for the innocent suspect
    ask potential-target[
       let d distance suspect
       set n_people count people in-radius d
       set n_thiefs count thieves in-radius d
       let N n_people + n_thiefs
  ]
end

to set-colors-of-agents
    ask thieves [
        set color red
        if stolenSuccessful = True[
            ask patch crimescene-x crimescene-y[
                set pcolor green
            ]
        ]
    ]
    ask people [
        set color grey
    ]
    ask suspect [
        set color sky
    ]
    ask potential-target [
        set color yellow
    ]
    ask stores[
        ask thieves in-cone vision-range vision-angle[
            ask patch-here[
               set pcolor 16
            ]
        ]
        let seenPeople people in-cone vision-range vision-angle
        ask suspect [
            let p patch-here
            if member? suspect seenPeople[
                ask p[
                    set pcolor 86
                ]
           ]
       ]
    ]
end

to seeAgent
  let x 0
  ask stores[
    ifelse member? myself turtles in-cone vision-range vision-angle[
     set x x + 1
    ] [
      set x x + 0
    ]
  ]
  ifelse x > 0[
    set seen 1
  ][
    set seen 0
  ]
end

to go

  set-colors-of-agents

  ;; stop the simulation when the victim realizes that they lost their wallet
  let flag 0
  ask person 11[
      if random 200 > 190[   ;; check pockets sometimes
          check-pockets
      ]
      ;;show lost
      if lost = True[
          set flag 1
      ]
  ]
  if flag = 1 [stop]  ;; if lost, then stop

  ;; if the victim doesn't know yet, we don't stop and we go on with the normal behaviour of the agents

  ask people[
      walk-to-store
      seeAgent
      ;;show [seen] of self
  ]

  ask thieves[

        ;; if the thief has successfully stolen, they are going to act normal and walk to places
      ifelse stolenSuccessful = True[
          walk-to-store
      ][
          if target = nobody[   ;; the agent has no target yet and is wandering around
              wander
          ]
          target-agent
          steal
     ]
     seeAgent
     ;;show [seen] of self
  ]

  ask stores[
     ask patches in-cone vision-range vision-angle[
         set pcolor 7
    ]
  ]

  ;; calculating the current location based priors for the thief and the innocent
  prior-suspect
  prior-innocent

  ask wallets[
     ;;show "whose are you?"
     ;;show owner
     if owner != nobody[
         move-to owner
    ]

  ]


  ;; for all wallets, we're asking them to be dropped if the owner is not the
  ask wallets [
      ifelse owner = thief 8[
          throw-away-wallet
      ][
          let beenStolen False
          ask thief 8[
              if stolenSuccessful = True[
                  set beenStolen True
              ]
          ]
          ;; lose wallet (very unlikely)
          if beenStolen = True and owner != nobody[
              if random 100 >= 99  [
                  ask owner[
                     set lost-wallet 1
                  ]
                  set owner nobody
              ]
          ]
          ;; wallet behaviour if there is an owner
          ifelse owner != nobody[
              move-to owner
          ][
             set owner one-of people with [distance myself < 1]  ;; agent that is near picks up the wallet
          ]
     ]
 ]
 ;; end simulation after 400 steps anyway
 if ticks > 400 [
    stop
 ]
 tick
end



to-report seenThief
   let x 0
   ask stores[
        ask thieves in-cone vision-range vision-angle[
            set x 1
        ]
   ]
   report x
end

to-report seenInnocent
    let x 0
    ask stores[
        let seenPeople people in-cone vision-range vision-angle
        ask suspect [
            if member? suspect seenPeople[
                set x 1
           ]
       ]
    ]
    report x
end

to-report stealingSuccess
    report [stolenSuccessful] of thieves
end


to-report N-report
  ask potential-target[
   let d distance thief 8
   let pset patches in-radius d
   set n_people count people in-radius d
    set n_thiefs count thieves in-radius d
  ]
  report n_people + n_thiefs
end

to-report wallet-owner
  report [owner] of wallets
end

to-report breed-wallet-owner
  let x [owner] of wallet 209
  ifelse x = nobody[
    report "nobody"
  ][
    report [breed] of x
  ]
end

to-report N-report-suspect
  ask potential-target[
   let d distance suspect
   let pset patches in-radius d
   set n_people count people in-radius d
   set n_thiefs count thieves in-radius d
  ]
  report n_people + n_thiefs
end

to-report agents-in-vision [param]
  let x count people in-cone vision-range vision-angle
  let y count thieves in-cone vision-range vision-angle
  report x + y - 1
end

to-report thieves-in-vision
  report count thieves in-cone vision-range vision-angle
end

to-report agent-id-in-vision
  let x [who] of people in-cone vision-range vision-angle
  let y [who] of thieves in-cone vision-range vision-angle
  report sentence (x) (y)
end

to-report location-crimescene-x
  report crimescene-x
end

to-report location-crimescene-y
  report crimescene-y
end

to-report time-crime
  report crimetime
end

to-report crimesceneInView
  report crimesceneSeen
end


to-report location
  report list (xcor) (ycor)
end

to-report getsmalln
  let x -1
  ifelse crimescene-x = 0 and crimescene-y = 0[
    set x -1][
    set x smalln  ]
  report x
end

to-report getidsA
  let x -1
  ifelse crimescene-x = 0 and crimescene-y = 0[
    set x -1][
    set x idsA  ]
  report x
end

to-report people-id-in-vision
  report ([who] of people in-cone vision-range vision-angle)
end


to-report thieves-id-in-vision
  report ([who] of thieves in-cone vision-range vision-angle)
end


to-report xloc
 report xcor
end

to-report yloc
  report ycor
end
@#$#@#$#@
GRAPHICS-WINDOW
262
41
884
664
-1
-1
12.04
1
10
1
1
1
0
0
0
1
-25
25
-25
25
1
1
1
ticks
30.0

SLIDER
1190
196
1362
229
shirt-options
shirt-options
0
100
0.0
1
1
NIL
HORIZONTAL

BUTTON
68
66
131
99
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1187
148
1359
181
thief-vision-range
thief-vision-range
0
100
15.0
1
1
NIL
HORIZONTAL

SLIDER
1187
100
1359
133
thief-vision-angle
thief-vision-angle
0
360
360.0
1
1
NIL
HORIZONTAL

SLIDER
1191
245
1363
278
vision-range
vision-range
0
30
25.0
1
1
NIL
HORIZONTAL

SLIDER
1188
293
1360
326
vision-angle
vision-angle
0
360
145.0
1
1
NIL
HORIZONTAL

BUTTON
66
24
139
57
NIL
setup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
68
106
164
139
go forever
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
399
765
719
973
Opportunity prior guilty
ticks
P
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot 1 / (N-report)"

PLOT
58
764
382
973
Number of agents in radius (guilty)
ticks
N in radius
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot N-report"

TEXTBOX
287
741
616
759
Opportunity Prior for thief (=guilty suspect)
14
0.0
1

TEXTBOX
964
725
1453
760
Opportunity Prior for random suspect (=innocent suspect)
14
0.0
1

PLOT
841
762
1147
971
Number agents in radius (innocent)
ticks
N in radius
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot N-report-suspect"

PLOT
1180
767
1501
964
Opp prior innocent
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot 1 / (N-report-suspect)"

MONITOR
966
539
1035
584
N-guilty
N-report
17
1
11

MONITOR
967
590
1050
635
N-innocent
N-report-suspect
17
1
11

MONITOR
1095
555
1200
600
NIL
wallet-owner
17
1
11

MONITOR
1091
611
1226
656
NIL
breed-wallet-owner
17
1
11

MONITOR
100
282
173
327
NIL
getsmalln
17
1
11

MONITOR
66
334
178
379
NIL
crimesceneSeen
17
1
11

MONITOR
61
231
144
276
NIL
getidsA
17
1
11

@#$#@#$#@
## WHAT IS IT?

A simulated environment for gathering statistics about agent location
and visibility in order to calculate an opportunity prior for reasoning in Bayesian Networks.

## HOW IT WORKS

Agents move around from store to store. There's one thief (red), who is targeting
a victim (yellow). The aim of the simulation is to gather statistics on N, which
is the number of agents in the area defined with as center the crime scene and 
as radius the last known location of the thief (=the location where the thief 
was last seen). This is then also done for an innocent agent (blue).

## HOW TO USE IT

Run the model, the plots show you how the number N and hence the
opportunity prior changes over time as the thief walks closer and further
away from the victim (as well as the innocent suspect)

## THINGS TO NOTICE

The calculation of the 'opportunity prior' cannot be done during the run, the 
plots only give an indication. This is because we do not know where the crime-scene
is before the crime has been committed. 
Behaviour of the agents and knowledge about their location is determined by the agent vision and angle (this is a feature for stores as well).

## THINGS TO TRY

Decrease the range of vision for stores and see how the increased uncertainty 
comes through in the analysis (code available online)

## EXTENDING THE MODEL

Add adaptive behaviour patterns: thief wants to stay out of the camera range.
Innocent suspect is now picked randomly, this is implausible. What happens if
we pick a suspect that is close to the crime scene?

## NETLOGO FEATURES

-

## RELATED MODELS

-

## CREDITS AND REFERENCES

See other code (R, Jupyter notebook) for further analysis.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

building store
false
0
Rectangle -7500403 true true 30 45 45 240
Rectangle -16777216 false false 30 45 45 165
Rectangle -7500403 true true 15 165 285 255
Rectangle -16777216 true false 120 195 180 255
Line -7500403 true 150 195 150 255
Rectangle -16777216 true false 30 180 105 240
Rectangle -16777216 true false 195 180 270 240
Line -16777216 false 0 165 300 165
Polygon -7500403 true true 0 165 45 135 60 90 240 90 255 135 300 165
Rectangle -7500403 true true 0 0 75 45
Rectangle -16777216 false false 0 0 75 45

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

coin heads
false
0
Circle -7500403 true true 15 15 270
Circle -16777216 false false 22 21 256
Line -16777216 false 165 180 192 196
Line -16777216 false 42 140 83 140
Line -16777216 false 37 151 91 151
Line -16777216 false 218 167 265 167
Polygon -16777216 false false 148 265 75 229 86 207 113 191 120 175 109 162 109 136 86 124 137 96 176 93 210 108 222 125 203 157 204 174 190 191 232 230
Polygon -16777216 false false 212 142 182 128 154 132 140 152 149 162 144 182 167 204 187 206 193 193 190 189 202 174 193 158 202 175 204 158
Line -16777216 false 164 154 182 152
Line -16777216 false 193 152 202 153
Polygon -16777216 false false 60 75 75 90 90 75 105 75 90 45 105 45 120 60 135 60 135 45 120 45 105 45 135 30 165 30 195 45 210 60 225 75 240 75 225 75 210 90 225 75 225 60 210 60 195 75 210 60 195 45 180 45 180 60 180 45 165 60 150 60 150 45 165 45 150 45 150 30 135 30 120 60 105 75

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment1" repetitions="200" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>[who] of turtles</metric>
    <metric>[agent-id-in-vision] of turtles</metric>
    <metric>[xcor] of turtles</metric>
    <metric>[ycor] of turtles</metric>
    <metric>location-crimescene-x</metric>
    <metric>location-crimescene-y</metric>
    <metric>crimetime</metric>
    <metric>[xcor] of thief 8</metric>
    <metric>[ycor] of thief 8</metric>
    <metric>[who] of suspect</metric>
    <metric>[xcor] of suspect</metric>
    <metric>[ycor] of suspect</metric>
    <metric>[xcor] of person 9</metric>
    <metric>[ycor] of person 9</metric>
    <metric>[xcor] of person 10</metric>
    <metric>[ycor] of person 10</metric>
    <metric>[xcor] of person 11</metric>
    <metric>[ycor] of person 11</metric>
    <metric>[xcor] of person 12</metric>
    <metric>[ycor] of person 12</metric>
    <metric>[xcor] of person 13</metric>
    <metric>[ycor] of person 13</metric>
    <metric>[xcor] of person 14</metric>
    <metric>[ycor] of person 14</metric>
    <metric>[xcor] of person 15</metric>
    <metric>[ycor] of person 15</metric>
    <metric>[xcor] of person 16</metric>
    <metric>[ycor] of person 16</metric>
    <metric>[xcor] of person 17</metric>
    <metric>[ycor] of person 17</metric>
    <metric>[xcor] of person 18</metric>
    <metric>[ycor] of person 18</metric>
    <metric>[xcor] of person 19</metric>
    <metric>[ycor] of person 19</metric>
    <metric>[xcor] of person 20</metric>
    <metric>[ycor] of person 20</metric>
    <metric>[xcor] of person 21</metric>
    <metric>[ycor] of person 21</metric>
    <metric>[xcor] of person 22</metric>
    <metric>[ycor] of person 22</metric>
    <metric>[xcor] of person 23</metric>
    <metric>[ycor] of person 23</metric>
    <metric>[xcor] of person 24</metric>
    <metric>[ycor] of person 24</metric>
    <metric>[xcor] of person 25</metric>
    <metric>[ycor] of person 25</metric>
    <metric>[xcor] of person 26</metric>
    <metric>[ycor] of person 26</metric>
    <metric>[xcor] of person 27</metric>
    <metric>[ycor] of person 27</metric>
    <metric>[xcor] of person 28</metric>
    <metric>[ycor] of person 28</metric>
    <metric>[xcor] of person 29</metric>
    <metric>[ycor] of person 29</metric>
    <metric>[xcor] of person 30</metric>
    <metric>[ycor] of person 30</metric>
    <metric>[xcor] of person 31</metric>
    <metric>[ycor] of person 31</metric>
    <metric>[xcor] of person 32</metric>
    <metric>[ycor] of person 32</metric>
    <metric>[xcor] of person 33</metric>
    <metric>[ycor] of person 33</metric>
    <metric>[xcor] of person 34</metric>
    <metric>[ycor] of person 34</metric>
    <metric>[xcor] of person 35</metric>
    <metric>[ycor] of person 35</metric>
    <metric>[xcor] of person 36</metric>
    <metric>[ycor] of person 36</metric>
    <metric>[xcor] of person 37</metric>
    <metric>[ycor] of person 37</metric>
    <metric>[xcor] of person 38</metric>
    <metric>[ycor] of person 38</metric>
    <metric>[xcor] of person 39</metric>
    <metric>[ycor] of person 39</metric>
    <metric>[xcor] of person 40</metric>
    <metric>[ycor] of person 40</metric>
    <metric>[xcor] of person 41</metric>
    <metric>[ycor] of person 41</metric>
    <metric>[xcor] of person 42</metric>
    <metric>[ycor] of person 42</metric>
    <metric>[xcor] of person 43</metric>
    <metric>[ycor] of person 43</metric>
    <metric>[xcor] of person 44</metric>
    <metric>[ycor] of person 44</metric>
    <metric>[xcor] of person 45</metric>
    <metric>[ycor] of person 45</metric>
    <metric>[xcor] of person 46</metric>
    <metric>[ycor] of person 46</metric>
    <metric>[xcor] of person 47</metric>
    <metric>[ycor] of person 47</metric>
    <metric>[xcor] of person 48</metric>
    <metric>[ycor] of person 48</metric>
    <metric>[xcor] of person 49</metric>
    <metric>[ycor] of person 49</metric>
    <metric>[xcor] of person 50</metric>
    <metric>[ycor] of person 50</metric>
    <metric>[xcor] of person 51</metric>
    <metric>[ycor] of person 51</metric>
    <metric>[xcor] of person 52</metric>
    <metric>[ycor] of person 52</metric>
    <metric>[xcor] of person 53</metric>
    <metric>[ycor] of person 53</metric>
    <metric>[xcor] of person 54</metric>
    <metric>[ycor] of person 54</metric>
    <metric>[xcor] of person 55</metric>
    <metric>[ycor] of person 55</metric>
    <metric>[xcor] of person 56</metric>
    <metric>[ycor] of person 56</metric>
    <metric>[xcor] of person 57</metric>
    <metric>[ycor] of person 57</metric>
    <metric>[xcor] of person 58</metric>
    <metric>[ycor] of person 58</metric>
    <metric>[xcor] of person 59</metric>
    <metric>[ycor] of person 59</metric>
    <metric>[xcor] of person 60</metric>
    <metric>[ycor] of person 60</metric>
    <metric>[xcor] of person 61</metric>
    <metric>[ycor] of person 61</metric>
    <metric>[xcor] of person 62</metric>
    <metric>[ycor] of person 62</metric>
    <metric>[xcor] of person 63</metric>
    <metric>[ycor] of person 63</metric>
    <metric>[xcor] of person 64</metric>
    <metric>[ycor] of person 64</metric>
    <metric>[xcor] of person 65</metric>
    <metric>[ycor] of person 65</metric>
    <metric>[xcor] of person 66</metric>
    <metric>[ycor] of person 66</metric>
    <metric>[xcor] of person 67</metric>
    <metric>[ycor] of person 67</metric>
    <metric>[xcor] of person 68</metric>
    <metric>[ycor] of person 68</metric>
    <metric>[xcor] of person 69</metric>
    <metric>[ycor] of person 69</metric>
    <metric>[xcor] of person 70</metric>
    <metric>[ycor] of person 70</metric>
    <metric>[xcor] of person 71</metric>
    <metric>[ycor] of person 71</metric>
    <metric>[xcor] of person 72</metric>
    <metric>[ycor] of person 72</metric>
    <metric>[xcor] of person 73</metric>
    <metric>[ycor] of person 73</metric>
    <metric>[xcor] of person 74</metric>
    <metric>[ycor] of person 74</metric>
    <metric>[xcor] of person 75</metric>
    <metric>[ycor] of person 75</metric>
    <metric>[xcor] of person 76</metric>
    <metric>[ycor] of person 76</metric>
    <metric>[xcor] of person 77</metric>
    <metric>[ycor] of person 77</metric>
    <metric>[xcor] of person 78</metric>
    <metric>[ycor] of person 78</metric>
    <metric>[xcor] of person 79</metric>
    <metric>[ycor] of person 79</metric>
    <metric>[xcor] of person 80</metric>
    <metric>[ycor] of person 80</metric>
    <metric>[xcor] of person 81</metric>
    <metric>[ycor] of person 81</metric>
    <metric>[xcor] of person 82</metric>
    <metric>[ycor] of person 82</metric>
    <metric>[xcor] of person 83</metric>
    <metric>[ycor] of person 83</metric>
    <metric>[xcor] of person 84</metric>
    <metric>[ycor] of person 84</metric>
    <metric>[xcor] of person 85</metric>
    <metric>[ycor] of person 85</metric>
    <metric>[xcor] of person 86</metric>
    <metric>[ycor] of person 86</metric>
    <metric>[xcor] of person 87</metric>
    <metric>[ycor] of person 87</metric>
    <metric>[xcor] of person 88</metric>
    <metric>[ycor] of person 88</metric>
    <metric>[xcor] of person 89</metric>
    <metric>[ycor] of person 89</metric>
    <metric>[xcor] of person 90</metric>
    <metric>[ycor] of person 90</metric>
    <metric>[xcor] of person 91</metric>
    <metric>[ycor] of person 91</metric>
    <metric>[xcor] of person 92</metric>
    <metric>[ycor] of person 92</metric>
    <metric>[xcor] of person 93</metric>
    <metric>[ycor] of person 93</metric>
    <metric>[xcor] of person 94</metric>
    <metric>[ycor] of person 94</metric>
    <metric>[xcor] of person 95</metric>
    <metric>[ycor] of person 95</metric>
    <metric>[xcor] of person 96</metric>
    <metric>[ycor] of person 96</metric>
    <metric>[xcor] of person 97</metric>
    <metric>[ycor] of person 97</metric>
    <metric>[xcor] of person 98</metric>
    <metric>[ycor] of person 98</metric>
    <metric>[xcor] of person 99</metric>
    <metric>[ycor] of person 99</metric>
    <metric>[xcor] of person 100</metric>
    <metric>[ycor] of person 100</metric>
    <metric>[xcor] of person 101</metric>
    <metric>[ycor] of person 101</metric>
    <metric>[xcor] of person 102</metric>
    <metric>[ycor] of person 102</metric>
    <metric>[xcor] of person 103</metric>
    <metric>[ycor] of person 103</metric>
    <metric>[xcor] of person 104</metric>
    <metric>[ycor] of person 104</metric>
    <metric>[xcor] of person 105</metric>
    <metric>[ycor] of person 105</metric>
    <metric>[xcor] of person 106</metric>
    <metric>[ycor] of person 106</metric>
    <metric>[xcor] of person 107</metric>
    <metric>[ycor] of person 107</metric>
    <metric>[xcor] of person 108</metric>
    <metric>[ycor] of person 108</metric>
    <metric>[xcor] of person 109</metric>
    <metric>[ycor] of person 109</metric>
    <metric>[xcor] of person 110</metric>
    <metric>[ycor] of person 110</metric>
    <metric>[xcor] of person 111</metric>
    <metric>[ycor] of person 111</metric>
    <metric>[xcor] of person 112</metric>
    <metric>[ycor] of person 112</metric>
    <metric>[xcor] of person 113</metric>
    <metric>[ycor] of person 113</metric>
    <metric>[xcor] of person 114</metric>
    <metric>[ycor] of person 114</metric>
    <metric>[xcor] of person 115</metric>
    <metric>[ycor] of person 115</metric>
    <metric>[xcor] of person 116</metric>
    <metric>[ycor] of person 116</metric>
    <metric>[xcor] of person 117</metric>
    <metric>[ycor] of person 117</metric>
    <metric>[xcor] of person 118</metric>
    <metric>[ycor] of person 118</metric>
    <metric>[xcor] of person 119</metric>
    <metric>[ycor] of person 119</metric>
    <metric>[xcor] of person 120</metric>
    <metric>[ycor] of person 120</metric>
    <metric>[xcor] of person 121</metric>
    <metric>[ycor] of person 121</metric>
    <metric>[xcor] of person 122</metric>
    <metric>[ycor] of person 122</metric>
    <metric>[xcor] of person 123</metric>
    <metric>[ycor] of person 123</metric>
    <metric>[xcor] of person 124</metric>
    <metric>[ycor] of person 124</metric>
    <metric>[xcor] of person 125</metric>
    <metric>[ycor] of person 125</metric>
    <metric>[xcor] of person 126</metric>
    <metric>[ycor] of person 126</metric>
    <metric>[xcor] of person 127</metric>
    <metric>[ycor] of person 127</metric>
    <metric>[xcor] of person 128</metric>
    <metric>[ycor] of person 128</metric>
    <metric>[xcor] of person 129</metric>
    <metric>[ycor] of person 129</metric>
    <metric>[xcor] of person 130</metric>
    <metric>[ycor] of person 130</metric>
    <metric>[xcor] of person 131</metric>
    <metric>[ycor] of person 131</metric>
    <metric>[xcor] of person 132</metric>
    <metric>[ycor] of person 132</metric>
    <metric>[xcor] of person 133</metric>
    <metric>[ycor] of person 133</metric>
    <metric>[xcor] of person 134</metric>
    <metric>[ycor] of person 134</metric>
    <metric>[xcor] of person 135</metric>
    <metric>[ycor] of person 135</metric>
    <metric>[xcor] of person 136</metric>
    <metric>[ycor] of person 136</metric>
    <metric>[xcor] of person 137</metric>
    <metric>[ycor] of person 137</metric>
    <metric>[xcor] of person 138</metric>
    <metric>[ycor] of person 138</metric>
    <metric>[xcor] of person 139</metric>
    <metric>[ycor] of person 139</metric>
    <metric>[xcor] of person 140</metric>
    <metric>[ycor] of person 140</metric>
    <metric>[xcor] of person 141</metric>
    <metric>[ycor] of person 141</metric>
    <metric>[xcor] of person 142</metric>
    <metric>[ycor] of person 142</metric>
    <metric>[xcor] of person 143</metric>
    <metric>[ycor] of person 143</metric>
    <metric>[xcor] of person 144</metric>
    <metric>[ycor] of person 144</metric>
    <metric>[xcor] of person 145</metric>
    <metric>[ycor] of person 145</metric>
    <metric>[xcor] of person 146</metric>
    <metric>[ycor] of person 146</metric>
    <metric>[xcor] of person 147</metric>
    <metric>[ycor] of person 147</metric>
    <metric>[xcor] of person 148</metric>
    <metric>[ycor] of person 148</metric>
    <metric>[xcor] of person 149</metric>
    <metric>[ycor] of person 149</metric>
    <metric>[xcor] of person 150</metric>
    <metric>[ycor] of person 150</metric>
    <metric>[xcor] of person 151</metric>
    <metric>[ycor] of person 151</metric>
    <metric>[xcor] of person 152</metric>
    <metric>[ycor] of person 152</metric>
    <metric>[xcor] of person 153</metric>
    <metric>[ycor] of person 153</metric>
    <metric>[xcor] of person 154</metric>
    <metric>[ycor] of person 154</metric>
    <metric>[xcor] of person 155</metric>
    <metric>[ycor] of person 155</metric>
    <metric>[xcor] of person 156</metric>
    <metric>[ycor] of person 156</metric>
    <metric>[xcor] of person 157</metric>
    <metric>[ycor] of person 157</metric>
    <metric>[xcor] of person 158</metric>
    <metric>[ycor] of person 158</metric>
    <metric>[xcor] of person 159</metric>
    <metric>[ycor] of person 159</metric>
    <metric>[xcor] of person 160</metric>
    <metric>[ycor] of person 160</metric>
    <metric>[xcor] of person 161</metric>
    <metric>[ycor] of person 161</metric>
    <metric>[xcor] of person 162</metric>
    <metric>[ycor] of person 162</metric>
    <metric>[xcor] of person 163</metric>
    <metric>[ycor] of person 163</metric>
    <metric>[xcor] of person 164</metric>
    <metric>[ycor] of person 164</metric>
    <metric>[xcor] of person 165</metric>
    <metric>[ycor] of person 165</metric>
    <metric>[xcor] of person 166</metric>
    <metric>[ycor] of person 166</metric>
    <metric>[xcor] of person 167</metric>
    <metric>[ycor] of person 167</metric>
    <metric>[xcor] of person 168</metric>
    <metric>[ycor] of person 168</metric>
    <metric>[xcor] of person 169</metric>
    <metric>[ycor] of person 169</metric>
    <metric>[xcor] of person 170</metric>
    <metric>[ycor] of person 170</metric>
    <metric>[xcor] of person 171</metric>
    <metric>[ycor] of person 171</metric>
    <metric>[xcor] of person 172</metric>
    <metric>[ycor] of person 172</metric>
    <metric>[xcor] of person 173</metric>
    <metric>[ycor] of person 173</metric>
    <metric>[xcor] of person 174</metric>
    <metric>[ycor] of person 174</metric>
    <metric>[xcor] of person 175</metric>
    <metric>[ycor] of person 175</metric>
    <metric>[xcor] of person 176</metric>
    <metric>[ycor] of person 176</metric>
    <metric>[xcor] of person 177</metric>
    <metric>[ycor] of person 177</metric>
    <metric>[xcor] of person 178</metric>
    <metric>[ycor] of person 178</metric>
    <metric>[xcor] of person 179</metric>
    <metric>[ycor] of person 179</metric>
    <metric>[xcor] of person 180</metric>
    <metric>[ycor] of person 180</metric>
    <metric>[xcor] of person 181</metric>
    <metric>[ycor] of person 181</metric>
    <metric>[xcor] of person 182</metric>
    <metric>[ycor] of person 182</metric>
    <metric>[xcor] of person 183</metric>
    <metric>[ycor] of person 183</metric>
    <metric>[xcor] of person 184</metric>
    <metric>[ycor] of person 184</metric>
    <metric>[xcor] of person 185</metric>
    <metric>[ycor] of person 185</metric>
    <metric>[xcor] of person 186</metric>
    <metric>[ycor] of person 186</metric>
    <metric>[xcor] of person 187</metric>
    <metric>[ycor] of person 187</metric>
    <metric>[xcor] of person 188</metric>
    <metric>[ycor] of person 188</metric>
    <metric>[xcor] of person 189</metric>
    <metric>[ycor] of person 189</metric>
    <metric>[xcor] of person 190</metric>
    <metric>[ycor] of person 190</metric>
    <metric>[xcor] of person 191</metric>
    <metric>[ycor] of person 191</metric>
    <metric>[xcor] of person 192</metric>
    <metric>[ycor] of person 192</metric>
    <metric>[xcor] of person 193</metric>
    <metric>[ycor] of person 193</metric>
    <metric>[xcor] of person 194</metric>
    <metric>[ycor] of person 194</metric>
    <metric>[xcor] of person 195</metric>
    <metric>[ycor] of person 195</metric>
    <metric>[xcor] of person 196</metric>
    <metric>[ycor] of person 196</metric>
    <metric>[xcor] of person 197</metric>
    <metric>[ycor] of person 197</metric>
    <metric>[xcor] of person 198</metric>
    <metric>[ycor] of person 198</metric>
    <metric>[xcor] of person 199</metric>
    <metric>[ycor] of person 199</metric>
    <metric>[xcor] of person 200</metric>
    <metric>[ycor] of person 200</metric>
    <metric>[xcor] of person 201</metric>
    <metric>[ycor] of person 201</metric>
    <metric>[xcor] of person 202</metric>
    <metric>[ycor] of person 202</metric>
    <metric>[xcor] of person 203</metric>
    <metric>[ycor] of person 203</metric>
    <metric>[xcor] of person 204</metric>
    <metric>[ycor] of person 204</metric>
    <metric>[xcor] of person 205</metric>
    <metric>[ycor] of person 205</metric>
    <metric>[xcor] of person 206</metric>
    <metric>[ycor] of person 206</metric>
    <metric>[xcor] of person 207</metric>
    <metric>[ycor] of person 207</metric>
    <metric>wallet-owner</metric>
    <metric>N-report</metric>
    <metric>N-report-suspect</metric>
    <metric>seenThief</metric>
    <metric>seenInnocent</metric>
    <metric>stealingSuccess</metric>
    <enumeratedValueSet variable="shirt-options">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thief-vision-range">
      <value value="67"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision-range">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision-angle">
      <value value="133"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thief-vision-angle">
      <value value="243"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment1Seen" repetitions="300" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>areaCovered</metric>
    <metric>wallet-owner</metric>
    <metric>stealingSuccess</metric>
    <metric>heterdaad</metric>
    <metric>location-crimescene-x</metric>
    <metric>location-crimescene-y</metric>
    <metric>crimetime</metric>
    <metric>getsmalln</metric>
    <metric>getidsA</metric>
    <metric>crimesceneSeen</metric>
    <metric>[who] of thief 8</metric>
    <metric>[xcor] of thief 8</metric>
    <metric>[ycor] of thief 8</metric>
    <metric>[seen] of thief 8</metric>
    <metric>[shirt-color] of thief 8</metric>
    <metric>[who] of person 9</metric>
    <metric>[xcor] of person 9</metric>
    <metric>[ycor] of person 9</metric>
    <metric>[seen] of person 9</metric>
    <metric>[shirt-color] of person 9</metric>
    <metric>[who] of person 10</metric>
    <metric>[xcor] of person 10</metric>
    <metric>[ycor] of person 10</metric>
    <metric>[seen] of person 10</metric>
    <metric>[shirt-color] of person 10</metric>
    <metric>[who] of person 11</metric>
    <metric>[xcor] of person 11</metric>
    <metric>[ycor] of person 11</metric>
    <metric>[seen] of person 11</metric>
    <metric>[shirt-color] of person 11</metric>
    <metric>[who] of person 12</metric>
    <metric>[xcor] of person 12</metric>
    <metric>[ycor] of person 12</metric>
    <metric>[seen] of person 12</metric>
    <metric>[shirt-color] of person 12</metric>
    <metric>[who] of person 13</metric>
    <metric>[xcor] of person 13</metric>
    <metric>[ycor] of person 13</metric>
    <metric>[seen] of person 13</metric>
    <metric>[shirt-color] of person 13</metric>
    <metric>[who] of person 14</metric>
    <metric>[xcor] of person 14</metric>
    <metric>[ycor] of person 14</metric>
    <metric>[seen] of person 14</metric>
    <metric>[shirt-color] of person 14</metric>
    <metric>[who] of person 15</metric>
    <metric>[xcor] of person 15</metric>
    <metric>[ycor] of person 15</metric>
    <metric>[seen] of person 15</metric>
    <metric>[shirt-color] of person 15</metric>
    <metric>[who] of person 16</metric>
    <metric>[xcor] of person 16</metric>
    <metric>[ycor] of person 16</metric>
    <metric>[seen] of person 16</metric>
    <metric>[shirt-color] of person 16</metric>
    <metric>[who] of person 17</metric>
    <metric>[xcor] of person 17</metric>
    <metric>[ycor] of person 17</metric>
    <metric>[seen] of person 17</metric>
    <metric>[shirt-color] of person 17</metric>
    <metric>[who] of person 18</metric>
    <metric>[xcor] of person 18</metric>
    <metric>[ycor] of person 18</metric>
    <metric>[seen] of person 18</metric>
    <metric>[shirt-color] of person 18</metric>
    <metric>[who] of person 19</metric>
    <metric>[xcor] of person 19</metric>
    <metric>[ycor] of person 19</metric>
    <metric>[seen] of person 19</metric>
    <metric>[shirt-color] of person 19</metric>
    <metric>[who] of person 20</metric>
    <metric>[xcor] of person 20</metric>
    <metric>[ycor] of person 20</metric>
    <metric>[seen] of person 20</metric>
    <metric>[shirt-color] of person 20</metric>
    <metric>[who] of person 21</metric>
    <metric>[xcor] of person 21</metric>
    <metric>[ycor] of person 21</metric>
    <metric>[seen] of person 21</metric>
    <metric>[shirt-color] of person 21</metric>
    <metric>[who] of person 22</metric>
    <metric>[xcor] of person 22</metric>
    <metric>[ycor] of person 22</metric>
    <metric>[seen] of person 22</metric>
    <metric>[shirt-color] of person 22</metric>
    <metric>[who] of person 23</metric>
    <metric>[xcor] of person 23</metric>
    <metric>[ycor] of person 23</metric>
    <metric>[seen] of person 23</metric>
    <metric>[shirt-color] of person 23</metric>
    <metric>[who] of person 24</metric>
    <metric>[xcor] of person 24</metric>
    <metric>[ycor] of person 24</metric>
    <metric>[seen] of person 24</metric>
    <metric>[shirt-color] of person 24</metric>
    <metric>[who] of person 25</metric>
    <metric>[xcor] of person 25</metric>
    <metric>[ycor] of person 25</metric>
    <metric>[seen] of person 25</metric>
    <metric>[shirt-color] of person 25</metric>
    <metric>[who] of person 26</metric>
    <metric>[xcor] of person 26</metric>
    <metric>[ycor] of person 26</metric>
    <metric>[seen] of person 26</metric>
    <metric>[shirt-color] of person 26</metric>
    <metric>[who] of person 27</metric>
    <metric>[xcor] of person 27</metric>
    <metric>[ycor] of person 27</metric>
    <metric>[seen] of person 27</metric>
    <metric>[shirt-color] of person 27</metric>
    <metric>[who] of person 28</metric>
    <metric>[xcor] of person 28</metric>
    <metric>[ycor] of person 28</metric>
    <metric>[seen] of person 28</metric>
    <metric>[shirt-color] of person 28</metric>
    <metric>[who] of person 29</metric>
    <metric>[xcor] of person 29</metric>
    <metric>[ycor] of person 29</metric>
    <metric>[seen] of person 29</metric>
    <metric>[shirt-color] of person 29</metric>
    <metric>[who] of person 30</metric>
    <metric>[xcor] of person 30</metric>
    <metric>[ycor] of person 30</metric>
    <metric>[seen] of person 30</metric>
    <metric>[shirt-color] of person 30</metric>
    <metric>[who] of person 31</metric>
    <metric>[xcor] of person 31</metric>
    <metric>[ycor] of person 31</metric>
    <metric>[seen] of person 31</metric>
    <metric>[shirt-color] of person 31</metric>
    <metric>[who] of person 32</metric>
    <metric>[xcor] of person 32</metric>
    <metric>[ycor] of person 32</metric>
    <metric>[seen] of person 32</metric>
    <metric>[shirt-color] of person 32</metric>
    <metric>[who] of person 33</metric>
    <metric>[xcor] of person 33</metric>
    <metric>[ycor] of person 33</metric>
    <metric>[seen] of person 33</metric>
    <metric>[shirt-color] of person 33</metric>
    <metric>[who] of person 34</metric>
    <metric>[xcor] of person 34</metric>
    <metric>[ycor] of person 34</metric>
    <metric>[seen] of person 34</metric>
    <metric>[shirt-color] of person 34</metric>
    <metric>[who] of person 35</metric>
    <metric>[xcor] of person 35</metric>
    <metric>[ycor] of person 35</metric>
    <metric>[seen] of person 35</metric>
    <metric>[shirt-color] of person 35</metric>
    <metric>[who] of person 36</metric>
    <metric>[xcor] of person 36</metric>
    <metric>[ycor] of person 36</metric>
    <metric>[seen] of person 36</metric>
    <metric>[shirt-color] of person 36</metric>
    <metric>[who] of person 37</metric>
    <metric>[xcor] of person 37</metric>
    <metric>[ycor] of person 37</metric>
    <metric>[seen] of person 37</metric>
    <metric>[shirt-color] of person 37</metric>
    <metric>[who] of person 38</metric>
    <metric>[xcor] of person 38</metric>
    <metric>[ycor] of person 38</metric>
    <metric>[seen] of person 38</metric>
    <metric>[shirt-color] of person 38</metric>
    <metric>[who] of person 39</metric>
    <metric>[xcor] of person 39</metric>
    <metric>[ycor] of person 39</metric>
    <metric>[seen] of person 39</metric>
    <metric>[shirt-color] of person 39</metric>
    <metric>[who] of person 40</metric>
    <metric>[xcor] of person 40</metric>
    <metric>[ycor] of person 40</metric>
    <metric>[seen] of person 40</metric>
    <metric>[shirt-color] of person 40</metric>
    <metric>[who] of person 41</metric>
    <metric>[xcor] of person 41</metric>
    <metric>[ycor] of person 41</metric>
    <metric>[seen] of person 41</metric>
    <metric>[shirt-color] of person 41</metric>
    <metric>[who] of person 42</metric>
    <metric>[xcor] of person 42</metric>
    <metric>[ycor] of person 42</metric>
    <metric>[seen] of person 42</metric>
    <metric>[shirt-color] of person 42</metric>
    <metric>[who] of person 43</metric>
    <metric>[xcor] of person 43</metric>
    <metric>[ycor] of person 43</metric>
    <metric>[seen] of person 43</metric>
    <metric>[shirt-color] of person 43</metric>
    <metric>[who] of person 44</metric>
    <metric>[xcor] of person 44</metric>
    <metric>[ycor] of person 44</metric>
    <metric>[seen] of person 44</metric>
    <metric>[shirt-color] of person 44</metric>
    <metric>[who] of person 45</metric>
    <metric>[xcor] of person 45</metric>
    <metric>[ycor] of person 45</metric>
    <metric>[seen] of person 45</metric>
    <metric>[shirt-color] of person 45</metric>
    <metric>[who] of person 46</metric>
    <metric>[xcor] of person 46</metric>
    <metric>[ycor] of person 46</metric>
    <metric>[seen] of person 46</metric>
    <metric>[shirt-color] of person 46</metric>
    <metric>[who] of person 47</metric>
    <metric>[xcor] of person 47</metric>
    <metric>[ycor] of person 47</metric>
    <metric>[seen] of person 47</metric>
    <metric>[shirt-color] of person 47</metric>
    <metric>[who] of person 48</metric>
    <metric>[xcor] of person 48</metric>
    <metric>[ycor] of person 48</metric>
    <metric>[seen] of person 48</metric>
    <metric>[shirt-color] of person 48</metric>
    <metric>[who] of person 49</metric>
    <metric>[xcor] of person 49</metric>
    <metric>[ycor] of person 49</metric>
    <metric>[seen] of person 49</metric>
    <metric>[shirt-color] of person 49</metric>
    <metric>[who] of person 50</metric>
    <metric>[xcor] of person 50</metric>
    <metric>[ycor] of person 50</metric>
    <metric>[seen] of person 50</metric>
    <metric>[shirt-color] of person 50</metric>
    <metric>[who] of person 51</metric>
    <metric>[xcor] of person 51</metric>
    <metric>[ycor] of person 51</metric>
    <metric>[seen] of person 51</metric>
    <metric>[shirt-color] of person 51</metric>
    <metric>[who] of person 52</metric>
    <metric>[xcor] of person 52</metric>
    <metric>[ycor] of person 52</metric>
    <metric>[seen] of person 52</metric>
    <metric>[shirt-color] of person 52</metric>
    <metric>[who] of person 53</metric>
    <metric>[xcor] of person 53</metric>
    <metric>[ycor] of person 53</metric>
    <metric>[seen] of person 53</metric>
    <metric>[shirt-color] of person 53</metric>
    <metric>[who] of person 54</metric>
    <metric>[xcor] of person 54</metric>
    <metric>[ycor] of person 54</metric>
    <metric>[seen] of person 54</metric>
    <metric>[shirt-color] of person 54</metric>
    <metric>[who] of person 55</metric>
    <metric>[xcor] of person 55</metric>
    <metric>[ycor] of person 55</metric>
    <metric>[seen] of person 55</metric>
    <metric>[shirt-color] of person 55</metric>
    <metric>[who] of person 56</metric>
    <metric>[xcor] of person 56</metric>
    <metric>[ycor] of person 56</metric>
    <metric>[seen] of person 56</metric>
    <metric>[shirt-color] of person 56</metric>
    <metric>[who] of person 57</metric>
    <metric>[xcor] of person 57</metric>
    <metric>[ycor] of person 57</metric>
    <metric>[seen] of person 57</metric>
    <metric>[shirt-color] of person 57</metric>
    <metric>[who] of person 58</metric>
    <metric>[xcor] of person 58</metric>
    <metric>[ycor] of person 58</metric>
    <metric>[seen] of person 58</metric>
    <metric>[shirt-color] of person 58</metric>
    <metric>[who] of person 59</metric>
    <metric>[xcor] of person 59</metric>
    <metric>[ycor] of person 59</metric>
    <metric>[seen] of person 59</metric>
    <metric>[shirt-color] of person 59</metric>
    <metric>[who] of person 60</metric>
    <metric>[xcor] of person 60</metric>
    <metric>[ycor] of person 60</metric>
    <metric>[seen] of person 60</metric>
    <metric>[shirt-color] of person 60</metric>
    <metric>[who] of person 61</metric>
    <metric>[xcor] of person 61</metric>
    <metric>[ycor] of person 61</metric>
    <metric>[seen] of person 61</metric>
    <metric>[shirt-color] of person 61</metric>
    <metric>[who] of person 62</metric>
    <metric>[xcor] of person 62</metric>
    <metric>[ycor] of person 62</metric>
    <metric>[seen] of person 62</metric>
    <metric>[shirt-color] of person 62</metric>
    <metric>[who] of person 63</metric>
    <metric>[xcor] of person 63</metric>
    <metric>[ycor] of person 63</metric>
    <metric>[seen] of person 63</metric>
    <metric>[shirt-color] of person 63</metric>
    <metric>[who] of person 64</metric>
    <metric>[xcor] of person 64</metric>
    <metric>[ycor] of person 64</metric>
    <metric>[seen] of person 64</metric>
    <metric>[shirt-color] of person 64</metric>
    <metric>[who] of person 65</metric>
    <metric>[xcor] of person 65</metric>
    <metric>[ycor] of person 65</metric>
    <metric>[seen] of person 65</metric>
    <metric>[shirt-color] of person 65</metric>
    <metric>[who] of person 66</metric>
    <metric>[xcor] of person 66</metric>
    <metric>[ycor] of person 66</metric>
    <metric>[seen] of person 66</metric>
    <metric>[shirt-color] of person 66</metric>
    <metric>[who] of person 67</metric>
    <metric>[xcor] of person 67</metric>
    <metric>[ycor] of person 67</metric>
    <metric>[seen] of person 67</metric>
    <metric>[shirt-color] of person 67</metric>
    <metric>[who] of person 68</metric>
    <metric>[xcor] of person 68</metric>
    <metric>[ycor] of person 68</metric>
    <metric>[seen] of person 68</metric>
    <metric>[shirt-color] of person 68</metric>
    <metric>[who] of person 69</metric>
    <metric>[xcor] of person 69</metric>
    <metric>[ycor] of person 69</metric>
    <metric>[seen] of person 69</metric>
    <metric>[shirt-color] of person 69</metric>
    <metric>[who] of person 70</metric>
    <metric>[xcor] of person 70</metric>
    <metric>[ycor] of person 70</metric>
    <metric>[seen] of person 70</metric>
    <metric>[shirt-color] of person 70</metric>
    <metric>[who] of person 71</metric>
    <metric>[xcor] of person 71</metric>
    <metric>[ycor] of person 71</metric>
    <metric>[seen] of person 71</metric>
    <metric>[shirt-color] of person 71</metric>
    <metric>[who] of person 72</metric>
    <metric>[xcor] of person 72</metric>
    <metric>[ycor] of person 72</metric>
    <metric>[seen] of person 72</metric>
    <metric>[shirt-color] of person 72</metric>
    <metric>[who] of person 73</metric>
    <metric>[xcor] of person 73</metric>
    <metric>[ycor] of person 73</metric>
    <metric>[seen] of person 73</metric>
    <metric>[shirt-color] of person 73</metric>
    <metric>[who] of person 74</metric>
    <metric>[xcor] of person 74</metric>
    <metric>[ycor] of person 74</metric>
    <metric>[seen] of person 74</metric>
    <metric>[shirt-color] of person 74</metric>
    <metric>[who] of person 75</metric>
    <metric>[xcor] of person 75</metric>
    <metric>[ycor] of person 75</metric>
    <metric>[seen] of person 75</metric>
    <metric>[shirt-color] of person 75</metric>
    <metric>[who] of person 76</metric>
    <metric>[xcor] of person 76</metric>
    <metric>[ycor] of person 76</metric>
    <metric>[seen] of person 76</metric>
    <metric>[shirt-color] of person 76</metric>
    <metric>[who] of person 77</metric>
    <metric>[xcor] of person 77</metric>
    <metric>[ycor] of person 77</metric>
    <metric>[seen] of person 77</metric>
    <metric>[shirt-color] of person 77</metric>
    <metric>[who] of person 78</metric>
    <metric>[xcor] of person 78</metric>
    <metric>[ycor] of person 78</metric>
    <metric>[seen] of person 78</metric>
    <metric>[shirt-color] of person 78</metric>
    <metric>[who] of person 79</metric>
    <metric>[xcor] of person 79</metric>
    <metric>[ycor] of person 79</metric>
    <metric>[seen] of person 79</metric>
    <metric>[shirt-color] of person 79</metric>
    <metric>[who] of person 80</metric>
    <metric>[xcor] of person 80</metric>
    <metric>[ycor] of person 80</metric>
    <metric>[seen] of person 80</metric>
    <metric>[shirt-color] of person 80</metric>
    <metric>[who] of person 81</metric>
    <metric>[xcor] of person 81</metric>
    <metric>[ycor] of person 81</metric>
    <metric>[seen] of person 81</metric>
    <metric>[shirt-color] of person 81</metric>
    <metric>[who] of person 82</metric>
    <metric>[xcor] of person 82</metric>
    <metric>[ycor] of person 82</metric>
    <metric>[seen] of person 82</metric>
    <metric>[shirt-color] of person 82</metric>
    <metric>[who] of person 83</metric>
    <metric>[xcor] of person 83</metric>
    <metric>[ycor] of person 83</metric>
    <metric>[seen] of person 83</metric>
    <metric>[shirt-color] of person 83</metric>
    <metric>[who] of person 84</metric>
    <metric>[xcor] of person 84</metric>
    <metric>[ycor] of person 84</metric>
    <metric>[seen] of person 84</metric>
    <metric>[shirt-color] of person 84</metric>
    <metric>[who] of person 85</metric>
    <metric>[xcor] of person 85</metric>
    <metric>[ycor] of person 85</metric>
    <metric>[seen] of person 85</metric>
    <metric>[shirt-color] of person 85</metric>
    <metric>[who] of person 86</metric>
    <metric>[xcor] of person 86</metric>
    <metric>[ycor] of person 86</metric>
    <metric>[seen] of person 86</metric>
    <metric>[shirt-color] of person 86</metric>
    <metric>[who] of person 87</metric>
    <metric>[xcor] of person 87</metric>
    <metric>[ycor] of person 87</metric>
    <metric>[seen] of person 87</metric>
    <metric>[shirt-color] of person 87</metric>
    <metric>[who] of person 88</metric>
    <metric>[xcor] of person 88</metric>
    <metric>[ycor] of person 88</metric>
    <metric>[seen] of person 88</metric>
    <metric>[shirt-color] of person 88</metric>
    <metric>[who] of person 89</metric>
    <metric>[xcor] of person 89</metric>
    <metric>[ycor] of person 89</metric>
    <metric>[seen] of person 89</metric>
    <metric>[shirt-color] of person 89</metric>
    <metric>[who] of person 90</metric>
    <metric>[xcor] of person 90</metric>
    <metric>[ycor] of person 90</metric>
    <metric>[seen] of person 90</metric>
    <metric>[shirt-color] of person 90</metric>
    <metric>[who] of person 91</metric>
    <metric>[xcor] of person 91</metric>
    <metric>[ycor] of person 91</metric>
    <metric>[seen] of person 91</metric>
    <metric>[shirt-color] of person 91</metric>
    <metric>[who] of person 92</metric>
    <metric>[xcor] of person 92</metric>
    <metric>[ycor] of person 92</metric>
    <metric>[seen] of person 92</metric>
    <metric>[shirt-color] of person 92</metric>
    <metric>[who] of person 93</metric>
    <metric>[xcor] of person 93</metric>
    <metric>[ycor] of person 93</metric>
    <metric>[seen] of person 93</metric>
    <metric>[shirt-color] of person 93</metric>
    <metric>[who] of person 94</metric>
    <metric>[xcor] of person 94</metric>
    <metric>[ycor] of person 94</metric>
    <metric>[seen] of person 94</metric>
    <metric>[shirt-color] of person 94</metric>
    <metric>[who] of person 95</metric>
    <metric>[xcor] of person 95</metric>
    <metric>[ycor] of person 95</metric>
    <metric>[seen] of person 95</metric>
    <metric>[shirt-color] of person 95</metric>
    <metric>[who] of person 96</metric>
    <metric>[xcor] of person 96</metric>
    <metric>[ycor] of person 96</metric>
    <metric>[seen] of person 96</metric>
    <metric>[shirt-color] of person 96</metric>
    <metric>[who] of person 97</metric>
    <metric>[xcor] of person 97</metric>
    <metric>[ycor] of person 97</metric>
    <metric>[seen] of person 97</metric>
    <metric>[shirt-color] of person 97</metric>
    <metric>[who] of person 98</metric>
    <metric>[xcor] of person 98</metric>
    <metric>[ycor] of person 98</metric>
    <metric>[seen] of person 98</metric>
    <metric>[shirt-color] of person 98</metric>
    <metric>[who] of person 99</metric>
    <metric>[xcor] of person 99</metric>
    <metric>[ycor] of person 99</metric>
    <metric>[seen] of person 99</metric>
    <metric>[shirt-color] of person 99</metric>
    <metric>[who] of person 100</metric>
    <metric>[xcor] of person 100</metric>
    <metric>[ycor] of person 100</metric>
    <metric>[seen] of person 100</metric>
    <metric>[shirt-color] of person 100</metric>
    <metric>[who] of person 101</metric>
    <metric>[xcor] of person 101</metric>
    <metric>[ycor] of person 101</metric>
    <metric>[seen] of person 101</metric>
    <metric>[shirt-color] of person 101</metric>
    <metric>[who] of person 102</metric>
    <metric>[xcor] of person 102</metric>
    <metric>[ycor] of person 102</metric>
    <metric>[seen] of person 102</metric>
    <metric>[shirt-color] of person 102</metric>
    <metric>[who] of person 103</metric>
    <metric>[xcor] of person 103</metric>
    <metric>[ycor] of person 103</metric>
    <metric>[seen] of person 103</metric>
    <metric>[shirt-color] of person 103</metric>
    <metric>[who] of person 104</metric>
    <metric>[xcor] of person 104</metric>
    <metric>[ycor] of person 104</metric>
    <metric>[seen] of person 104</metric>
    <metric>[shirt-color] of person 104</metric>
    <metric>[who] of person 105</metric>
    <metric>[xcor] of person 105</metric>
    <metric>[ycor] of person 105</metric>
    <metric>[seen] of person 105</metric>
    <metric>[shirt-color] of person 105</metric>
    <metric>[who] of person 106</metric>
    <metric>[xcor] of person 106</metric>
    <metric>[ycor] of person 106</metric>
    <metric>[seen] of person 106</metric>
    <metric>[shirt-color] of person 106</metric>
    <metric>[who] of person 107</metric>
    <metric>[xcor] of person 107</metric>
    <metric>[ycor] of person 107</metric>
    <metric>[seen] of person 107</metric>
    <metric>[shirt-color] of person 107</metric>
    <metric>[who] of person 108</metric>
    <metric>[xcor] of person 108</metric>
    <metric>[ycor] of person 108</metric>
    <metric>[seen] of person 108</metric>
    <metric>[shirt-color] of person 108</metric>
    <metric>[who] of person 109</metric>
    <metric>[xcor] of person 109</metric>
    <metric>[ycor] of person 109</metric>
    <metric>[seen] of person 109</metric>
    <metric>[shirt-color] of person 109</metric>
    <metric>[who] of person 110</metric>
    <metric>[xcor] of person 110</metric>
    <metric>[ycor] of person 110</metric>
    <metric>[seen] of person 110</metric>
    <metric>[shirt-color] of person 110</metric>
    <metric>[who] of person 111</metric>
    <metric>[xcor] of person 111</metric>
    <metric>[ycor] of person 111</metric>
    <metric>[seen] of person 111</metric>
    <metric>[shirt-color] of person 111</metric>
    <metric>[who] of person 112</metric>
    <metric>[xcor] of person 112</metric>
    <metric>[ycor] of person 112</metric>
    <metric>[seen] of person 112</metric>
    <metric>[shirt-color] of person 112</metric>
    <metric>[who] of person 113</metric>
    <metric>[xcor] of person 113</metric>
    <metric>[ycor] of person 113</metric>
    <metric>[seen] of person 113</metric>
    <metric>[shirt-color] of person 113</metric>
    <metric>[who] of person 114</metric>
    <metric>[xcor] of person 114</metric>
    <metric>[ycor] of person 114</metric>
    <metric>[seen] of person 114</metric>
    <metric>[shirt-color] of person 114</metric>
    <metric>[who] of person 115</metric>
    <metric>[xcor] of person 115</metric>
    <metric>[ycor] of person 115</metric>
    <metric>[seen] of person 115</metric>
    <metric>[shirt-color] of person 115</metric>
    <metric>[who] of person 116</metric>
    <metric>[xcor] of person 116</metric>
    <metric>[ycor] of person 116</metric>
    <metric>[seen] of person 116</metric>
    <metric>[shirt-color] of person 116</metric>
    <metric>[who] of person 117</metric>
    <metric>[xcor] of person 117</metric>
    <metric>[ycor] of person 117</metric>
    <metric>[seen] of person 117</metric>
    <metric>[shirt-color] of person 117</metric>
    <metric>[who] of person 118</metric>
    <metric>[xcor] of person 118</metric>
    <metric>[ycor] of person 118</metric>
    <metric>[seen] of person 118</metric>
    <metric>[shirt-color] of person 118</metric>
    <metric>[who] of person 119</metric>
    <metric>[xcor] of person 119</metric>
    <metric>[ycor] of person 119</metric>
    <metric>[seen] of person 119</metric>
    <metric>[shirt-color] of person 119</metric>
    <metric>[who] of person 120</metric>
    <metric>[xcor] of person 120</metric>
    <metric>[ycor] of person 120</metric>
    <metric>[seen] of person 120</metric>
    <metric>[shirt-color] of person 120</metric>
    <metric>[who] of person 121</metric>
    <metric>[xcor] of person 121</metric>
    <metric>[ycor] of person 121</metric>
    <metric>[seen] of person 121</metric>
    <metric>[shirt-color] of person 121</metric>
    <metric>[who] of person 122</metric>
    <metric>[xcor] of person 122</metric>
    <metric>[ycor] of person 122</metric>
    <metric>[seen] of person 122</metric>
    <metric>[shirt-color] of person 122</metric>
    <metric>[who] of person 123</metric>
    <metric>[xcor] of person 123</metric>
    <metric>[ycor] of person 123</metric>
    <metric>[seen] of person 123</metric>
    <metric>[shirt-color] of person 123</metric>
    <metric>[who] of person 124</metric>
    <metric>[xcor] of person 124</metric>
    <metric>[ycor] of person 124</metric>
    <metric>[seen] of person 124</metric>
    <metric>[shirt-color] of person 124</metric>
    <metric>[who] of person 125</metric>
    <metric>[xcor] of person 125</metric>
    <metric>[ycor] of person 125</metric>
    <metric>[seen] of person 125</metric>
    <metric>[shirt-color] of person 125</metric>
    <metric>[who] of person 126</metric>
    <metric>[xcor] of person 126</metric>
    <metric>[ycor] of person 126</metric>
    <metric>[seen] of person 126</metric>
    <metric>[shirt-color] of person 126</metric>
    <metric>[who] of person 127</metric>
    <metric>[xcor] of person 127</metric>
    <metric>[ycor] of person 127</metric>
    <metric>[seen] of person 127</metric>
    <metric>[shirt-color] of person 127</metric>
    <metric>[who] of person 128</metric>
    <metric>[xcor] of person 128</metric>
    <metric>[ycor] of person 128</metric>
    <metric>[seen] of person 128</metric>
    <metric>[shirt-color] of person 128</metric>
    <metric>[who] of person 129</metric>
    <metric>[xcor] of person 129</metric>
    <metric>[ycor] of person 129</metric>
    <metric>[seen] of person 129</metric>
    <metric>[shirt-color] of person 129</metric>
    <metric>[who] of person 130</metric>
    <metric>[xcor] of person 130</metric>
    <metric>[ycor] of person 130</metric>
    <metric>[seen] of person 130</metric>
    <metric>[shirt-color] of person 130</metric>
    <metric>[who] of person 131</metric>
    <metric>[xcor] of person 131</metric>
    <metric>[ycor] of person 131</metric>
    <metric>[seen] of person 131</metric>
    <metric>[shirt-color] of person 131</metric>
    <metric>[who] of person 132</metric>
    <metric>[xcor] of person 132</metric>
    <metric>[ycor] of person 132</metric>
    <metric>[seen] of person 132</metric>
    <metric>[shirt-color] of person 132</metric>
    <metric>[who] of person 133</metric>
    <metric>[xcor] of person 133</metric>
    <metric>[ycor] of person 133</metric>
    <metric>[seen] of person 133</metric>
    <metric>[shirt-color] of person 133</metric>
    <metric>[who] of person 134</metric>
    <metric>[xcor] of person 134</metric>
    <metric>[ycor] of person 134</metric>
    <metric>[seen] of person 134</metric>
    <metric>[shirt-color] of person 134</metric>
    <metric>[who] of person 135</metric>
    <metric>[xcor] of person 135</metric>
    <metric>[ycor] of person 135</metric>
    <metric>[seen] of person 135</metric>
    <metric>[shirt-color] of person 135</metric>
    <metric>[who] of person 136</metric>
    <metric>[xcor] of person 136</metric>
    <metric>[ycor] of person 136</metric>
    <metric>[seen] of person 136</metric>
    <metric>[shirt-color] of person 136</metric>
    <metric>[who] of person 137</metric>
    <metric>[xcor] of person 137</metric>
    <metric>[ycor] of person 137</metric>
    <metric>[seen] of person 137</metric>
    <metric>[shirt-color] of person 137</metric>
    <metric>[who] of person 138</metric>
    <metric>[xcor] of person 138</metric>
    <metric>[ycor] of person 138</metric>
    <metric>[seen] of person 138</metric>
    <metric>[shirt-color] of person 138</metric>
    <metric>[who] of person 139</metric>
    <metric>[xcor] of person 139</metric>
    <metric>[ycor] of person 139</metric>
    <metric>[seen] of person 139</metric>
    <metric>[shirt-color] of person 139</metric>
    <metric>[who] of person 140</metric>
    <metric>[xcor] of person 140</metric>
    <metric>[ycor] of person 140</metric>
    <metric>[seen] of person 140</metric>
    <metric>[shirt-color] of person 140</metric>
    <metric>[who] of person 141</metric>
    <metric>[xcor] of person 141</metric>
    <metric>[ycor] of person 141</metric>
    <metric>[seen] of person 141</metric>
    <metric>[shirt-color] of person 141</metric>
    <metric>[who] of person 142</metric>
    <metric>[xcor] of person 142</metric>
    <metric>[ycor] of person 142</metric>
    <metric>[seen] of person 142</metric>
    <metric>[shirt-color] of person 142</metric>
    <metric>[who] of person 143</metric>
    <metric>[xcor] of person 143</metric>
    <metric>[ycor] of person 143</metric>
    <metric>[seen] of person 143</metric>
    <metric>[shirt-color] of person 143</metric>
    <metric>[who] of person 144</metric>
    <metric>[xcor] of person 144</metric>
    <metric>[ycor] of person 144</metric>
    <metric>[seen] of person 144</metric>
    <metric>[shirt-color] of person 144</metric>
    <metric>[who] of person 145</metric>
    <metric>[xcor] of person 145</metric>
    <metric>[ycor] of person 145</metric>
    <metric>[seen] of person 145</metric>
    <metric>[shirt-color] of person 145</metric>
    <metric>[who] of person 146</metric>
    <metric>[xcor] of person 146</metric>
    <metric>[ycor] of person 146</metric>
    <metric>[seen] of person 146</metric>
    <metric>[shirt-color] of person 146</metric>
    <metric>[who] of person 147</metric>
    <metric>[xcor] of person 147</metric>
    <metric>[ycor] of person 147</metric>
    <metric>[seen] of person 147</metric>
    <metric>[shirt-color] of person 147</metric>
    <metric>[who] of person 148</metric>
    <metric>[xcor] of person 148</metric>
    <metric>[ycor] of person 148</metric>
    <metric>[seen] of person 148</metric>
    <metric>[shirt-color] of person 148</metric>
    <metric>[who] of person 149</metric>
    <metric>[xcor] of person 149</metric>
    <metric>[ycor] of person 149</metric>
    <metric>[seen] of person 149</metric>
    <metric>[shirt-color] of person 149</metric>
    <metric>[who] of person 150</metric>
    <metric>[xcor] of person 150</metric>
    <metric>[ycor] of person 150</metric>
    <metric>[seen] of person 150</metric>
    <metric>[shirt-color] of person 150</metric>
    <metric>[who] of person 151</metric>
    <metric>[xcor] of person 151</metric>
    <metric>[ycor] of person 151</metric>
    <metric>[seen] of person 151</metric>
    <metric>[shirt-color] of person 151</metric>
    <metric>[who] of person 152</metric>
    <metric>[xcor] of person 152</metric>
    <metric>[ycor] of person 152</metric>
    <metric>[seen] of person 152</metric>
    <metric>[shirt-color] of person 152</metric>
    <metric>[who] of person 153</metric>
    <metric>[xcor] of person 153</metric>
    <metric>[ycor] of person 153</metric>
    <metric>[seen] of person 153</metric>
    <metric>[shirt-color] of person 153</metric>
    <metric>[who] of person 154</metric>
    <metric>[xcor] of person 154</metric>
    <metric>[ycor] of person 154</metric>
    <metric>[seen] of person 154</metric>
    <metric>[shirt-color] of person 154</metric>
    <metric>[who] of person 155</metric>
    <metric>[xcor] of person 155</metric>
    <metric>[ycor] of person 155</metric>
    <metric>[seen] of person 155</metric>
    <metric>[shirt-color] of person 155</metric>
    <metric>[who] of person 156</metric>
    <metric>[xcor] of person 156</metric>
    <metric>[ycor] of person 156</metric>
    <metric>[seen] of person 156</metric>
    <metric>[shirt-color] of person 156</metric>
    <metric>[who] of person 157</metric>
    <metric>[xcor] of person 157</metric>
    <metric>[ycor] of person 157</metric>
    <metric>[seen] of person 157</metric>
    <metric>[shirt-color] of person 157</metric>
    <metric>[who] of person 158</metric>
    <metric>[xcor] of person 158</metric>
    <metric>[ycor] of person 158</metric>
    <metric>[seen] of person 158</metric>
    <metric>[shirt-color] of person 158</metric>
    <metric>[who] of person 159</metric>
    <metric>[xcor] of person 159</metric>
    <metric>[ycor] of person 159</metric>
    <metric>[seen] of person 159</metric>
    <metric>[shirt-color] of person 159</metric>
    <metric>[who] of person 160</metric>
    <metric>[xcor] of person 160</metric>
    <metric>[ycor] of person 160</metric>
    <metric>[seen] of person 160</metric>
    <metric>[shirt-color] of person 160</metric>
    <metric>[who] of person 161</metric>
    <metric>[xcor] of person 161</metric>
    <metric>[ycor] of person 161</metric>
    <metric>[seen] of person 161</metric>
    <metric>[shirt-color] of person 161</metric>
    <metric>[who] of person 162</metric>
    <metric>[xcor] of person 162</metric>
    <metric>[ycor] of person 162</metric>
    <metric>[seen] of person 162</metric>
    <metric>[shirt-color] of person 162</metric>
    <metric>[who] of person 163</metric>
    <metric>[xcor] of person 163</metric>
    <metric>[ycor] of person 163</metric>
    <metric>[seen] of person 163</metric>
    <metric>[shirt-color] of person 163</metric>
    <metric>[who] of person 164</metric>
    <metric>[xcor] of person 164</metric>
    <metric>[ycor] of person 164</metric>
    <metric>[seen] of person 164</metric>
    <metric>[shirt-color] of person 164</metric>
    <metric>[who] of person 165</metric>
    <metric>[xcor] of person 165</metric>
    <metric>[ycor] of person 165</metric>
    <metric>[seen] of person 165</metric>
    <metric>[shirt-color] of person 165</metric>
    <metric>[who] of person 166</metric>
    <metric>[xcor] of person 166</metric>
    <metric>[ycor] of person 166</metric>
    <metric>[seen] of person 166</metric>
    <metric>[shirt-color] of person 166</metric>
    <metric>[who] of person 167</metric>
    <metric>[xcor] of person 167</metric>
    <metric>[ycor] of person 167</metric>
    <metric>[seen] of person 167</metric>
    <metric>[shirt-color] of person 167</metric>
    <metric>[who] of person 168</metric>
    <metric>[xcor] of person 168</metric>
    <metric>[ycor] of person 168</metric>
    <metric>[seen] of person 168</metric>
    <metric>[shirt-color] of person 168</metric>
    <metric>[who] of person 169</metric>
    <metric>[xcor] of person 169</metric>
    <metric>[ycor] of person 169</metric>
    <metric>[seen] of person 169</metric>
    <metric>[shirt-color] of person 169</metric>
    <metric>[who] of person 170</metric>
    <metric>[xcor] of person 170</metric>
    <metric>[ycor] of person 170</metric>
    <metric>[seen] of person 170</metric>
    <metric>[shirt-color] of person 170</metric>
    <metric>[who] of person 171</metric>
    <metric>[xcor] of person 171</metric>
    <metric>[ycor] of person 171</metric>
    <metric>[seen] of person 171</metric>
    <metric>[shirt-color] of person 171</metric>
    <metric>[who] of person 172</metric>
    <metric>[xcor] of person 172</metric>
    <metric>[ycor] of person 172</metric>
    <metric>[seen] of person 172</metric>
    <metric>[shirt-color] of person 172</metric>
    <metric>[who] of person 173</metric>
    <metric>[xcor] of person 173</metric>
    <metric>[ycor] of person 173</metric>
    <metric>[seen] of person 173</metric>
    <metric>[shirt-color] of person 173</metric>
    <metric>[who] of person 174</metric>
    <metric>[xcor] of person 174</metric>
    <metric>[ycor] of person 174</metric>
    <metric>[seen] of person 174</metric>
    <metric>[shirt-color] of person 174</metric>
    <metric>[who] of person 175</metric>
    <metric>[xcor] of person 175</metric>
    <metric>[ycor] of person 175</metric>
    <metric>[seen] of person 175</metric>
    <metric>[shirt-color] of person 175</metric>
    <metric>[who] of person 176</metric>
    <metric>[xcor] of person 176</metric>
    <metric>[ycor] of person 176</metric>
    <metric>[seen] of person 176</metric>
    <metric>[shirt-color] of person 176</metric>
    <metric>[who] of person 177</metric>
    <metric>[xcor] of person 177</metric>
    <metric>[ycor] of person 177</metric>
    <metric>[seen] of person 177</metric>
    <metric>[shirt-color] of person 177</metric>
    <metric>[who] of person 178</metric>
    <metric>[xcor] of person 178</metric>
    <metric>[ycor] of person 178</metric>
    <metric>[seen] of person 178</metric>
    <metric>[shirt-color] of person 178</metric>
    <metric>[who] of person 179</metric>
    <metric>[xcor] of person 179</metric>
    <metric>[ycor] of person 179</metric>
    <metric>[seen] of person 179</metric>
    <metric>[shirt-color] of person 179</metric>
    <metric>[who] of person 180</metric>
    <metric>[xcor] of person 180</metric>
    <metric>[ycor] of person 180</metric>
    <metric>[seen] of person 180</metric>
    <metric>[shirt-color] of person 180</metric>
    <metric>[who] of person 181</metric>
    <metric>[xcor] of person 181</metric>
    <metric>[ycor] of person 181</metric>
    <metric>[seen] of person 181</metric>
    <metric>[shirt-color] of person 181</metric>
    <metric>[who] of person 182</metric>
    <metric>[xcor] of person 182</metric>
    <metric>[ycor] of person 182</metric>
    <metric>[seen] of person 182</metric>
    <metric>[shirt-color] of person 182</metric>
    <metric>[who] of person 183</metric>
    <metric>[xcor] of person 183</metric>
    <metric>[ycor] of person 183</metric>
    <metric>[seen] of person 183</metric>
    <metric>[shirt-color] of person 183</metric>
    <metric>[who] of person 184</metric>
    <metric>[xcor] of person 184</metric>
    <metric>[ycor] of person 184</metric>
    <metric>[seen] of person 184</metric>
    <metric>[shirt-color] of person 184</metric>
    <metric>[who] of person 185</metric>
    <metric>[xcor] of person 185</metric>
    <metric>[ycor] of person 185</metric>
    <metric>[seen] of person 185</metric>
    <metric>[shirt-color] of person 185</metric>
    <metric>[who] of person 186</metric>
    <metric>[xcor] of person 186</metric>
    <metric>[ycor] of person 186</metric>
    <metric>[seen] of person 186</metric>
    <metric>[shirt-color] of person 186</metric>
    <metric>[who] of person 187</metric>
    <metric>[xcor] of person 187</metric>
    <metric>[ycor] of person 187</metric>
    <metric>[seen] of person 187</metric>
    <metric>[shirt-color] of person 187</metric>
    <metric>[who] of person 188</metric>
    <metric>[xcor] of person 188</metric>
    <metric>[ycor] of person 188</metric>
    <metric>[seen] of person 188</metric>
    <metric>[shirt-color] of person 188</metric>
    <metric>[who] of person 189</metric>
    <metric>[xcor] of person 189</metric>
    <metric>[ycor] of person 189</metric>
    <metric>[seen] of person 189</metric>
    <metric>[shirt-color] of person 189</metric>
    <metric>[who] of person 190</metric>
    <metric>[xcor] of person 190</metric>
    <metric>[ycor] of person 190</metric>
    <metric>[seen] of person 190</metric>
    <metric>[shirt-color] of person 190</metric>
    <metric>[who] of person 191</metric>
    <metric>[xcor] of person 191</metric>
    <metric>[ycor] of person 191</metric>
    <metric>[seen] of person 191</metric>
    <metric>[shirt-color] of person 191</metric>
    <metric>[who] of person 192</metric>
    <metric>[xcor] of person 192</metric>
    <metric>[ycor] of person 192</metric>
    <metric>[seen] of person 192</metric>
    <metric>[shirt-color] of person 192</metric>
    <metric>[who] of person 193</metric>
    <metric>[xcor] of person 193</metric>
    <metric>[ycor] of person 193</metric>
    <metric>[seen] of person 193</metric>
    <metric>[shirt-color] of person 193</metric>
    <metric>[who] of person 194</metric>
    <metric>[xcor] of person 194</metric>
    <metric>[ycor] of person 194</metric>
    <metric>[seen] of person 194</metric>
    <metric>[shirt-color] of person 194</metric>
    <metric>[who] of person 195</metric>
    <metric>[xcor] of person 195</metric>
    <metric>[ycor] of person 195</metric>
    <metric>[seen] of person 195</metric>
    <metric>[shirt-color] of person 195</metric>
    <metric>[who] of person 196</metric>
    <metric>[xcor] of person 196</metric>
    <metric>[ycor] of person 196</metric>
    <metric>[seen] of person 196</metric>
    <metric>[shirt-color] of person 196</metric>
    <metric>[who] of person 197</metric>
    <metric>[xcor] of person 197</metric>
    <metric>[ycor] of person 197</metric>
    <metric>[seen] of person 197</metric>
    <metric>[shirt-color] of person 197</metric>
    <metric>[who] of person 198</metric>
    <metric>[xcor] of person 198</metric>
    <metric>[ycor] of person 198</metric>
    <metric>[seen] of person 198</metric>
    <metric>[shirt-color] of person 198</metric>
    <metric>[who] of person 199</metric>
    <metric>[xcor] of person 199</metric>
    <metric>[ycor] of person 199</metric>
    <metric>[seen] of person 199</metric>
    <metric>[shirt-color] of person 199</metric>
    <metric>[who] of person 200</metric>
    <metric>[xcor] of person 200</metric>
    <metric>[ycor] of person 200</metric>
    <metric>[seen] of person 200</metric>
    <metric>[shirt-color] of person 200</metric>
    <metric>[who] of person 201</metric>
    <metric>[xcor] of person 201</metric>
    <metric>[ycor] of person 201</metric>
    <metric>[seen] of person 201</metric>
    <metric>[shirt-color] of person 201</metric>
    <metric>[who] of person 202</metric>
    <metric>[xcor] of person 202</metric>
    <metric>[ycor] of person 202</metric>
    <metric>[seen] of person 202</metric>
    <metric>[shirt-color] of person 202</metric>
    <metric>[who] of person 203</metric>
    <metric>[xcor] of person 203</metric>
    <metric>[ycor] of person 203</metric>
    <metric>[seen] of person 203</metric>
    <metric>[shirt-color] of person 203</metric>
    <metric>[who] of person 204</metric>
    <metric>[xcor] of person 204</metric>
    <metric>[ycor] of person 204</metric>
    <metric>[seen] of person 204</metric>
    <metric>[shirt-color] of person 204</metric>
    <metric>[who] of person 205</metric>
    <metric>[xcor] of person 205</metric>
    <metric>[ycor] of person 205</metric>
    <metric>[seen] of person 205</metric>
    <metric>[shirt-color] of person 205</metric>
    <metric>[who] of person 206</metric>
    <metric>[xcor] of person 206</metric>
    <metric>[ycor] of person 206</metric>
    <metric>[seen] of person 206</metric>
    <metric>[shirt-color] of person 206</metric>
    <metric>[who] of person 207</metric>
    <metric>[xcor] of person 207</metric>
    <metric>[ycor] of person 207</metric>
    <metric>[seen] of person 207</metric>
    <metric>[shirt-color] of person 207</metric>
    <enumeratedValueSet variable="shirt-options">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thief-vision-range">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thief-vision-angle">
      <value value="360"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision-range">
      <value value="2"/>
      <value value="10"/>
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision-angle">
      <value value="145"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
