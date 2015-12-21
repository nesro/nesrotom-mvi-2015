; 2015
; nesrotom@fit.cvut.cz
; MI-MVI 2nd semestral work

; goal:
; DrD+ is a Czech variant of DnD. The goal of this work is to breed
; good oponents for heroes. A good oponent makes a lot of damage,
; heroes will be badly hurt, but in the end, the heroes should win.
; Please note that this consider only basic aspects of battle.
; In reality, some bigass mage could win the battle with one spell. :)

; CZ name:
; model bitev v DrD+

; CZ description:
; Draci Doupe plus obsahuje system pro boj. cilem ulohy je vytvorit model
; boje skupinky hrdinu proti nepratelum a vychovat (pomoci evolucniho
; algoritmu) tak dobre nepratele, kteri budou rovnocenymi souperi (tak,
; aby hrdinove dostali nakladacku, ale nakonec pokud mozno zvitezili)

; ---------- -------- --------- --------- --------- --------- ---------

turtles-own [
  chromosome
  m_life      ; max life
  m_remaining ; remaining life
  m_ypos
]

breed [heroes hero]
breed [enemies enemy]

globals [
  g_heroes_cnt
  g_enemies_cnt
  g_population_cnt ; how many chromosomes
  g_patch_size
  g_h  ; list of heroes
  g_e  ; list of enemies
  g_ch ; list of chromosomes
  g_fn ; list of fitnesses
  g_best_fitness
  g_best_chromosome
  g_chlen ; length of a chromosome
];

to make-hero [ i ]
  set m_ypos ( 3 * (i + 1))

  setxy 2 m_ypos
  set m_remaining hero_life

  set shape "person"
  set size 3
  set color gray

  set label (round m_remaining)
  set label-color red
end

; SETUP

to setup
  clear-all
  ;print "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
  ;print "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"

  ; global variables
  set g_best_fitness 0
  set g_heroes_cnt 2
  set g_enemies_cnt enemies_cnt
  set g_population_cnt 16
  set g_patch_size 13
  set g_chlen 12

  ; create random chromosome for each population
  set g_ch n-values g_population_cnt [ n-values (g_enemies_cnt * g_chlen) [ random 2 ] ]
  ;print g_ch

  ask patches [set pcolor white]
end


to setup-make-heroes
  set g_patch_size 3;

  let i g_patch_size;
  create-heroes g_heroes_cnt [
    make-hero i
    set i i + 1;
  ]
end

to make-enemy [ ch i ]
  set m_ypos ( (round (32 / g_enemies_cnt - 1)) * (i + 1))
  setxy 30 m_ypos
  set m_remaining (ch2life ch i)
  set m_life m_remaining

  set label (ch2life ch i)
  set label-color red
  set color white
  set size 2
  set shape "sheep"
end

to setup-make-enemies [ ch ]
  let i 0
  create-enemies g_enemies_cnt [
    make-enemy ch i
    set i (i + 1)
  ]
end

to go

  ; create an array of fitnesses
  let fitnesses [ ]
  let i 0
  while [i < g_population_cnt] [
    let f (fitness (item i g_ch))
    ;type i type " gen has fitness " print f
    ; generation fitness chromosome
    set fitnesses lput (list i f (item i g_ch)) fitnesses
    set i (i + 1)
  ]
  ;print fitnesses


  let sorted_fit sort-by [ (item 1 ?1) > (item 1 ?2) ] fitnesses
  let best_fit sublist sorted_fit 0 ((g_population_cnt / 2))
  let worst_fit sublist sorted_fit (g_population_cnt / 2) (g_population_cnt - 1)
  ;type "sorted_fit " print sorted_fit
  ;type "best_fit " print best_fit
  ;type "best_fit len " print length best_fit
  ;type "worst_fit " print worst_fit

  if (item 1 (item 0 sorted_fit)) > g_best_fitness [
    set g_best_fitness (item 1 (item 0 sorted_fit))
    set g_best_chromosome (item 2 (item 0 sorted_fit))
  ]

  plot g_best_fitness

  ;type "best fitness " print (item 1 (item 0 sorted_fit))

  let half_cnt (g_population_cnt / 4)
  let half0best sublist best_fit 0 half_cnt
  let half1best sublist best_fit half_cnt ((g_population_cnt / 2))

  ;print (length half0best)
  ;print (length half1best)

  ; create new chromose array
  set g_ch []
  let k 0
  while [ k < (g_population_cnt / 2) ] [
    set g_ch lput (item 2 (item k best_fit)) g_ch
    set k k + 1
  ]
  ;type "added " print (length g_ch)

  ;(foreach best_fit worst_fit [
  (foreach half0best half1best [
      ;print "foreach"
      ;print ?1
      ;print ?2

      let new-chromosome cross (item 2 ?1) (item 2 ?2)
      let a item 0 new-chromosome
      let b item 1 new-chromosome

      if random 10 < 1 [ set a (mutate a)]
      if random 10 < 1 [ set b (mutate b)]

      set g_ch lput a g_ch
      set g_ch lput b g_ch
  ])

  ;print "g_ch:"
  ;print g_ch
  ;print length g_ch
end

; and return a pair of new chromosomes: [c1 c2]
to-report cross [ a b ]
  let n length a
  let x (random (n - 1)) + 1
  let c1 sentence (sublist a 0 x) (sublist b x n)
  let c2 sentence (sublist b x n) (sublist a 0 x)
  report (list c1 c2)
end

to-report mutate [ ch ]
  let k random length ch
  report (replace-item k ch (1 - (item k ch)))
end

; tady se provede simulace boje a urci se fitness neprate
; i = generation
to-report fitness [ ch ]
  let j 0
  let fitness_val 1

  clear-turtles
  setup-make-heroes
  setup-make-enemies ch
  pause

  loop [
    ; each enemy attacks a hero
    set j 0
    ;while [j < g_enemies_cnt] [
    ;print "---- ask enemies"
    ask enemies [
      ;type "enemy=" print j
      ;type "enemy dmg=" print ch2dmg ch j
      ;type "enemy life=" print ch2life ch j

      let anyone-alive3 one-of heroes with [m_remaining < 1]
      if anyone-alive3 != nobody  [
        stop
      ]

      let ewho who
      ask one-of heroes with [m_remaining > 0] [
        ;type "enemy is attacking to " print who
        set m_remaining (m_remaining - ch2dmg ch j)
        ;type "ouch!" print who
        set label m_remaining

        ; inc fitness for each attack from enemy
        set fitness_val (fitness_val + 1)

        create-link-from (enemy ewho)
        pause
      ]

      set j (j + 1)
      pause
    ] ; while [j < g_enemies_cnt]

    ;dup
    let anyone-alive2 one-of heroes with [m_remaining < 1]
    if anyone-alive2 != nobody  [
      ;print "a hero died, this is not acceptable"
      report 0
    ]

      ; each hero attack an enemy
    set j 0
    ;while [ j < g_heroes_cnt ] [
    ;print "---- ask heroes"
    ask heroes [
      let anyone-alive one-of enemies with [m_remaining > 0]
      if anyone-alive = nobody  [
        ;print "nomonster alive!"
        stop
      ]

      ;dup
      let anyone-alive3 one-of enemies with [m_remaining > 0]
      if anyone-alive3 = nobody  [
        ;print "ask heroes: no more alive monster"
        stop
      ]

      let hwho who
      ask one-of enemies with [m_remaining > 0] [
        set m_remaining (m_remaining - h2dmg j)
        set label m_remaining

        ; inc fitness for each attack from hero
        set fitness_val (fitness_val + 1)

        ;type "hero is attacking " print who
        ;type "boom!" print who
        ;type "ask heroes, setting " type who type " to " print m_remaining

        create-link-from (hero hwho)
        pause
      ]

      ;set j (j + 1)
      pause
    ]

    ;dup
    let anyone-alive3 one-of enemies with [m_remaining > 0]
    if anyone-alive3 = nobody  [
      ;print "no enemy alive!"
      report fitness_val
    ]


  ] ; loop

  ask heroes [
    set fitness_val (fitness_val + (m_life - m_remaining))
  ]

  ;clear-turtles
  report fitness_val
end ; to-report fitness [ ch ]

; chromosome: ((3b dmg) (3b life)*g_enemies_cnt
to-report ch2dmg [ ch i ]
  report 1 +
    32 * (item ((i * g_chlen) +  5) ch) +
    16 * (item ((i * g_chlen) +  4) ch) +
     8 * (item ((i * g_chlen) +  3) ch) +
     4 * (item ((i * g_chlen) +  2) ch) +
     2 * (item ((i * g_chlen) +  1) ch) +
     1 * (item ((i * g_chlen) +  0) ch)
end

to-report ch2life [ ch i ]
    report 1 +
    32 * (item ((i * g_chlen) + 11) ch) +
    16 * (item ((i * g_chlen) + 10) ch) +
     8 * (item ((i * g_chlen) +  9) ch) +
     4 * (item ((i * g_chlen) +  8) ch) +
     2 * (item ((i * g_chlen) +  7) ch) +
     1 * (item ((i * g_chlen) +  6) ch)
end

to-report h2dmg [ i ]
  report hero_dmg
end

to pause
  ;let i 0
  ;while [ i < 100] [ set i i + 1]
end

to setupgo
  setup
  go
end

to print_best_chromosome
  print g_best_chromosome
  let k 0
  while [ k < g_enemies_cnt ] [
    type "enemy=" type k type ", dmg=" type (ch2dmg g_best_chromosome k) type ", life=" print (ch2life g_best_chromosome k)
    set k k + 1
  ]

end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
649
470
-1
-1
13.0
1
10
1
1
1
0
0
0
1
0
32
0
32
0
0
1
ticks
30.0

BUTTON
27
46
100
79
NIL
setup
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
17
102
189
135
enemies_cnt
enemies_cnt
0
100
4
1
1
NIL
HORIZONTAL

BUTTON
103
46
166
79
NIL
go\n
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
30
230
93
263
NIL
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

SLIDER
18
144
190
177
hero_dmg
hero_dmg
0
100
30
1
1
NIL
HORIZONTAL

SLIDER
18
186
190
219
hero_life
hero_life
0
100
100
1
1
NIL
HORIZONTAL

BUTTON
32
313
124
346
NIL
setupgo
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
49
368
158
413
NIL
g_best_fitness
17
1
11

PLOT
16
418
216
568
plot g_best_fitness
NIL
plot g_best_fitness
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot g_best_fitness"
"pen-1" 1.0 0 -7500403 true "" ""

BUTTON
1
273
205
306
NIL
print_best_chromosome\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
227
481
639
636
Fitness is how many attacks were perfomed in the battle and how little life remain. If any hero dies, the fitness is 0, since this is unaceptable.\nThe chromosome is a binary list with life and damage for every enemy ((6 bits for life + 6 bits for damage) * enemies_cnt).
12
0.0
1

@#$#@#$#@
# 2015
# nesrotom@fit.cvut.cz
# MI-MVI 2nd semestral work
# Tomas Nesrovnal

## WHAT IS IT?

Simulace souboje a hledani vhodneho nastaveni nepratel pomoci GA.

## HOW IT WORKS

Je nahodne vygenerovan chromosom, ktery pro kazdeho nepritele urci jeho zivot a zraneni. Fitness funkce znazornuje delku boje a kolik zivotu hrdinove ztratili.

## HOW TO USE IT

Setup nastavi, go spusti jednen cyklus vypoctu fitness, krizeni a mutace.
Vysledek lze zobrazit tlacitkem print_best_chromosome
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
NetLogo 5.2.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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
