Evaluation
================
Maarten van der Velde
Last updated: 2025-03-14

- [Evaluation questions](#evaluation-questions)
  - [Hoe koos je welke les je ging
    doen?](#hoe-koos-je-welke-les-je-ging-doen)
  - [Bij welk onderwerp vond je MemoryLab het meest
    toevoegen?](#bij-welk-onderwerp-vond-je-memorylab-het-meest-toevoegen)
  - [Geef MemoryLab een cijfer](#geef-memorylab-een-cijfer)
  - [Zou je deze leermethode ook willen gebruiken voor andere
    rekenlesstof?](#zou-je-deze-leermethode-ook-willen-gebruiken-voor-andere-rekenlesstof)
  - [Wat vond je fijn aan het oefenen met
    MemoryLab?](#wat-vond-je-fijn-aan-het-oefenen-met-memorylab)
  - [Wat vond je niet fijn aan het oefenen met
    MemoryLab?](#wat-vond-je-niet-fijn-aan-het-oefenen-met-memorylab)

``` r
library(here)
library(data.table)
library(ggplot2)
library(jsonlite)
library(purrr)
library(tidytext)
library(dplyr)
library(wordcloud2)

theme_memorylab_url <- "https://raw.githubusercontent.com/SlimStampen/theme_memorylab/master/theme_memorylab.R"
source(theme_memorylab_url)
```

# Evaluation questions

Students answered a number of evaluation questions after the posttest.

## Hoe koos je welke les je ging doen?

``` r
q1 <- fread(here("data", "feedback", "q1_choice.csv"))
q1[, freq := Totaal / sum(Totaal)]

q1
```

    ##                                          Antwoord Noorderpoort Alfa-college
    ##                                            <char>        <int>        <int>
    ## 1:                 Wat ik moeilijk/uitdagend vond            9            2
    ## 2: Wat niet te moeilijk en niet te makkelijk vond           23            2
    ## 3:                          Wat ik makkelijk vond           19            2
    ## 4:                                         Random           28            2
    ## 5:                         Wat de docent vertelde            2            0
    ## 6:                           Anders, namelijk ...            3            2
    ##    Totaal       freq
    ##     <int>      <num>
    ## 1:     11 0.11702128
    ## 2:     25 0.26595745
    ## 3:     21 0.22340426
    ## 4:     30 0.31914894
    ## 5:      2 0.02127660
    ## 6:      5 0.05319149

``` r
ggplot(q1, aes(x = reorder(Antwoord, freq), y = freq)) +
  geom_col(fill = colours_memorylab[1]) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Hoe koos je welke les je ging doen?",
       x = NULL,
       y = NULL) +
  coord_flip() +
  theme_ml() +
  theme(panel.grid.major.x = element_line(colour = "grey90"))
```

![](/Users/maarten/Documents/projects/mbo-rekenen/output/03_evaluation_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
ggsave(here("output", "evaluatie_leskeuze.png"), width = 10, height = 4)
```

What proportion of students made a choice based on difficulty?

``` r
q1[1:3, sum(freq)]
```

    ## [1] 0.606383

## Bij welk onderwerp vond je MemoryLab het meest toevoegen?

``` r
q2 <- fread(here("data", "feedback", "q2_topic.csv"), header = TRUE)
q2_long <- melt(q2, measure.vars = 2:12, variable.name = "positie", value.name = "N")
q2_long[, positie := as.numeric(positie)]

# Calculate the average position by topic
q2_rank <- q2_long[, .(mean_rank = weighted.mean(positie, N)), by = "topic"]

ggplot(q2_rank, aes(x = reorder(topic, -mean_rank), y = mean_rank)) +
  geom_col(fill = colours_memorylab[1]) +
  labs(title = "Bij welk onderwerp vond je MemoryLab het meest toevoegen?",
       x = NULL,
       y = "Gemiddelde rang (lager is beter)") +
  coord_flip() +
  theme_ml() +
  theme(panel.grid.major.x = element_line(colour = "grey90"))
```

![](/Users/maarten/Documents/projects/mbo-rekenen/output/03_evaluation_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
ggsave(here("output", "evaluatie_onderwerp.png"), width = 10, height = 4)
```

## Geef MemoryLab een cijfer

``` r
q3 <- fread(here("data", "feedback", "q3_grade.csv"))
q3[, freq := Totaal / sum(Totaal)]

# Mean grade
q3[, weighted.mean(grade, Totaal)]
```

    ## [1] 6.561224

``` r
# What proportion of students gave a passing grade?
q3[grade >= 6, sum(freq)]
```

    ## [1] 0.8265306

``` r
ggplot(q3, aes(x = grade, y = freq)) +
  geom_col(fill = colours_memorylab[1]) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Geef MemoryLab een cijfer",
       x = "Cijfer",
       y = NULL) +
  theme_ml()
```

![](/Users/maarten/Documents/projects/mbo-rekenen/output/03_evaluation_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
ggsave(here("output", "evaluatie_cijfer.png"), width = 10, height = 4)
```

## Zou je deze leermethode ook willen gebruiken voor andere rekenlesstof?

``` r
q4  <- fread(here("data", "feedback", "q4_useother.csv"))
q4[, freq := Totaal / sum(Totaal)]
q4
```

    ##    Antwoord Noorderpoort Alfa-college Totaal      freq
    ##      <char>        <int>        <int>  <int>     <num>
    ## 1:       Ja           63            3     66 0.7173913
    ## 2:      Nee           19            7     26 0.2826087

## Wat vond je fijn aan het oefenen met MemoryLab?

``` r
stop_words <- get_stopwords(language = "nl")

# Read text file 
q1 <- readLines(here("data", "feedback", "Q_watvondjefijn.txt"))

q1_words <- tibble(answer = tolower(q1)) |>
  unnest_tokens(word, answer) |>
  anti_join(stop_words, by = "word") |>
  count(word, sort = TRUE)

# Make wordcloud
wordcloud2(q1_words,
           shuffle = FALSE,
           minSize = 4,
           size = 4,
           rotateRatio = .4
           )
```

<div class="wordcloud2 html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-0b197131a87aca113ed9" style="width:1152px;height:1152px;"></div>
<script type="application/json" data-for="htmlwidget-0b197131a87aca113ed9">{"x":{"word":["makkelijk","overzichtelijk","duidelijk","oefenen","fijn","mee","goed","herhaling","werken","leert","overzicht","dingen","echt","gewoon","heel","ingedeeld","kunt","lekker","leren","leuk","makelijk","moeilijk","online","systeem","verschillende","vragen","waar","wel","1","basis","beetje","begin","bent","best","beter","dezeldfde","doent","eigen","elke","enz","erg","fout","gebruiken","gemakkelijk","genoeg","haalt","handig","hebt","heen","helpt","herhaald","herhalen","herhallen","inmiddels","inzien","keuze","kiezen","kijken","kreeg","makkelijke","makkkeeeelijkkk","meerde","minder","moeilijksheidgradaties","moelijk","moeten","nie","niveau","oevenen","onderwerp","onthouden","opdrachten","opties","plek","prima","programma","rekenen","resultaten","simpel","slecht","snel","super","tijd","uitgebreid","vaak","vind","vraag","weet","werden","wilt","zat"],"freq":[19,16,10,7,6,5,4,4,4,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],"fontFamily":"Segoe UI","fontWeight":"bold","color":"random-dark","minSize":4,"weightFactor":37.89473684210526,"backgroundColor":"white","gridSize":0,"minRotation":-0.7853981633974483,"maxRotation":0.7853981633974483,"shuffle":false,"rotateRatio":0.4,"shape":"circle","ellipticity":0.65,"figBase64":null,"hover":null},"evals":[],"jsHooks":{"render":[{"code":"function(el,x){\n                        console.log(123);\n                        if(!iii){\n                          window.location.reload();\n                          iii = False;\n\n                        }\n  }","data":null}]}}</script>

``` r
# Take screenshot of viewer to save
```

![](../output/wordcloud_fijn.png)

## Wat vond je niet fijn aan het oefenen met MemoryLab?

``` r
q2 <- readLines(here("data", "feedback", "Q_watvondjenietfijn.txt"))

q2_words <- tibble(answer = tolower(q2)) |>
  unnest_tokens(word, answer) |>
  anti_join(stop_words, by = "word") |>
  count(word, sort = TRUE)

# Make wordcloud
wordcloud2(q2_words,
           shuffle = FALSE,
           minSize = 4,
           size = 4,
           rotateRatio = .4
)
```

<div class="wordcloud2 html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-5e08d345b3db6ca6e47a" style="width:1152px;height:1152px;"></div>
<script type="application/json" data-for="htmlwidget-5e08d345b3db6ca6e47a">{"x":{"word":["makkelijk","vragen","fout","makkelijke","gelijk","herhaling","lang","teken","vaak","antwoord","duurt","euro","hele","hetzelfde","sommen","soms","vergat","zelfde","zien","10","best","bijvoorbeeld","dezelfde","domme","goed","heel","kiezen","kreeg","moeilijker","papier","uitleg","voordat","vraag","wel","00","aftrekken","alleen","alsnog","antwoorde","antwoorden","basischool","basisschool","beetje","bent","comma's","constant","dingen","dingetjes","duurden","eigenlijk","gaat","geacht","gegeven","geleerd","genoeg","gerekend","gesteld","getalle","ging","goede","goeie","herhaalde","herhaalt","herhalen","herlaling","invuld","jhiuoi","klaar","klikt","leert","liever","mocht","net","niks","niveau","nou","oefent","onderwerpen","online","opdr","opdrachten","optellen","punten","saaaaai","saai","saaie","schreef","schrijf","schrijven","simpel","snel","sommige","staat","stom","stond","telkens","tevaak","teveel","tijd","uitdagend","uitdaging","vervelend","vond","vooral","waarom","wanneer","weinig","werden","word","yte","zichtbaar","zodat"],"freq":[12,11,9,5,4,4,4,4,4,3,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],"fontFamily":"Segoe UI","fontWeight":"bold","color":"random-dark","minSize":4,"weightFactor":60,"backgroundColor":"white","gridSize":0,"minRotation":-0.7853981633974483,"maxRotation":0.7853981633974483,"shuffle":false,"rotateRatio":0.4,"shape":"circle","ellipticity":0.65,"figBase64":null,"hover":null},"evals":[],"jsHooks":{"render":[{"code":"function(el,x){\n                        console.log(123);\n                        if(!iii){\n                          window.location.reload();\n                          iii = False;\n\n                        }\n  }","data":null}]}}</script>

``` r
# Take screenshot of viewer to save
```

![](../output/wordcloud_nietfijn.png)
