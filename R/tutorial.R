# Example 1
g1 <- 'dag {
    X [exposure,pos="0.000,0.000"]
    Y [outcome,pos="2.000,0.000"]
    Z [pos="1.000,1.000"]
    Z -> X
    Z -> Y
    X -> Y
}'

label1 <- c("X" = "Ice-cream", "Y" = "Sunburn", "Z" = "Sunshine")
effect1 <- "total"
correct1 <- "Controlling for sunshine will close the backdoor path between ice-cream sales and sunburn"

# Example 2 - Mediation total effect
g2 <- 'dag {
    X [exposure,pos="0.000,0.000"]
    Y [outcome,pos="2.000,0.000"]
    Z [pos="1.000,1.000"]
    X -> Z
    Z -> Y
    X -> Y
}'

label2 <- c("X" = "Exercise", "Y" = "Mood", "Z" = "Sleep quality")
effect2 <- "total"

# Example 3 - Mediation direct effect
g3 <- 'dag {
    X [exposure,pos="0.000,0.000"]
    Y [outcome,pos="2.000,0.000"]
    Z [pos="1.000,1.000"]
    X -> Z
    Z -> Y
    X -> Y
}'

label3 <- c("X" = "Exercise", "Y" = "Mood", "Z" = "Sleep quality")
effect3 <- "direct"



# Example 4 from https://rss.onlinelibrary.wiley.com/doi/10.1111/1740-9713.01413
g4 <- 'dag {
    X [exposure,pos="0.000,2.000"]
    Y [outcome,pos="0.000,0.000"]
    Z [pos="1.000,1.000"]
    X -> Z
    Y -> Z
    X -> Y
}'

label4 <- c("X" = "Smoking", "Y" = "Coronavirus", "Z" = "Hospitalisation")
effect4 <- "total"

# Example 5 from https://academic.oup.com/aje/article/176/10/938/92975
g5 <- 'dag {
Z1 [pos="0.000,1.000"]
Z2 [pos="2.000,1.000"]
Z3 [pos="1.000,0.500"]
X [exposure,pos="0.000,0.000"]
Y [outcome,pos="2.000,0.000"]
Z1 -> Z3
Z1 -> X
Z2 -> Z3
Z2 -> Y
X -> Y
}'

label5 <- c("X" = "SSRIs", "Y" = "Lung cancer", "Z1" = "Depression", "Z2" = "Smoking", "Z3" = "Coronary disease")
effect5 <- "total"

# Example 6 from https://www.nature.com/articles/s41390-018-0071-3
g6 <- 'dag {
X [exposure,pos="1.000,1.000"]
Y [outcome,pos="3.000,1.000"]
Z1 [pos="0.000,3.000"]
Z2 [pos="2.000,0.000"]
Z3 [pos="2.000,-1.000"]
Z1 -> X
Z1 -> Y
X -> Z2
X -> Z3
Z2 -> Y
X -> Y
Y -> Z3
}'

label6 <- c("X" = "Screen time", "Y" = "Obesity", "Z1" = "Parental education", "Z2" = "Physical activity", "Z3" = "Self harm")
effect6 <- "total"

# Example 6 from https://www.nature.com/articles/s41390-018-0071-3
g7 <- 'dag {
X [exposure,pos="1.000,1.000"]
Y [outcome,pos="3.000,1.000"]
Z1 [pos="0.000,3.000"]
Z2 [pos="2.000,0.000"]
Z3 [pos="2.000,-1.000"]
Z1 -> X
Z1 -> Y
X -> Z2
X -> Z3
Z2 -> Y
X -> Y
Y -> Z3
}'

label7 <- c("X" = "Screen time", "Y" = "Obesity", "Z1" = "Parental education", "Z2" = "Physical activity", "Z3" = "Self harm")
effect7 <- "direct"

# Example 8 from https://doi.org/10.1016/j.chiabu.2019.02.011
g8 <- 'dag {
Z1 [pos="1.000,2.000"]
Z2 [pos="2.000,0.000"]
X [exposure,pos="0.000,3.000"]
Y [outcome,pos="2.000,3.000"]
X -> Z1
Z2 -> Z1
Z2 -> Y
Z1 -> Y
X -> Y
}'

label8 <- c("X" = "Childhood\nabuse", "Y" = "Opioid use\ndisorder", "Z1" = "Chronic pain", "Z2" = "Unintentional\ninjury")
effect8 <- "direct"


tuteNames <- list(
  shiny::HTML(paste('Ice-cream and Sunburn', shiny::br(), shiny::helpText('Confounding'))),
  shiny::HTML(paste('Exercise and Mood', shiny::br(), shiny::helpText('Mediation (Total effect)'))),
  shiny::HTML(paste('Exercise and Mood', shiny::br(), shiny::helpText('Mediation (Direct effect)'))),
  shiny::HTML(paste('Smoking and Coronavirus', shiny::br(), shiny::helpText('Collider Bias'))),
  shiny::HTML(paste('SSRIs and Lung cancer', shiny::br(), shiny::helpText('M-bias'))),
  shiny::HTML(paste('Screen time and Obesity', shiny::br(), shiny::helpText('A mixed example (Total effect)'))),
  shiny::HTML(paste('Screen time and Obesity', shiny::br(), shiny::helpText('A mixed example (Direct effect)'))),
  shiny::HTML(paste('Childhood abuse and Opioid use disorder', shiny::br(), shiny::helpText('A mixed example (Direct effect)')))
)


tuteHeaders <- c(
  "Ice-cream \U1F366 and Sunburn \U1F31E - An example of Confounding",
  "Exercise \U1F3C3 and Mood \U1F642 - An example of Mediation (Total effect)",
  "Exercise \U1F3C3 and Mood \U1F642 - An example of Mediation (Direct effect)",
  "Smoking \U1F6AC and Coronavirus \U1F9A0 - An example of Collider Bias",
  "SSRI use \U1F48A and lung cancer \U1FAC1 - An example of M-bias",
  "Screen time \U1F4BB and obesity \U1F35F - A mixed example (Total effect)",
  "Screen time \U1F4BB and obesity \U1F35F - A mixed example (Direct effect)",
  "Childhood abuse and opioid use disorder - A mixed example (Direct effect)"
)

nExamples <- length(tuteNames)
