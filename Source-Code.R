#Master Script
#Last edited: 26/05/2026

#Load libraries
library(tidyverse) #For data manipulation, cleaning, and visualisation
library(stargazer) #For coefficient tables
library(ggthemes) #For visualisation aesthetics
library(modelsummary) #For descriptive statistics
library(ggeffects) #For predicted probability plots
library(scales) #To display percentages
library(tinytable) #To save descriptive statistics tables
library(webshot2) #Required for tinytable
library(pscl) #For pseudo R2

drinks <- read.csv("D:\\Datasets\\alcoholicdrinks.csv", na.strings = "") #Ensure empty strings are parsed as NA
#Dataset can be provided on request

#Inspect dataset
tibble(drinks)

drinks <- subset(drinks, select = -c(15, 16)) #Remove columns with NAs

drinks$turnout_model <- replace_na(drinks$turnout_model, "Will not vote") #Rename missing values

#Rename variable names to more natural language alternatives
drinks <- drinks %>% 
  rename(`Voting Intention` = votingintention)
drinks <- drinks %>% 
  rename(`Beverage Preference` = cocktail)

#Create unclustered beverage preference variable for robustness testing later
drinks$`Unclustered Beverage Preference` <- drinks$`Beverage Preference`

#Descriptive statistics
drinksDemography <- data.frame(
  drinks$age_1,
  drinks$gender,
  drinks$region,
  drinks$ethnicity,
  drinks$education,
  drinks$urbanrural,
  drinks$workstatus,
  drinks$finances,
  drinks$ownrent,
  drinks$personalincome
)

#Rename demographic variables for presentation purposes
drinksDemography <- drinksDemography %>%
  rename(
    "Age" = drinks.age_1,
    "Gender" = drinks.gender,
    "Region" = drinks.region,
    "Ethnicity" = drinks.ethnicity,
    "Education" = drinks.education,
    "Urban/Rural Status" = drinks.urbanrural,
    "Work Status" = drinks.workstatus,
    "Finances" = drinks.finances,
    "Own/Rent Status" = drinks.ownrent,
    "Personal Income" = drinks.personalincome
    )

#Collapse response options into broader categories for presentation purposes
drinksDemography$Region <- 
  case_when(
    drinksDemography$Region == "East Midlands" ~ "Midlands",
    drinksDemography$Region == "West Midlands" ~ "Midlands",
    drinksDemography$Region == "North East England" ~ "North England",
    drinksDemography$Region == "North West England" ~ "North England",
    drinksDemography$Region == "South East England" ~ "South England",
    drinksDemography$Region == "South West England" ~ "South England",
    drinksDemography$Region == "Yorkshire and the Humber" ~ "North England",
    drinksDemography$Region == "Greater London" ~ "South England",
    drinksDemography$Region == "East of England" ~ "South England",
    TRUE ~ drinksDemography$Region
  )

drinksDemography$Education <-
  case_when(
    drinksDemography$Education == "Vocational or Technical Qualifications Completed (e.g. HND, NVQ)" ~ "Vocational",
    drinksDemography$Education == "Secondary Education Completed (GCSE/O Level/CSE or equivalent)" ~ "Secondary or Below",
    drinksDemography$Education == "University Education Completed (First Degree e.g. BA, BSc)" ~ "Undergraduate",
    drinksDemography$Education == "Secondary Education Completed (A Level or equivalent)" ~ "Secondary or Below",
    drinksDemography$Education == "Postgraduate Education Completed (e.g. Masters)" ~ "Postgraduate",
    drinksDemography$Education == "Doctorate, Post-doctorate or equivalent (Higher Degree)" ~ "Postgraduate",
    drinksDemography$Education == "Incomplete Secondary Education (Below GCSE/O Level)" ~ "Secondary or Below",
    drinksDemography$Education == "Some Vocational or Technical Qualifications" ~ "Vocational",
    TRUE ~ drinksDemography$Education
  )

drinksDemography$`Personal Income` <-
  case_when(
    drinksDemography$`Personal Income` == "No annual income" ~ "Low (No Annual Income to £29,999)",
    drinksDemography$`Personal Income` == "Less than £15,000" ~ "Low (No Annual Income to £29,999)",
    drinksDemography$`Personal Income` == "£15,000 - £19,999" ~ "Low (No Annual Income to £29,999)",
    drinksDemography$`Personal Income` == "£20,000 - £24,999" ~ "Low (No Annual Income to £29,999)",
    drinksDemography$`Personal Income` == "£25,000 - £29,999" ~ "Low (No Annual Income to £29,999)",
    drinksDemography$`Personal Income` == "£30,000 - £34,999" ~ "Middle (£30,000 to £49,999)",
    drinksDemography$`Personal Income` == "£35,000 - £39,999" ~ "Middle (£30,000 to £49,999)",
    drinksDemography$`Personal Income` == "£40,000 - £49,999" ~ "Middle (£30,000 to £49,999)",
    drinksDemography$`Personal Income` == "£50,000 - £69,999" ~ "High (£50,000 to £100,000 or More)",
    drinksDemography$`Personal Income` == "£70,000 - £99,999" ~ "High (£50,000 to £100,000 or More)",
    drinksDemography$`Personal Income` == "£100,000 or more" ~ "High (£50,000 to £100,000 or More)",
    TRUE ~ drinksDemography$`Personal Income`
  )

drinksDemography$`Work Status` <-
  case_when(
    drinksDemography$`Work Status` == "Working full time - working 30 hours per week or more" ~ "Employed",
    drinksDemography$`Work Status` == "Working part time - working less than 30 hours per week" ~ "Employed",
    drinksDemography$`Work Status` == "Not working/temporarily unemployed/sick but seeking work" ~ "Unemployed",
    drinksDemography$`Work Status` == "Not working and not seeking work" ~ "Unemployed",
    drinksDemography$`Work Status` == "Retired on a state pension only" ~ "Retired",
    drinksDemography$`Work Status` == "Retired with a private pension" ~ "Retired",
    drinksDemography$`Work Status` == "Student" ~ "Student",
    drinksDemography$`Work Status` == "Homemaker/Househusband/Housewife etc" ~ "Inactive",
    drinksDemography$`Work Status` == "Prefer not to say" ~ "Prefer not to say",
    TRUE ~ drinksDemography$`Work Status`
  )


#Rename urban/city centre and mixed descent so they are less verbose
drinks$urbanrural <- case_when(
  drinks$urbanrural == "Urban/City Centre" ~ "Urban Centre",
  TRUE ~ drinks$urbanrural
)

drinks$ethnicity <- case_when(
  drinks$ethnicity == "Mixed descent (e.g. White & Asian, White & Black)" ~ "Mixed descent",
  TRUE ~ drinks$ethnicity
)

#Build sample demographics table
demosTable <- datasummary(
  Gender + Education + `Personal Income` + Region + `Work Status`
  ~ N + Percent(),
  data = drinksDemography,
  title = "Table 1: Sample Demographics",
  notes = "Notes: Age (Mean = 48.70, SD = 17.85) is reported separately from the table due to data type conflicts.",
  output = "tinytable"
) |>
  style_tt(i = "caption", bold = TRUE)

#Calculate average age to report separately
mean(drinks$age_1)

#Build full demographics/controls table for appendix
fullControlsTableCat1 <- datasummary(
  gender + education + ethnicity ~ N + Percent(),
  data = drinks,
  title = "Table 1: Full Descriptive Statistics for Control Variables",
  notes = "Notes: Age (Mean = 48.70, SD = 17.85) is omitted from the table due to data type conflicts.",
  output = "tinytable"
) |>
  style_tt(i = "caption", bold = TRUE)

fullControlsTableCat2 <- datasummary(
  urbanrural + workstatus + finances ~ N + Percent(),
  data = drinks,
  title = "Table B1 continued: Full Descriptive Statistics for Control Variables",
  output = "tinytable"
) |>
  style_tt(i = "caption", bold = TRUE)

fullControlsTableCat3 <- datasummary(
  ownrent+ personalincome ~ N + Percent(),
  data = drinks,
  title = "Table B1 continued: Full Descriptive Statistics for Control Variables",
  output = "tinytable"
) |>
  style_tt(i = "caption", bold = TRUE)


#Save the tables
save_tt(fullControlsTableCat1, "fullControls1.html", overwrite = TRUE)
save_tt(fullControlsTableCat2, "fullControls2.html", overwrite = TRUE)
save_tt(fullControlsTableCat3, "fullControls3.html", overwrite = TRUE)
save_tt(demosTable, "demosTable.png", overwrite = TRUE)

#Cluster the IV and DV
drinks$`Clustered Voting Intention` <- case_when(
  drinks$`Voting Intention` == "Conservative" ~ "Right",
  drinks$`Voting Intention` == "Reform UK" ~ "Right",
  drinks$`Voting Intention` == "Liberal Democrat" ~ "Centre",
  drinks$`Voting Intention` == "Labour" ~ "Left",
  drinks$`Voting Intention` == "Scottish National Party (SNP)" ~ "Left",
  drinks$`Voting Intention` == "Plaid Cymru" ~ "Left",
  drinks$`Voting Intention` == "The Green Party" ~ "Left",
  TRUE ~ drinks$`Voting Intention`
)

#Collapse individual cocktails into one main cocktails category to build statistical power - all are considered 'aesthetic' beverages
drinks$`Beverage Preference` <- case_when(
  drinks$`Beverage Preference` == "Aperol Spritz" ~ "Cocktails",
  drinks$`Beverage Preference` == "Daiquiri" ~ "Cocktails",
  drinks$`Beverage Preference` == "Sex on the Beach" ~ "Cocktails",
  drinks$`Beverage Preference` == "Frozen margarita" ~ "Cocktails",
  TRUE ~ drinks$`Beverage Preference`
)

drinks$`Clustered Beverage Preference` <- case_when(
  drinks$`Beverage Preference` == "Cocktails" ~ "Aesthetic",
  drinks$`Beverage Preference` == "Rose wine" ~ "Aesthetic",
  drinks$`Beverage Preference` == "Pimms" ~ "Status Traditional",
  drinks$`Beverage Preference` == "Gin and Tonic" ~ "Status Traditional",
  drinks$`Beverage Preference` == "Lager" ~ "Mass Market",
  drinks$`Beverage Preference` == "Cider" ~ "Mass Market",
  drinks$`Beverage Preference` == "Not applicable - I do not drink alcohol" ~ "Non Drinker",
  TRUE ~ drinks$`Beverage Preference`
)

#Assign respondents to clusters with binary operators to facilitate logistic regression
drinks$RightCluster <- case_when(
  drinks$`Voting Intention` == "Conservative" ~ 1,
  drinks$`Voting Intention` == "Reform UK" ~ 1,
  TRUE ~ 0
)

drinks$LeftCluster <- case_when(
  drinks$`Voting Intention` == "Labour" ~ 1,
  drinks$`Voting Intention` == "The Green Party" ~ 1,
  drinks$`Voting Intention` == "Plaid Cymru" ~ 1,
  drinks$`Voting Intention` == "Scottish National Party (SNP)" ~ 1,
  TRUE ~ 0
)

drinks$CenterCluster <- case_when(
  drinks$`Voting Intention` == "Liberal Democrat" ~ 1,
  TRUE ~ 0
)

#Spaced versions of beverage clusters for the descriptive statistics
drinks$`Mass Market` <- case_when(
  drinks$`Beverage Preference` == "Lager" ~ 1,
  drinks$`Beverage Preference` == "Cider" ~ 1,
  TRUE ~ 0
)


drinks$Aesthetic <- case_when(
  drinks$`Beverage Preference` == "Cocktails" ~ 1,
  drinks$`Beverage Preference` == "Rose wine" ~ 1,
  TRUE ~ 0
)

drinks$`Status Traditional` <- case_when(
  drinks$`Beverage Preference` == "Gin and Tonic" ~ 1,
  drinks$`Beverage Preference` == "Pimms" ~ 1,
  TRUE ~ 0
)

drinks$`Does not drink` <- case_when(
  drinks$`Beverage Preference` == "Not applicable - I do not drink alcohol" ~ 1,
  TRUE ~ 0
)

#Build descriptive statistics for beverage cluster membership
votingintentionTable <- datasummary(
  `Voting Intention` ~ N + Percent(),
  data = drinks,
  title = "Table 2: Descriptive Statistics on Voting Intention",
  output = "tinytable"
) |>
  style_tt(i = "caption", bold = TRUE)

#Build descriptive statistics for beverage preference cluster membership
beveragePrefTableClustered <- datasummary(
  `Clustered Beverage Preference` ~ N + Percent(),
  data = drinks,
  title = "Table 3: Descriptive Statistics on Clustered Beverage Preference",
  output = "tinytable",
  notes = c("Notes:",
  "'Aesthetic' = aperol spritz, frozen margarita, daiquiri, rose wine;",
  "'Mass Market' = cider, lager;",
  "'Status Traditional' = gin and tonic, Pimm's.")
) |>
  style_tt(i = "caption", bold = TRUE)

#Save the tables
save_tt(beveragePrefTableClustered, "beveragePrefTableclusteredTable.png", overwrite = TRUE)
save_tt(votingintentionTable, "votingintenttable.png", overwrite = TRUE)

#Build descriptive statistics for clustered voting intention
clusteredVotingIntention <- datasummary(
  `Clustered Voting Intention` ~ N + Percent(),
  data = drinks,
  title = "Table 2: Descriptive Statistics on Clustered Voting Intention",
  output = "tinytable",
  notes = c(
    "Notes:",
    "'Right' = Conservative, Reform UK;",
    "'Left' = Labour, Scottish National Party (SNP), Plaid Cymru, The Green Party;",
    "'Centre' = Liberal Democrats."
  )
) |>
  style_tt(i = "caption", bold = TRUE)

#Save the table
save_tt(clusteredVotingIntention, "votingintenttableClustered.png", overwrite = TRUE)

#Non-spaced versions of the beverage cluster variables for the models to avoid issues that can arise from spaces
drinks$MassMarket <- drinks$`Mass Market`
drinks$StatusTraditional <- drinks$`Status Traditional`

#Beverage clusters, logistic regression (right)
M1 <- glm(RightCluster ~ MassMarket, data = drinks, family = "binomial")

M1C <- glm(RightCluster ~ MassMarket + gender + age_1 + region + ethnicity + education + urbanrural + workstatus + finances + ownrent + personalincome, data = drinks, family = "binomial")

M2 <- glm(RightCluster ~ StatusTraditional, data = drinks, family = "binomial")

M2C <- glm(RightCluster ~ StatusTraditional + gender + age_1 + region + ethnicity + education + urbanrural + workstatus + finances + ownrent + personalincome, data = drinks, family = "binomial")

M3 <- glm(RightCluster ~ Aesthetic, data = drinks, family = "binomial")

M3C <- glm(RightCluster ~ Aesthetic + gender + age_1 + region + ethnicity + education + urbanrural + workstatus + finances + ownrent + personalincome, data = drinks, family = "binomial")


#Beverage clusters, logistic regression (left)
M4 <- glm(LeftCluster ~ MassMarket, data = drinks, family = "binomial")

M4C <- glm(LeftCluster ~ MassMarket + gender + age_1 + region + ethnicity + education + urbanrural + workstatus + finances + ownrent + personalincome, data = drinks, family = "binomial")

M5 <- glm(LeftCluster ~ StatusTraditional, data = drinks, family = "binomial")

M5C <- glm(LeftCluster ~ StatusTraditional + gender + age_1 + region + ethnicity + education + urbanrural + workstatus + finances + ownrent + personalincome, data = drinks, family = "binomial")

M6 <- glm(LeftCluster ~ Aesthetic, data = drinks, family = "binomial")

M6C <- glm(LeftCluster ~ Aesthetic + gender + age_1 + region + ethnicity + education + urbanrural + workstatus + finances + ownrent + personalincome, data = drinks, family = "binomial")

#Robustness, right
M7 <- glm(RightCluster ~ `Unclustered Beverage Preference`, data = drinks, family = "binomial")

M7C <- glm(RightCluster ~ `Unclustered Beverage Preference` + gender + age_1 + region + ethnicity + education + urbanrural + workstatus + finances + ownrent + personalincome, data = drinks, family = "binomial")

#Change order of variables
coefOrder <- c("MassMarket", "StatusTraditional", "Aesthetic")

stargazer(
  M7, M7C,
  
  type = "html",
  out = "robustnessrightbeverage_models.html",
  
  title = "Table E1: Logistic Regression Models Predicting Right Voting (Unclustered IV)",
  dep.var.labels = "Right voting intention",
  
  column.labels = c(
    "M7",
    "M7 Control"
  ),
  
  model.numbers = FALSE,
  
  keep = "Lager|Gin and Tonic|None of the above|Not applicable - I do not drink alcohol|Cider|Aperol Spritz|Pimms|Rose wine|Sex on the Beach|Frozen margarita|Daiquiri",
  
  order = coefOrder,
  omit.stat = c("ser", "f"),
  add.lines = list(c(
    "McFadden pseudo R²",
    round(pR2(M7)["McFadden"], 3),
    round(pR2(M7C)["McFadden"], 3)
  )
  ),
  digits = 3,
  single.row = FALSE,
  no.space = TRUE,
  
  notes = "Table shows coefficients with standard errors in parentheses.<br>Controls omitted for brevity."
)

#Robustness, left
M8 <- glm(LeftCluster ~ `Unclustered Beverage Preference`, data = drinks, family = "binomial")

M8C <- glm(LeftCluster ~ `Unclustered Beverage Preference` + gender + age_1 + region + ethnicity + education + urbanrural + workstatus + finances + ownrent + personalincome, data = drinks, family = "binomial")

stargazer(
  M8, M8C,
  
  type = "html",
  out = "robustnessleftbeverage_models.html",
  
  title = "Table E2: Logistic Regression Models Predicting Left Voting (Unclustered IV)",
  dep.var.labels = "Left voting intention",
  
  column.labels = c(
    "M8",
    "M8 Control"
  ),
  
  model.numbers = FALSE,
  
  keep = "Lager|Gin and Tonic|None of the above|Not applicable - I do not drink alcohol|Cider|Aperol Spritz|Pimms|Rose wine|Sex on the Beach|Frozen margarita|Daiquiri",
  
  order = coefOrder,
  omit.stat = c("ser", "f"),
  add.lines = list(c(
    "McFadden pseudo R²",
    round(pR2(M8)["McFadden"], 3),
    round(pR2(M8C)["McFadden"], 3)
  )
  ),
  digits = 3,
  single.row = FALSE,
  no.space = TRUE,
  
  notes = "Table shows coefficients with standard errors in parentheses.<br>Controls omitted for brevity."
)

#Psuedo R^2
pR2(M1)["McFadden"]
pR2(M1C)["McFadden"]
pR2(M2)["McFadden"]
pR2(M2C)["McFadden"]
pR2(M3)["McFadden"]
pR2(M3C)["McFadden"]

pR2(M4)["McFadden"]
pR2(M4C)["McFadden"]
pR2(M5)["McFadden"]
pR2(M5C)["McFadden"]
pR2(M6)["McFadden"]
pR2(M6C)["McFadden"]
pR2(M7)["McFadden"]
pR2(M7C)["McFadden"]

#Coefficient table for left voting
stargazer(
  M4, M4C,
  M5, M5C,
  M6, M6C,
  
  type = "html",
  out = "left_wing_beverage_models.html",
  
  title = "Table 5: Logistic Regression Models Predicting Left Voting",
  dep.var.labels = "Left voting intention",
  
  column.labels = c(
    "M4",
    "M4 Control",
    "M5",
    "M5 Control",
    "M6",
    "M6 Control"
  ),
  
  model.numbers = FALSE,
  
  keep = "MassMarket|StatusTraditional|Aesthetic",
  
  order = coefOrder,
  omit.stat = c("ser", "f"),
  add.lines = list(c(
    "McFadden pseudo R²",
    round(pR2(M4)["McFadden"], 3),
    round(pR2(M4C)["McFadden"], 3),
    round(pR2(M5)["McFadden"], 3),
    round(pR2(M5C)["McFadden"], 3),
    round(pR2(M6)["McFadden"], 3),
    round(pR2(M6C)["McFadden"], 3)
  )
  ),
  digits = 3,
  single.row = FALSE,
  no.space = TRUE,
  
  notes = "Table shows coefficients with standard errors in parentheses.<br>Controls omitted for brevity (for full table see appendix)."
)

#Coefficient table for right voting
stargazer(
  M1, M1C,
  M2, M2C,
  M3, M3C,
  
  type = "html",
  out = "right_wing_beverage_models.html",
  
  title = "Table 4: Logistic Regression Models Predicting Right Voting",
  dep.var.labels = "Right voting intention",
  
  column.labels = c(
    "M1",
    "M1 Control",
    "M2",
    "M2 Control",
    "M3",
    "M3 Control"
  ),
  
  model.numbers = FALSE,
  
  keep = "MassMarket|StatusTraditional|Aesthetic",
  
  order = coefOrder,
  
  omit.stat = c("ser", "f"),
  add.lines = list(c(
    "McFadden pseudo R²",
    round(pR2(M1)["McFadden"], 3),
    round(pR2(M1C)["McFadden"], 3),
    round(pR2(M2)["McFadden"], 3),
    round(pR2(M2C)["McFadden"], 3),
    round(pR2(M3)["McFadden"], 3),
    round(pR2(M3C)["McFadden"], 3)
    )
                   ),
  digits = 3,
  single.row = FALSE,
  no.space = TRUE,
  
  notes = "Table shows coefficients with standard errors in parentheses.<br>Controls omitted for brevity (for full table see appendix)."
)

#Generate predicted probabilities with ggeffects
M2CPreds <- ggpredict(M2C, terms = "StatusTraditional")
M2CDF <- as.data.frame(M2CPreds)

M5CPreds <- ggpredict(M5C, terms = "StatusTraditional")
M5CDF <- as.data.frame(M5CPreds)

#Create predicted probability plot for right voting and status traditional
M2CVis <- ggplot(
  data = M2CPreds,
  aes(x = factor(x), y = predicted)
) + 
  geom_linerange(
    aes(ymin = conf.low, ymax = conf.high, colour = factor(x)),
    linewidth = 0.6
  ) +
  geom_point(
    aes(fill = factor(x)),
    shape = 21,
    colour = "black",
    size = 3,
    stroke = 0.5
  ) +
  scale_colour_manual(
    values = c("0" = "#000000", "1" = "#000000"),
    labels = c("0" = "Other", "1" = "Status Traditional")
  ) +
  scale_fill_manual(
    values = c("0" = "#000000", "1" = "#000000"),
    labels = c("0" = "Other", "1" = "Status Traditional")
  ) +
  scale_x_discrete(labels = c(
    "0" = "Other",
    "1" = "Status Traditional"
  )) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Figure 1: Predicted Probability of Right Voting Intention by Beverage Preference",
    x = "Beverage preference",
    y = "Predicted probability"
  ) +
  theme_clean() +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    panel.border = element_blank(),
    plot.background = element_blank()
  ) +
  guides(fill = "none", colour = "none")

#Save visualisation
ggsave("M2CVis.png", plot = M2CVis, width = 8, height = 5, dpi = 300, bg = "transparent")

#Create predicted probability plot for left coting and status traditional 
M5CVis <- ggplot(
  data = M5CPreds,
  aes(x = factor(x), y = predicted)
) + 
  geom_linerange(
    aes(ymin = conf.low, ymax = conf.high, colour = factor(x)),
    linewidth = 0.6
  ) +
  geom_point(
    aes(fill = factor(x)),
    shape = 21,
    colour = "black",
    size = 3,
    stroke = 0.5
  ) +
  scale_colour_manual(
    values = c("0" = "#000000", "1" = "#000000"),
    labels = c("0" = "Other", "1" = "Status Traditional")
  ) +
  scale_fill_manual(
    values = c("0" = "#000000", "1" = "#000000"),
    labels = c("0" = "Other", "1" = "Status Traditional")
  ) +
  scale_x_discrete(labels = c(
    "0" = "Other",
    "1" = "Status Traditional"
  )) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Figure 2: Predicted Probability of Left Voting Intention by Beverage Preference",
    x = "Beverage preference",
    y = "Predicted probability"
  ) +
  theme_clean() +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    panel.border = element_blank(),
    plot.background = element_blank()
  ) +
  guides(fill = "none", colour = "none")

#Save visualisation
ggsave("M5CVis.png", plot = M5CVis, width = 8, height = 5, dpi = 300, bg = "transparent")

#Full coefficient table for right voting
stargazer(
  M1, M1C,
  M2, M2C,
  M3, M3C,
  
  type = "html",
  out = "appendixright1.html",
  
  order = coefOrder,
  
  title = "Table D1: Logistic Regression Models Predicting Right Voting (Full Table)",
  dep.var.labels = "Right voting intention",
  
  column.labels = c("M1", "M1 Control", "M2", "M2 Control","M3", "M3 Control"),
  model.numbers = FALSE,
  
  keep = "MassMarket|Aesthetic|StatusTraditional|gender|age_1|ethnicity|education",
  
  omit.stat = c("ser", "f"),
  digits = 3,
  single.row = FALSE,
  no.space = TRUE,
  
  notes = "Table shows coefficients with standard errors in parentheses."
)

stargazer(
  M1, M1C,
  M2, M2C,
  M3, M3C,
  
  type = "html",
  out = "appendixright2.html",
  
  order = coefOrder,
  
  title = "Table D1 continued: Logistic Regression Models Predicting Right Voting (Full Table)",
  dep.var.labels = "Right voting intention",
  
  column.labels = c("M1", "M1 Control", "M2", "M2 Control","M3", "M3 Control"),
  model.numbers = FALSE,
  
  keep = "urbanrural|workstatus|finances",
  
  omit.stat = c("ser", "f"),
  digits = 3,
  single.row = FALSE,
  no.space = TRUE,
  
  notes = "Table shows coefficients with standard errors in parentheses."
)

stargazer(
  M1, M1C,
  M2, M2C,
  M3, M3C,
  
  type = "html",
  out = "appendixright3.html",
  
  order = coefOrder,
  
  title = "Table D1 continued: Logistic Regression Models Predicting Right Voting (Full Table)",
  dep.var.labels = "Right voting intention",
  
  column.labels = c("M1", "M1 Control", "M2", "M2 Control","M3", "M3 Control"),
  model.numbers = FALSE,
  
  keep = "ownrent|personalincome",
  
  omit.stat = c("ser", "f"),
  digits = 3,
  single.row = FALSE,
  no.space = TRUE,
  
  notes = "Table shows coefficients with standard errors in parentheses."
)

#Full coefficient table for left voting
stargazer(
  M4, M4C,
  M5, M5C,
  M6, M6C,
  
  type = "html",
  out = "appendixleft1.html",
  
  order = coefOrder,
  
  title = "Table D2: Logistic Regression Models Predicting Left Voting (Full Table)",
  dep.var.labels = "Left voting intention",
  
  column.labels = c("M4", "M4 Control", "M5", "M5 Control","M6", "M6 Control"),
  model.numbers = FALSE,
  
  keep = "MassMarket|Aesthetic|StatusTraditional|gender|age_1|ethnicity|education",
  
  omit.stat = c("ser", "f"),
  digits = 3,
  single.row = FALSE,
  no.space = TRUE,
  
  notes = "Table shows coefficients with standard errors in parentheses."
)

stargazer(
  M4, M4C,
  M5, M5C,
  M6, M6C,
  
  type = "html",
  out = "appendixleft2.html",
  
  order = coefOrder,
  
  title = "Table D2 continued: Logistic Regression Models Predicting Left Voting (Full Table)",
  dep.var.labels = "Left voting intention",
  
  column.labels = c("M4", "M4 Control", "M5", "M5 Control","M6", "M6 Control"),
  model.numbers = FALSE,
  
  keep = "urbanrural|workstatus|finances",
  
  omit.stat = c("ser", "f"),
  digits = 3,
  single.row = FALSE,
  no.space = TRUE,
  
  notes = "Table shows coefficients with standard errors in parentheses."
)

stargazer(
  M4, M4C,
  M5, M5C,
  M6, M6C,
  
  type = "html",
  out = "appendixleft3.html",
  
  order = coefOrder,
  
  title = "Table D2 continued: Logistic Regression Models Predicting Left Voting (Full Table)",
  dep.var.labels = "Left voting intention",
  
  column.labels = c("M4", "M4 Control", "M5", "M5 Control","M6", "M6 Control"),
  model.numbers = FALSE,
  
  keep = "ownrent|personalincome",
  
  omit.stat = c("ser", "f"),
  digits = 3,
  single.row = FALSE,
  no.space = TRUE,
  
  notes = "Table shows coefficients with standard errors in parentheses."
)
