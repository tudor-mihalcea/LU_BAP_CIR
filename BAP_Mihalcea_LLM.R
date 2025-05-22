library (MASS)
library (ggResidpanel)
library (car)
library (expss)
library (rio)
library (broom)
library (dplyr)
library (readr)
library (caret)
library (httr)
library (stringr)
library (progress)
library (e1071)
library (lubridate)
library (scales)
library (readxl)
library (sjPlot)
library (ggeffects)
library (ggplot2)
library (tidyverse)
library (psych)
library (summarytools)
library (flextable)
library (janitor)

setwd("C:/Users/Tud/Desktop")
survey <- read_excel("Dataset FINAL.xlsx", sheet = 1)
str(survey)
survey[ ,c("Q11_1", "Q11_2", "Q11_3", "Q11_4", "Q11_5", "Q11_6",
       "Q11_7", "Q11_8", "Q11_9", "Q11_10", "Q11_11", "Q11_12",
       "Q11_13", "Q11_14", "Q11_15", "Q11_16")] <- 
  lapply(survey[ ,c("Q11_1", "Q11_2", "Q11_3", "Q11_4", "Q11_5", "Q11_6",
                "Q11_7", "Q11_8", "Q11_9", "Q11_10", "Q11_11", "Q11_12",
                "Q11_13", "Q11_14", "Q11_15", "Q11_16")], as.numeric)

names (survey)[names(survey) == "Q8"] <- "open_q1"
names (survey)[names(survey) == "Q9"] <- "open_q2"
names (survey)[names(survey) == "Q13"] <- "open_q3"

survey$open_q1 <- as.character(survey$open_q1)
survey$open_q2 <- as.character(survey$open_q2)
survey$open_q3 <- as.character(survey$open_q3)

str(survey$Finished)

survey_clean <- survey %>%
  filter (Finished == "True")

#LLM FUNCTION TO CONNECT TO GPT-4o
send_to_gpt <- function (text, api_key, model = "gpt-4o", retries = 3) {
  url <- "https://api.openai.com/v1/chat/completions"
  #API REQUEST
  attempt <- 1
  while (attempt <= retries) {
    response <- POST (
      url = url,
      add_headers (Authorization = paste ("Bearer", api_key)),
      content_type_json (),
      body = list (
        model = model,
        messages = list (list(role = "user", content = text))
      ),
      encode = "json"
    )
    
    #PARSED RESPONSE
    parsed_response <- content (response, as = "parsed")
    if (!is.null (parsed_response$choices) && length(parsed_response$choices) > 0) {
      return (parsed_response$choices[[1]]$message$content)
    }
    else {
      print (paste ("Attempt", attempt, ": No valid response from API key"))
      return (NULL)
    }
    attempt <- attempt + 1
    Sys.sleep (1)
  }
  return (NULL)
}

#GPT PROMPT FOR QUESTION NUMBER 1
api_key <- "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"
analyze_answer_openq1 <- function (open_question) {
  if (is.na(open_question) || trimws(open_question) == "") {
    return(NA)
  }
  prompt <- paste (
    "You are a political scientist analyzing how Romanians express a sense of collective victimhood in relation to Western institutions or countries (e.g. EU, US, West Europe)",
    "The question to which people gave answers is: 'În opinia dvs., cum sunt privite România și românii de către țările și instituțiile occidentale?' ",
    "Rate the following Romanian response on a scale from -1 to +1, you can use decimals as well for precision and accuracy",
    "-1 = very low perceived victimhood, 0 = neutral, balanced or unclear, 1 = strong perceived victimhood (clear belief that Romania is treated unfairly or looked down upon)",
    "Take into account things such as indirect or sarcastic phrases, Romanian abbreviations or informal language or emotional tone, even if it is implicit",
    "Iterate it as 'Victimhood Score: [Score]'",
    "Response is as follows:", open_question
  )
  response <- send_to_gpt(prompt, api_key)
  print(paste("GPT Response:", response))
  if (!is.null(response)) {
    score_text <- str_extract(response, "-?\\d+\\.?\\d*")
    print(paste("Extracted Victimhood Score:", score_text))
    victimhood <- ifelse(!is.na(score_text), as.numeric(score_text), NA)
    return (c(victimhood))
  }
  else {
    return (c(NA))
  }
}

#GPT PROMPT FOR QUESTION NUMBER 2
analyze_answer_openq2 <- function (open_question) {
  if (is.na(open_question) || trimws(open_question) == "") {
    return(NA)
  }
  prompt <- paste (
    "You are a political scientist analyzing how Romanians express a sense of collective victimhood in relation to Western institutions or countries (e.g. EU, US, West Europe)",
    "The question to which people gave answers is: 'Considerați că România este tratată corect în cadrul Uniunii Europene (UE)? De ce da/ De ce nu?' ",
    "Rate the following Romanian response on a scale from -1 to +1, you can use decimals as well for precision and accuracy",
    "-1 = very low perceived victimhood, 0 = neutral, balanced or unclear, 1 = strong perceived victimhood (clear belief that Romania is treated unfairly or looked down upon)",
    "Take into account things such as indirect or sarcastic phrases, Romanian abbreviations or informal language or emotional tone, even if it is implicit",
    "Iterate it as 'Victimhood Score: [Score]'",
    "Response is as follows:", open_question
  )
  response <- send_to_gpt(prompt, api_key)
  print(paste("GPT Response:", response))
  if (!is.null(response)) {
    score_text <- str_extract(response, "-?\\d+\\.?\\d*")
    print(paste("Extracted Victimhood Score:", score_text))
    victimhood <- ifelse(!is.na(score_text), as.numeric(score_text), NA)
    return (c(victimhood))
  }
  else {
    return (c(NA))
  }
}

#GPT PROMPT FOR QUESTION NUMBER 3
analyze_answer_openq3 <- function (open_question) {
  if (is.na(open_question) || trimws(open_question) == "") {
    return(NA)
  }
  prompt <- paste (
    "You are a political scientist analyzing how Romanians express a sense of collective victimhood in relation to Western institutions or countries (e.g. EU, US, West Europe)",
    "The question to which people gave answers is: 'Credeți că valorile și cultura României sunt respectate de țările și instituțiile occidentale? De ce da/De ce nu?' ",
    "Rate the following Romanian response on a scale from -1 to +1, you can use decimals as well for precision and accuracy",
    "-1 = very low perceived victimhood, 0 = neutral, balanced or unclear, 1 = strong perceived victimhood (clear belief that Romania is treated unfairly or looked down upon)",
    "Take into account things such as indirect or sarcastic phrases, Romanian abbreviations or informal language or emotional tone, even if it is implicit",
    "Iterate it as 'Victimhood Score: [Score]'",
    "Response is as follows:", open_question
  )
  response <- send_to_gpt(prompt, api_key)
  print(paste("GPT Response:", response))
  if (!is.null(response)) {
    score_text <- str_extract(response, "-?\\d+\\.?\\d*")
    print(paste("Extracted Victimhood Score:", score_text))
    victimhood <- ifelse(!is.na(score_text), as.numeric(score_text), NA)
    return (c(victimhood))
  }
  else {
    return (c(NA))
  }
}

#CLASSIFYING OPEN QUESTION 1 ANSWERS: (How are Romanians and Romania perceived by Western Institutions and Countries)
results_open1 <- matrix (NA, nrow(survey_clean), ncol = 1)
colnames (results_open1) <- c("VictimhoodScore")
pb_q1 <- txtProgressBar(min = 0, max = nrow(survey_clean), style = 3)
for (i in 1:nrow(survey_clean)) {
  results_open1[i, ] <- analyze_answer_openq1 (survey_clean$open_q1[i])
  setTxtProgressBar (pb_q1, i)
  Sys.sleep(1)
}
close(pb_q1)
q1_df <- as.data.frame (results_open1, stringsAsFactors = FALSE)
colnames (q1_df) <- c("VictimhoodScore Open 1")

#CLASSIFYING OPEN QUESTION 2 ANSWERS: (Do you consider Romania to be treated fairly in the EU? Why yes or why not?)
results_open2 <- matrix (NA, nrow(survey_clean), ncol = 1)
colnames (results_open2) <- c("VictimhoodScore")
pb_q2 <- txtProgressBar(min = 0, max = nrow(survey_clean), style = 3)
for (i in 1:nrow(survey_clean)) {
  results_open2[i, ] <- analyze_answer_openq2 (survey_clean$open_q2[i])
  setTxtProgressBar (pb_q2, i)
  Sys.sleep(1)
}
close(pb_q2)
q2_df <- as.data.frame (results_open2, stringsAsFactors = FALSE)
colnames (q2_df) <- c("VictimhoodScore Open 2")

#CLASSIFYING OPEN QUESTION 3 ANSWERS: (Do you believe Romanian values are being respected by the Western countries and institutions? Why yes or why not?)
results_open3 <- matrix (NA, nrow(survey_clean), ncol = 1)
colnames (results_open3) <- c("VictimhoodScore")
pb_q3 <- txtProgressBar (min = 0, max = nrow(survey_clean), style = 3)
for (i in 1:nrow(survey_clean)) {
  results_open3[i, ] <- analyze_answer_openq3 (survey_clean$open_q3[i])
  setTxtProgressBar (pb_q3, i)
  Sys.sleep(1)
}
close (pb_q3)
q3_df <- as.data.frame (results_open3, stringsAsFactors = FALSE)
colnames (q3_df) <- c("VictimhoodScore Open 3")

survey_clean$PerceivedVictimhood <- rowMeans (
  cbind (q1_df$`VictimhoodScore Open 1`, q2_df$`VictimhoodScore Open 2`, q3_df$`VictimhoodScore Open 3`),
  na.rm = TRUE
)
survey_clean$PerceivedVictimhood[is.nan(survey_clean$PerceivedVictimhood)] <- NA
survey_clean$PerceivedVictimhood[survey_clean$PerceivedVictimhood > 1] <- 1

survey_clean$FarRightScore <- round(survey_clean$FarRightScore, 2)
survey_clean$PerceivedVictimhood <- round(survey_clean$PerceivedVictimhood, 2)
survey_clean$Euroskepticism <- round(survey_clean$Euroskepticism, 2)

survey_clean$Q6 <- trimws(survey_clean$Q6)
survey_clean$Q6[survey_clean$Q6 == "Prefer să nu spun."] <- NA
survey_clean$income <- as.numeric(factor(
  survey_clean$Q6,
  levels = c("Sub 1500 RON", "1500-3000 RON", "3001-5000 RON", "5001-8000 RON", "Peste 8000 RON")
))

ggplot(survey_clean, aes(x = PerceivedVictimhood, y = FarRightScore)) +
  geom_point(alpha = 0.4, size = 2, color = "darkblue") +
  geom_smooth(method = "loess", se = TRUE, color = "darkred", size = 1.2) +
  labs(
    title = "Relationship Between Perceived Victimhood and Far-Right Alignment",
    x = "Perceived Victimhood Score (–1 to +1)",
    y = "Far-Right Alignment Score (1 to 5)"
  ) +
  theme_minimal()

#MODEL FOR FAR-RIGHT SCORE AND PERCEIVED VICTIMHOOD ONLY
model <- lm(FarRightScore ~ PerceivedVictimhood, data = survey_clean)
summary (model)
tab_model(model,
          show.std = TRUE,
          show.ci = FALSE,
          show.p = TRUE,
          show.se = TRUE,
          dv.labels = "Far-Right Alignment",
          title = "Linear Regression Predicting Far-Right Alignment",
          pred.labels = c(
            "Intercept",
            "Perceived Victimhood"
          ),
          digits = 3)

#MODEL FOR FAR-RIGHT SCORE AND PERCEIVED VICTIMHOOD WITH AGE AS CONTROL
survey_clean$age <- as.numeric(survey_clean$Q3)
model_age <- lm(FarRightScore ~ PerceivedVictimhood + age, data = survey_clean)
summary(model_age)
tab_model(model_age,
          show.std = TRUE,
          show.ci = FALSE,
          show.p = TRUE,
          show.se = TRUE,
          dv.labels = "Far-Right Alignment",
          title = "Linear Regression Predicting Far-Right Alignment",
          pred.labels = c(
            "Intercept",
            "Perceived Victimhood",
            "Age"
          ),
          digits = 3)


#MODEL FOR FAR-RIGHT SCORE AND PERCEIVED VICTIMHOOD WITH BOTH AGE AND RELIGIOSITY AS CONTROL
survey_clean$Q7 <- trimws(survey_clean$Q7)
survey_clean$Q7[survey_clean$Q7 == "Prefer să nu spun."] <- NA

survey_clean$religiosity <- factor(
  survey_clean$Q7,
  levels = c("Deloc", "Puțin religios/religioasă", "Moderat de religios/religioasă", 
             "Religios/Religioasă", "Foarte religios/religioasă"),
  ordered = FALSE
)
table(survey_clean$religiosity)
survey_clean$religiosity <- relevel(survey_clean$religiosity, ref = "Moderat de religios/religioasă")
model_age_relig <- lm(FarRightScore ~ PerceivedVictimhood + age + religiosity, data = survey_clean)
summary(model_age_relig)

tab_model(model_age_relig,
          show.std = TRUE,
          show.ci = FALSE,
          show.p = TRUE,
          show.se = TRUE,
          dv.labels = "Far-Right Alignment",
          title = "Linear Regression Predicting Far-Right Alignment",
          pred.labels = c(
            "Intercept",
            "Perceived Victimhood",
            "Age",
            "Not religious",
            "Slightly religious",
            "Religious"
          ),
          digits = 3)

#CHECKING MODEL FIT
model_data <- survey_clean %>%
  select (FarRightScore, PerceivedVictimhood, age, religiosity) %>%
    na.omit()
model1 <- lm (FarRightScore ~ PerceivedVictimhood, data = model_data)
model2 <- lm (FarRightScore ~ PerceivedVictimhood + age, data = model_data)
model3 <- lm (FarRightScore ~ PerceivedVictimhood + age + religiosity, data = model_data)
anova (model1, model2, model3)

#PREDICTED VALUE PLOTS
predictions_far_right <- ggpredict (model3, terms = "PerceivedVictimhood")
plot(predictions_far_right) +
  labs (
    title = "Predicted Far-Right Alignment by Perceived Victimhood",
    x = "Perceived Victimhood Score (–1 to +1)",
    y = "Predicted Far-Right Alignment Score (1 to 5)"
  ) +
  theme_minimal()

#MODELS FOR EUROSKEPTICISM
euro_model_data <- survey_clean %>%
  select (Euroskepticism, PerceivedVictimhood, age, religiosity) %>%
    na.omit()
euro_model1 <- lm (Euroskepticism ~ PerceivedVictimhood, data = euro_model_data)
euro_model2 <- lm (Euroskepticism ~ PerceivedVictimhood + age, data = euro_model_data)
euro_model3 <- lm (Euroskepticism ~ PerceivedVictimhood + age + religiosity, data = euro_model_data)
anova (euro_model1, euro_model2, euro_model3)
ggplot(survey_clean, aes(x = PerceivedVictimhood, y = Euroskepticism)) +
  geom_point(alpha = 0.4, size = 2, color = "darkblue") +
  geom_smooth(method = "loess", se = TRUE, color = "darkred", size = 1.2) +
  labs(
    title = "Relationship Between Perceived Victimhood and Euroskepticism",
    x = "Perceived Victimhood Score (–1 to +1)",
    y = "Euroskepticism Score (1 to 5)"
  ) +
  theme_minimal()
summary (euro_model1)
tab_model(euro_model1,
          show.std = TRUE,
          show.ci = FALSE,
          show.p = TRUE,
          show.se = TRUE,
          dv.labels = "Euroskepticism",
          title = "Linear Regression Predicting Euroskepticism",
          pred.labels = c(
            "Intercept",
            "Perceived Victimhood"
          ),
          digits = 3)
summary(euro_model2)
tab_model(euro_model2,
          show.std = TRUE,
          show.ci = FALSE,
          show.p = TRUE,
          show.se = TRUE,
          dv.labels = "Euroskepticism",
          title = "Linear Regression Predicting Euroskepticism",
          pred.labels = c(
            "Intercept",
            "Perceived Victimhood",
            "Age"
          ),
          digits = 3)
summary(euro_model3)
tab_model(euro_model3,
          show.std = TRUE,
          show.ci = FALSE,
          show.p = TRUE,
          show.se = TRUE,
          dv.labels = "Euroskepticism",
          title = "Linear Regression Predicting Euroskepticism",
          pred.labels = c(
            "Intercept",
            "Perceived Victimhood",
            "Age",
            "Not religious",
            "Slightly religious",
            "Religious"
          ),
          digits = 3)

#PREDICTED VALUES PLOT
predictions_euroskepticism <- ggpredict (euro_model3, terms = "PerceivedVictimhood")
plot(predictions_euroskepticism) +
  labs (
    title = "Predicted Euroskepticism by Perceived Victimhood",
    x = "Perceived Victimhood Score (–1 to +1)",
    y = "Predicted Euroskepticism Score (1 to 5)"
  ) +
  theme_minimal()

#ASSUMPTIONS FOR THE FAR-RIGHT MODEL
####################################
#INDEPENDENCE OF ERRORS
durbinWatsonTest(model_age_relig)
#MULTICOLLINEARITY
vif(model_age_relig)
#LINEARITY AND ADDITIVITY + HOMOSKEDASTICITY
resid_panel (model_age_relig, plots = c("resid"))
avPlots (model_age_relig)
#NORMAL DISTRIBUTION OF ERRORS
resid_panel (model_age_relig, plots = c("hist", "qq"))
#OUTLIERS
outliers_data <- augment (model_age_relig)
summary(outliers_data$.std.resid)
outliers_data <- outliers_data |>
  mutate(SRE1.96 = case_when(
    .std.resid > 1.96 | .std.resid < -1.96  ~ 1,
    .std.resid > -1.96 & .std.resid < 1.96 ~ 0),
    SRE2.58 = case_when(
      .std.resid > 2.58 | .std.resid < -2.58  ~ 1,
      .std.resid > -2.58 & .std.resid < 2.58 ~ 0),
    SRE3.29 = case_when(
      .std.resid > 3.29 | .std.resid < -3.29  ~ 1,
      .std.resid > -3.29 & .std.resid < 3.29 ~ 0
    ))
fre (outliers_data$SRE1.96)
fre (outliers_data$SRE2.58)
fre (outliers_data$SRE3.29)
model_age_relig_1.96 <- lm(FarRightScore ~ PerceivedVictimhood + age + religiosity, data = subset (outliers_data, SRE1.96 == 0))
summary(model_age_relig_1.96)
summary(model_age_relig)
#INFLUENTIAL CASES
summary (outliers_data$.cooksd)
resid_panel (model_age_relig, plots = c("cookd"))

#ASSUMPTIONS FOR THE EUROSKEPTICISM MODEL
#########################################
#INDEPENDENCE OF ERRORS
durbinWatsonTest (euro_model3)
#MULTICOLLINEARITY
vif(euro_model3)
#LINEARITY + ADDITIVITY + HOMOSKEDASTICITY
resid_panel (euro_model3, plots = c("resid"))
avPlots (euro_model3)
#NORMAL DISTRIBUTION OF ERRORS
resid_panel (euro_model3, plots = c("hist", "qq"))
#OUTLIERS
euro_outliers_data <- augment (euro_model3)
summary(euro_outliers_data$.std.resid)
euro_outliers_data <- euro_outliers_data |>
  mutate(SRE1.96 = case_when(
    .std.resid > 1.96 | .std.resid < -1.96  ~ 1,
    .std.resid > -1.96 & .std.resid < 1.96 ~ 0),
    SRE2.58 = case_when(
      .std.resid > 2.58 | .std.resid < -2.58  ~ 1,
      .std.resid > -2.58 & .std.resid < 2.58 ~ 0),
    SRE3.29 = case_when(
      .std.resid > 3.29 | .std.resid < -3.29  ~ 1,
      .std.resid > -3.29 & .std.resid < 3.29 ~ 0
    ))
fre (euro_outliers_data$SRE1.96)
fre (euro_outliers_data$SRE2.58)
fre (euro_outliers_data$SRE3.29)
euro_model3_1.96 <- lm(Euroskepticism ~ PerceivedVictimhood + age + religiosity, data = subset (euro_outliers_data, SRE1.96 == 0))
summary (euro_model3_1.96)
summary (euro_model3)
#INFLUENTIAL CASES
summary (euro_outliers_data$.cooksd)
resid_panel (euro_model3, plots = c("cookd"))

#DESCRIPTIVE STATS
describe (survey_clean[, c("FarRightScore", "PerceivedVictimhood", "Euroskepticism")])
dfSummary (survey_clean[, c("FarRightScore", "PerceivedVictimhood", "Euroskepticism")])
summary_table <- data.frame(
  Variable = c("FarRightScore", "PerceivedVictimhood", "Euroskepticism"),
  Mean = c(2.81, -0.07, 2.26),
  SD = c(0.85, 0.44, 0.88),
  Min = c(1.00, -1.00, 1.00),
  Max = c(5.00, 1.00, 4.60),
  N = c(185, 184, 185)
)
flextable(summary_table)

#PEARSON CORRELATION
cor.test(survey_clean$PerceivedVictimhood, survey_clean$FarRightScore, method = "pearson")
cor.test(survey_clean$PerceivedVictimhood, survey_clean$Euroskepticism, method = "pearson")
