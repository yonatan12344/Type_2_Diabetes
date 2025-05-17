library(shiny)
library(ggplot2)
### Prexisting Statistical Analysis
df <- read.csv("Type2_Diabetes_Dataset.csv")
library(dplyr)
library(reshape2)
library(tseries)
library(ggplot2)
library(performance)
# Dealing with outliers and missing data
# This paper, https://pmc.ncbi.nlm.nih.gov/articles/PMC10834804/ confirms that there
# are no missing values within the dataset
# Resampling to deal with Bias, we want to have the same number of people with diabetes
# and without, a random seem for reproducibility is included.
set.seed(123)
non_diabetic <- df %>%
  filter(Outcome == 0)
diabetic_93 <- df %>%
  filter(Outcome == 1) %>%
  sample_n(nrow(non_diabetic))
balanced_dataset <- bind_rows(non_diabetic, diabetic_93)
# Outliers check
boxplot(balanced_dataset$Age, main = "Age Boxplot", ylab = "Ages", col = 'blue')
boxplot(balanced_dataset$BMI, main = "BMI Boxplot", ylab = "BMI", col = 'blue')
boxplot(balanced_dataset$Skin.Thickness.mm., main = "Skin thickness Boxplot", ylab = "thickness", col = 'blue')
boxplot(balanced_dataset$BP.Systolic., main = "BP Systolic", ylab = "BP", col = 'blue')
boxplot(balanced_dataset$BP.Diastolic., main = "BP Diastolic", ylab = "BP number", col = 'blue')
boxplot(balanced_dataset$DiabetesPedigreeFunction, main = "Diabetes Pedigree Function", ylab = "Diabetic Pedgiree values", col = 'blue')
boxplot(balanced_dataset$Glucose, main = "Glucose Boxplot", ylab = "Glucose", col = 'blue')
boxplot(balanced_dataset$Age, main = "Age Boxplot", ylab = "Ages", col = 'blue')
boxplot(balanced_dataset$Insulin, main = "Insulin Boxplot", ylab = "insulin serum values", col = 'blue')
boxplot(balanced_dataset$No..of.Pregnancy, main = "Number of Pregnancy Boxplot", ylab = "number of babies", col = 'blue')
# With the exception of Insulin there aren't to many outliers in the data
# so we will just transform the data by bringing outliers to 75 % and 25 % IQR
for (col in names(balanced_dataset)[names(balanced_dataset) != "Outcome"]) {
  x <- balanced_dataset[[col]]
  
  # Only process numeric columns
  if (is.numeric(x)) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    lower <- Q1 - 1.5 * IQR_val
    upper <- Q3 + 1.5 * IQR_val
    
    # Replace outliers
    x[x < lower] <- Q1
    x[x > upper] <- Q3
    
    # Update the column in the data frame
    balanced_dataset[[col]] <- x
  }
}
# Proving that there is perfect seperation of diabetic and nondiabetic people with Glucose, hence why we have to
# remove the feature from future analysis


# Boxplot of Glucose by Outcome
ggplot(balanced_dataset, aes(x = factor(Outcome), y = Glucose, fill = factor(Outcome))) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"),
                    labels = c("Non-Diabetic", "Diabetic")) +
  scale_x_discrete(labels = c("0" = "Non-Diabetic", "1" = "Diabetic")) +
  labs(title = "Glucose Levels: Diabetic vs Non-Diabetic",
       x = "Outcome", y = "Glucose Level") +
  theme_minimal()



# Because Insulin contains so many outliers, we will just remove the feature from our analysis
# Because oral Glucose tests are used in diagnosing diabetes, we will also remove this feature from analysis
balanced_dataset <- subset(balanced_dataset, select = -Insulin)
balanced_dataset <- subset(balanced_dataset, select = -Glucose)
# Our goal is to do Logistic Regression Analysis on the data set
# According to this paper we need to check the following assumptions, https://www.researchgate.net/publication/341354759_Logistic_and_Linear_Regression_Assumptions_Violation_Recognition_and_Control
# 1. Binary Outcomes
# 2 Observation Independence
# 3 Abcscene of Multicollinearity
# 4 Linearity of Indepedent variables and Log Odds
# 5 Appropiate Sample Size
# 6 Independence of Errors

# Binary outcome is satisfied, 1 is diabetic, 0 is nondiabetic
# for observation Independence the experimental design was Retrospective data collection, which
# already potential docks us against meeting this metric. Unfortunately due to a lack or more information on data collection methods
# we cannot prove their is Observation Indepedence

# Abscene of Multicolinearity
# This can be checked with a Pearson's correlation coefficent and a heatmap
cor_matrix <- cor(balanced_dataset[, !names(balanced_dataset) %in% "Outcome"])
cor_matrix_melted <- melt(cor_matrix)
corr_matrix_data <- ggplot(cor_matrix_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 0, low = "blue", high = "red", mid = "white") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Because Age and Number of pregancies is strongly correlated at about 0.76
# we will drop that feature, in addition systolic and diastolic blood pressure
# are moderately correlated at 0.62, therefore we will one of each set of features from 
# the dataset,
balanced_dataset_no_preg <- subset(balanced_dataset, select = -No..of.Pregnancy)
balanced_dataset_no_preg_no_ds <- subset(balanced_dataset, select = -BP.Diastolic.)

# Linearity of Indepedent variables and Log Odds
# This can just be checked with scatterplots displaying indepedent variable and log odds
# This requires us to already have our Logistic Regression model in use, so the code for it will be here
set.seed(123)
Logistic_model <- glm(Outcome ~ ., data = balanced_dataset, family = "binomial")
summary(Logistic_model)

# Add predicted log odds to the dataset
balanced_dataset$log_odds <- predict(Logistic_model, type = "link")

# Get names of all predictor variables (excluding Outcome and log_odds)
predictor_vars <- setdiff(names(balanced_dataset), c("Outcome", "log_odds"))

# Plot each predictor vs log_odds
for (var in predictor_vars) {
  p <- ggplot(balanced_dataset, aes_string(x = var, y = "log_odds")) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_smooth(method = "loess", color = "darkred", se = FALSE) +
    labs(title = paste("Log Odds vs", var),
         x = var,
         y = "Log Odds") +
    theme_minimal()
  print(p)
}
# Checking the resulting plots we see that the following independent variables are the most clearly linear with the log outcomes,
# BMI, Age, and Skin Thickness, and the others are not hence they will be removed before we build our final logistic regression model

refined_data <- subset(balanced_dataset, select = -log_odds)
refined_data <- subset(refined_data, select = -DiabetesPedigreeFunction)
refined_data <- subset(refined_data, select = -BP.Systolic.)
refined_data <- subset(refined_data, select = -No..of.Pregnancy)
refined_data <- subset(refined_data, select = -BP.Diastolic.)
Logistic_model <- glm(Outcome ~ ., data = refined_data, family = "binomial")

### This is our Final Logisitic Regression Model please look at these values
summary(Logistic_model)

# Checking for Appropiate Sample Size
# Using the 10 Events per prediction rule of thumb, our sample size of 93 observation for diabetic
# and 93 observations for non diabetic should be enough to get away with logistic regression here

#Checking for independence of Errors, unfortunately without more knowledge on the experimental design and data collection, which
# is not provided there is no effective way to check for this.

## Chi Square Analysis 
# We want to check to see if there are any differences between the diabetic and nondiabetic group
# in regards to blood pressure are a categorical variable
#chi-squared test of independence
#Four assumptions:
#  1. All observations must be independent.
#  2. Categories must be mutually exclusive.
#  3. Categories must be mutually exhaustive.
#  4. The data are expressed as frequencies/counts, and the fe for each cell must exceed 5 for all categories.
# Making BP categories based on CDC cutoffs
set.seed(123)
non_diabetic <- df %>%
  filter(Outcome == 0)
diabetic_93 <- df %>%
  filter(Outcome == 1) %>%
  sample_n(nrow(non_diabetic))
balanced_dataset_Chi_Square <- bind_rows(non_diabetic, diabetic_93)
balanced_dataset_Chi_Square$BP_Category <- with(balanced_dataset_Chi_Square, ifelse(BP.Systolic. > 180 | BP.Diastolic. > 120, "Hypertensive Crisis",
                                                                                    ifelse(BP.Systolic. >= 140 | BP.Diastolic. >= 90, "Stage 2 Hypertension",
                                                                                           ifelse(BP.Systolic. >= 130 | BP.Diastolic. >= 80, "Stage 1 Hypertension",
                                                                                                  ifelse(BP.Systolic. >= 120 & BP.Diastolic. < 80, "Elevated",
                                                                                                         ifelse(BP.Systolic. < 120 & BP.Diastolic. < 80, "Normal", NA))))))
bp_table <- table(balanced_dataset_Chi_Square$Outcome, balanced_dataset_Chi_Square$BP_Category)
rownames(bp_table) <- c("Non-Diabetic", "Diabetic")
print(bp_table)
chisq.test(bp_table)


## Bonferroni Correction
# We wanted to do a Chi Square test to figure out if blood pressure categories had an influence on whether someone had diabetes
# We couldn't do this with regression since A, siastolic and diastolic BP are correlated meaning one datapoint had to be removed, and B,
# the remainign variable we had Systolic BP had a nonlinear relationship with the log odds of the outcome, and the predictor variable. Because
# this violates an assumption of the logistic regression test, we use Chi Square instead
# Alpha value is now 0.025 since we did two tests. Bonferoni = alpha value / number of statistical tests

# ODDS Ratios from Logistic Regression Model
# ODDS ratios from other variables are no considered their p values are not significant 
# Skin Thickness odds ratio: e^0.012680 = each 1 mm increase in skin thickness increases diabetes risk by about 1 percent

### Boxplots for variables considered for logistic Regression, after their outliers have been transformed
# Boxplot for Skin Thickness
### Boxplots for variables considered for logistic Regression, after their outliers have been transformed

# Boxplot for Skin Thickness
boxplot(refined_data$Skin.Thickness.mm., 
        main = "Skin Thickness Boxplot", 
        ylab = "mm", 
        col = 'blue',
        cex.main = 2,
        cex.lab = 2,
        cex.axis = 2)
mtext("Figure 1: Distribution of Skin Thickness (mm) in our sample population", side = 1, line = 4, cex = 1.5)

# Boxplot for BMI
boxplot(refined_data$BMI, 
        main = "BMI Boxplot", 
        ylab = "BMI's", 
        col = 'blue',
        cex.main = 2,
        cex.lab = 2,
        cex.axis = 2)
mtext("Figure 2: Distribution of BMI in our sample population", side = 1, line = 4, cex = 1.5)

# Boxplot for Age
boxplot(refined_data$Age, 
        main = "AGE Boxplot", 
        ylab = "Ages", 
        col = 'blue',
        cex.main = 2,
        cex.lab = 2,
        cex.axis = 2)
mtext("Figure 3: Distribution of Age in our sample population", side = 1, line = 4, cex = 1.5)


ui <- fluidPage(div(titlePanel("Pabna Diabetes Dataset Analysis"),
                p("Introudction to type 2 Diabetes and dataset, and why we used
                  logisitic regression, and chi square analysis"),
                titlePanel("Initial exploratory Data analysis and processing"),
                p("A previous paper linked here  https://pmc.ncbi.nlm.nih.gov/articles/PMC10834804/, 
                has confirmed that this dataset has no missing data. Unfortunately the dataset is not balanced
                in regards to the number of patients with diabetes, and without. Random subsampling of the larger,
                diabetic group was performed in order to balance the dataset.
                  "),
                div(
                  style = "display:block; margin-top: 20px;",
                  plotOutput("Table_for_og_data")
                )

                ,
                div(
                  style = "display:block; margin-top: 20px;",
                  plotOutput("corr_matrix_data")
                )
                titlePanel("Checking Logistic Regression assumptions"),
                p("The assumptions for logistic regression are, sufficent sample size, independence of observations,
                  binary outcome, and linearity of the log odds ratio and little to no colinearity with the indpedent data"),
                p("We assumed that the data was indepdent, and had an appropiate sample size"),
                titlePanel('Colinearity Check'),
                plotOutput("corrplot"),
                titlePanel("Linearity of the Log Odds for Variables"),
                titlePanel("Checking Chi Square Analysis assumptions"),
                titlePanel("Discussion"),
                titlePanel("Citation and Authors"),
                
                p("The statistical analysis was done by Yonatan Amsalu, Azan Choudy, Ines Fernandez
                  , Boyao yang, and Spencer Slade. The intial dataset was sourced from here, 
                  https://data.mendeley.com/datasets/vxnyysk9vc/2")
                
))
server <- function(input, output, session) {
  output$Table_for_og_data <- renderPlot({ df_2 <- read.csv("Type2_Diabetes_Dataset.csv")
  
  # Plot diabetes status counts
  data_unbalanced <- ggplot(df_2, aes(x = factor(Outcome, levels = c(1, 0), labels = c("diabetic", "nondiabetic")))) +
    geom_bar(fill = "steelblue") +
    labs(x = "Diabetes Status", y = "Count", title = "Diabetes Status Distribution") +
    theme_minimal()
  
  # Linearity of Log Odds
  output$corrplot <- renderPlot({
    corr_matrix_data
    print(corr_matrix_data)
  })
    
    
  })
  
}
shinyApp(ui, server)

