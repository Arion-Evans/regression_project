# load libraries
library(data.table)
library(corrplot)
library(ggplot2)
library(glmulti)

# load data
data = setDT(read.csv("Life Expectancy Data.csv"))


# replace/remove NAs
data = data[!is.na(Life.expectancy)]
cols = names(data)[!grepl("Country|Status", names(data))]
data[, (cols) := lapply(.SD, function(x) {ifelse(is.na(x), -1, x)}), .SDcols = cols]


fix_plot_text = theme(axis.title = element_text(size = 16),
                      axis.text = element_text(size = 14),
                      plot.title = element_text(size = 16))

summary(data)
head(data)


# EDA ---------------------------------------------------------------------

# target variable
ggplot(data, aes(x = Life.expectancy)) +
  geom_histogram(fill = "aquamarine4", colour = "white") +
  geom_vline(xintercept = quantile(data$Life.expectancy, 0.5), linetype = "dashed", colour = "red", size = 1) +
  annotate("text", x = quantile(data$Life.expectancy, 0.5) - 3, y = 400, label = "Median", colour = "red") +
  theme_minimal() +
  labs(x = "Life Expectancy", y = "Count", title = "Life expectancy distribution")

# correlation
corr = cor(data[, c(2,4:22)], use = "complete.obs")
corrplot(corr, type = "lower")


# status
ggplot(data[, length(unique(Country)), Status], aes(x = Status, y = V1)) +
  geom_bar(stat = "identity", fill = "aquamarine4") +
  theme_minimal() +
  labs(y = "Countries", title = "Distribution of countries by status")
  

# status vs expectancy
ggplot(data, aes(x = Status, y = Life.expectancy)) + 
  geom_boxplot( fill = "aquamarine4") +
  theme_minimal() +
  fix_plot_text +
  labs(y = "Life Expectancy", title = "Life Expectancy by Country Status")


# status & adult mortality
ggplot(data, aes(x = Adult.Mortality, y = Life.expectancy, colour = Status)) + 
  geom_point()+
  theme_minimal() +
  fix_plot_text +
  labs(y = "Life Expectancy", title = "Life Expectancy by Adult Mortality & Country Status",
       x = "Adult Mortality")

# timeseries by status
ggplot(data[, mean(Life.expectancy, na.rm = T), .(Year, Status)], aes(x = Year, y = V1, colour = Status, group = Status)) + 
  geom_point()+
  geom_line() +
  theme_minimal() +
  fix_plot_text +
  labs(y = "Life Expectancy", title = "Life Expectancy Timeseries by Country Status",
       x = "Year")



# models ------------------------------------------------------------------

mod1 = lm(data = data[, -c("Country")], Life.expectancy ~.)
summary(mod1)



# model selection ---------------------------------------------------------


basemodel = lm(data = data[, -c("Country")], formula = Life.expectancy ~ Alcohol)
fullmodel = lm(data = data[, -c("Country")], formula = Life.expectancy ~ .)

forward = step(object = basemodel, direction = "forward")
summary(forward)

backward = step(fullmodel, direction = "backward")
summary(backward)

stepwise = step(fullmodel, direction = "both")
summary(stepwise)



test = glmulti(y = Life.expectancy ~ ., 
        data = data, 
        fitfunction = "glm", 
        level = 1, 
        method = "g", 
        marginality = TRUE,
        crit = "bic", 
        conseq = 2,
        family = gaussian())








