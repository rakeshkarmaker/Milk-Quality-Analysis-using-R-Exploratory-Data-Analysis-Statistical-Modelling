# ------------------------------
# Install & Load Required Packages
# ------------------------------
packages <- c("dplyr", "ggplot2", "reshape2", "corrplot", 
              "gridExtra", "fmsb", "GGally")
install.packages(packages)
lapply(packages, library, character.only = TRUE)

# ------------------------------
# Load Dataset
# ------------------------------
milk_data <- read.csv("milknew.csv",
                      header = TRUE, sep = ",")

# ====================================================
#                   PART A
# ====================================================

milk_data$Grade <- factor(milk_data$Grade, levels = c("low", "medium", "high"))
milk_data$Grade_numeric <- as.numeric(milk_data$Grade)

numeric_cols <- c("pH", "Temprature", "Taste", "Odor", "Fat", "Turbidity", "Colour")
correlations <- sapply(numeric_cols, function(col) cor(milk_data$Grade_numeric, milk_data[[col]], use = "complete.obs"))
cor_df <- data.frame(Correlation = correlations)
print(cor_df)

# Histograms & Boxplots
hist_colour <- ggplot(milk_data, aes(x = Colour)) +
  geom_histogram(binwidth = 1, fill = 'purple', color = 'black') +
  ggtitle('Histogram of Colour') + xlab('Colour') + ylab('Frequency')

box_colour <- ggplot(milk_data, aes(y = Colour)) +
  geom_boxplot(fill = 'cyan') + ggtitle('Boxplot of Colour') + ylab('Colour')

hist_ph <- ggplot(milk_data, aes(x = pH)) +
  geom_histogram(binwidth = 0.5, fill = 'blue', color = 'black') +
  ggtitle('Histogram of pH') + xlab('pH') + ylab('Frequency')

box_ph <- ggplot(milk_data, aes(y = pH)) +
  geom_boxplot(fill = 'yellow') + ggtitle('Boxplot of pH') + ylab('pH')

hist_temp <- ggplot(milk_data, aes(x = Temprature)) +
  geom_histogram(binwidth = 1, fill = 'green', color = 'black') +
  ggtitle('Histogram of Temperature') + xlab('Temperature') + ylab('Frequency')

box_temp <- ggplot(milk_data, aes(y = Temprature)) +
  geom_boxplot(fill = 'orange') + ggtitle('Boxplot of Temperature') + ylab('Temperature')

grid.arrange(hist_colour, box_colour, hist_ph, box_ph, hist_temp, box_temp, nrow = 3)

# Chi-Squared Test
categorical_vars <- c("Colour", "Taste", "Odor")
chi_squared_results <- lapply(categorical_vars, function(col) {
  test_result <- chisq.test(table(milk_data[[col]], milk_data$Grade), simulate.p.value = TRUE, B = 1000)
  return(data.frame(Variable = col, p_value = test_result$p.value))
})
chi_squared_results_df <- do.call(rbind, chi_squared_results)
print(chi_squared_results_df)

ggplot(chi_squared_results_df, aes(x = reorder(Variable, p_value), y = p_value)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  coord_flip() + labs(title = "Chi-Squared Test p-values (Monte Carlo)", x = "Variables", y = "p-value") +
  theme_minimal()

# Correlation Heatmap & Scatterplot Matrix
numeric_columns <- c("pH", "Temprature", "Taste", "Odor", "Fat", "Turbidity", "Grade")
milk_numeric <- milk_data[, numeric_columns]
pearson_corr <- cor(milk_numeric, method = "pearson")
heatmap(pearson_corr, main = "Pearson Correlation Heatmap", col = rainbow(20), scale = "none", margins = c(5,5))
corrplot(pearson_corr, method = "color", type = "lower", addCoef.col = "black", 
         tl.col = "black", tl.srt = 45, title = "Pearson Correlation Plot", mar = c(0, 0, 1, 0))
pairs(milk_numeric, main = "Scatterplot Matrix")

# Violin Plots
ggplot(milk_data, aes(x=Grade, y=pH, fill=Grade)) + geom_violin(trim=FALSE) + ggtitle('Violin Plot of pH by Grade') + theme_minimal()
ggplot(milk_data, aes(x=Grade, y=Temprature, fill=Grade)) + geom_violin(trim=FALSE) + ggtitle('Violin Plot of Temperature by Grade') + theme_minimal()

# Radar Chart
radar_data <- milk_data %>% select(pH, Temprature, Fat, Grade)
radar_data <- as.data.frame(lapply(radar_data[,1:3], scale))
radar_data <- rbind(rep(1, 3), rep(-1, 3), radar_data)
radarchart(radar_data, axistype = 1, pcol = rainbow(3), pfcol = rainbow(3, alpha = 0.5), plwd = 2, title = "Radar Chart of Milk Attributes")

# Line Graphs
ggplot(milk_data, aes(x=pH, y=Grade, group=Grade, color=Grade)) + geom_line() + geom_point() + labs(title="Line Graph of pH vs Grade") + theme_minimal()
ggplot(milk_data, aes(x=Temprature, y=Grade, group=Grade, color=Grade)) + geom_line() + geom_point() + labs(title="Line Graph of Temperature vs Grade") + theme_minimal()

# ====================================================
#                   PART B
# ====================================================

str(milk_data)
colSums(is.na(milk_data))

# Histograms with Density
ggplot(milk_data, aes(x = pH)) +
  geom_histogram(aes(y = ..density..), fill = "pink", color = "white", bins = 10) +
  geom_density(color = "black", size = 2, bw = 0.5) +
  labs(title = "Histogram of pH", x = "pH")

ggplot(milk_data, aes(x = Temprature)) +
  geom_histogram(aes(y = ..density..), fill = "cornflowerblue", color = "white", bins = 10) +
  geom_density(color = "black", size = 2, bw = 5) +
  labs(title = "Histogram of Temprature", x = "Temprature")

ggplot(milk_data, aes(x = Colour)) +
  geom_histogram(aes(y = ..density..), fill = "lightblue", color = "black", bins = 30, alpha = 0.5) +
  geom_density(color = "black", size = 2) +
  labs(title = "Distribution of Colour", x = "Colour", y = "Density")

# Bar Plot
ggplot(milk_data, aes(x = Grade, fill = Grade)) + geom_bar(color = "white") + labs(title = "Bar Plot of Milk Grade", x = "Grade", y = "Frequency")

# Scatter Plots with Regression
ggplot(milk_data, aes(x = pH, y = Temprature)) + geom_point(aes(color = pH), size = 3) +
  geom_smooth(method = lm, color = "black", fill = "cornflowerblue", se = TRUE) +
  scale_color_gradient(low = "red", high = "darkblue") + labs(title = "Scatter Plot of pH vs Temperature", x = "pH", y = "Temperature")

ggplot(milk_data, aes(x = pH, y = Colour)) + geom_point(aes(color = pH), size = 3) +
  geom_smooth(method = lm, color = "black", fill = "cornflowerblue", se = TRUE) +
  scale_color_gradient(low = "lightblue", high = "darkblue") + labs(title = "Scatter Plot of pH vs Colour", x = "pH", y = "Colour")

ggplot(milk_data, aes(x = Colour, y = Temprature)) + geom_point(aes(color = Colour), size = 3) +
  geom_smooth(method = lm, color = "black", se = TRUE) +
  scale_color_gradient(low = "lightgreen", high = "darkgreen") + labs(title = "Scatter Plot of Colour vs Temprature", x = "Colour", y = "Temprature")

# Violin + Boxplot Overlay
ggplot(milk_data, aes(x=Grade, y=pH, fill=Grade)) + geom_violin(trim=FALSE) + geom_boxplot(width = .03, fill = "orange", outlier.color = "black", outlier.size = 4) + ggtitle('Violin Plot of pH by Grade') + ylim(6, 8) + theme_minimal()
ggplot(milk_data, aes(x=Grade, y=Temprature, fill=Grade)) + geom_violin(trim=FALSE) + geom_boxplot(width = .03, fill = "orange", outlier.color = "black", outlier.size = 4) + ggtitle('Violin Plot of Temperature by Grade') + theme_minimal()
ggplot(milk_data, aes(x=Grade, y=Colour, fill=Grade)) + geom_violin(trim=FALSE) + geom_boxplot(width = .03, fill = "orange", outlier.color = "black", outlier.size = 4) + ggtitle('Violin Plot of Colour by Grade') + theme_minimal()

# Radar Chart with Comparisons
max_min <- data.frame(pH = c(8, 0), Temperature = c(100, 0), Colour = c(10, 0), Odor = c(1, 0), Taste = c(1, 0))
data_values <- rbind(max_min,  
                     Comparison1 = as.numeric(milk_data[1, c("pH", "Temprature", "Taste", "Odor", "Fat")]),
                     Comparison2 = as.numeric(milk_data[2, c("pH", "Temprature", "Taste", "Odor", "Fat")]),
                     Comparison3 = as.numeric(milk_data[3, c("pH", "Temprature", "Taste", "Odor", "Fat")])
)
colors_border <- c("red", "blue", "green")
colors_in <- c(rgb(1, 0, 0, 0.3), rgb(0, 0, 1, 0.3), rgb(0, 1, 0, 0.3))
radarchart(data_values, axistype = 1, pcol = colors_border, pfcol = colors_in, plwd = 2, plty = 1,
           cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0, 8, 2), cglwd = 0.8, vlcex = 0.8)
legend(x = "topright", legend = c("Test subject 1", "Test subject 2", "Test subject 3"), bty = "n", pch = 20, col = colors_border, text.col = "black", cex = 1.2, pt.cex = 3)

# Line Graphs by Grade
milk_data$Grade <- as.factor(milk_data$Grade)
ggplot(milk_data, aes(x = pH, y = Temprature, color = Grade)) + geom_line(aes(group = Grade), size = 1) + geom_point(size = 3) + labs(title = "Line Graph of pH and Temprature by Milk Grade", x = "pH", y = "Temprature")
ggplot(milk_data, aes(x = Temprature, y = Colour, color = Grade)) + geom_line(aes(group = Grade), size = 1) + geom_point(size = 3) + labs(title = "Line Graph of Temprature and Colour by Milk Grade", x = "Temprature", y = "Colour")
ggplot(milk_data, aes(x = Colour, y = pH, color = Grade)) + geom_line(aes(group = Grade), size = 1) + geom_point(size = 3) + labs(title = "Line Graph of pH and Colour by Milk Grade", x = "pH", y = "Colour")

