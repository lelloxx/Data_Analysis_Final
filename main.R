library(caret)
library(Metrics)
library(glmnet)
library(mgcv)
library(rpart)
library(e1071)
library(splines)
library(MASS)


#Importing dataset and libraries

a="/Users/alessandroausteri/Documents/GitHub/Data_Analysis_Final/BigMartSales.csv"
l="/Users/lorenzolaterza/Desktop/Data_Analysis_Final/BigMartSales.csv"


df= read.table(a,header=TRUE, sep=",") 
library(dplyr)
library(corrplot)
library(ggplot2)
library(gridExtra)

#-----------------------------------------------------------------------------------------------
#EDA

##Data Cleaning

###Checking and Removing Duplicates

# Check for entire row duplicates
duplicates_entire_row <- duplicated(df)

# Count the number of entirely duplicated rows
num_entire_row_duplicates <- sum(duplicates_entire_row)
print(paste("Number of duplicated rows:", num_entire_row_duplicates))

###Fixing Features of the Dataset

#Unique values of item_fat_content
item_fat_content_uniques <- unique(df$Item_Fat_Content)

print(item_fat_content_uniques)

#Correct the names of `Item_Fat_Content` (change every LF or lowfat into "Low Fat" and alle reg into "Regular Fat")
df$Item_Fat_Content <- ifelse(grepl("^[Rr]", df$Item_Fat_Content), "Regular Fat", "Low Fat")
item_fat_content_uniques <- unique(df$Item_Fat_Content)

print(item_fat_content_uniques)

###Missing Values

# Crea una copia del dataframe convertendo tutte le colonne in stringhe
df_copy <- lapply(df, as.character)

# Creiamo una funzione per contare "NA", "" o "0" in ogni colonna della copia
count_na_empty_or_zero_strings <- function(column) {
  # Sostituiamo i veri NA con stringhe vuote per una conta coerente
  column[is.na(column)] <- ""
  sum(column == "" | column == "0", na.rm = TRUE)
}

# Applicare la funzione a tutte le colonne della copia e stampare i risultati
na_empty_or_zero_counts <- sapply(df_copy, count_na_empty_or_zero_strings)

# Stampa il numero di "NA", "" o "0" per ogni colonna della copia
print(na_empty_or_zero_counts)

#As the only three features which has missing values are Item_Weight, Item_Visibility, Outlet_Size

#Outlet_Size handling of missing Values

#conteggio records senza Outlet_Size:
null<-sum(nchar(df$Outlet_Size) ==0)

#numero di records
total<- nrow(df)

print(paste("Number of rows with missing valuess on Outlet_Size:",null))

#percentuale di righe senza outlet_size
percentuale_stringhe_vuote <- (null / total) * 100

#stampa percentuale di righe senza outlet_size

print(paste("Percentuale di records con stringhe vuote in 'Outlet_Size':", percentuale_stringhe_vuote, "%"))

# Aggiornamento della colonna 'Outlet_Size'
df <- df %>%
  mutate(Outlet_Size = case_when(
    Outlet_Type == "Grocery Store" ~ "Small",
    #Outlet_Type %in% c("Supermarket Type2", "Supermarket Type3") ~ "Medium",
    TRUE ~ Outlet_Size  # Mantiene il valore originale per tutte le altre condizioni
  ))

# Visualizza le modifiche per confermare
# Creazione di una tabella di riepilogo
outlet_summary <- df %>%
  filter(Outlet_Size %in% c("Small", "Medium", "High")) %>%  # Filtra per includere solo le righe con i valori specificati
  group_by(Outlet_Type, Outlet_Size) %>%  # Raggruppa per tipo e dimensione del negozio
  summarise(Count = n(), .groups = 'drop')  # Calcola il conteggio e rimuove il raggruppamento automatico

# Visualizzazione della tabella di riepilogo
print(outlet_summary)

#Discovering which percentage of missing values we have now

#conteggio records senza Outlet_Size:
null<-sum(nchar(df$Outlet_Size) ==0)

#numero di records
total<- nrow(df)

print(paste("Number of rows with missing valuess on Outlet_Size:",null))

#percentuale di righe senza outlet_size
percentuale_stringhe_vuote <- (null / total) * 100

#stampa percentuale di righe senza outlet_size

print(paste("Percentuale di records con stringhe vuote in 'Outlet_Size':", percentuale_stringhe_vuote, "%"))

#As for Supermarket Type1 we don't have any kind of correloations we have to substitute it with NA 

# Aggiornamento della colonna 'Outlet_Size' per riempire le stringhe vuote
df <- df %>%
  mutate(Outlet_Size = if_else(nchar(Outlet_Size) == 0, "NA", Outlet_Size))

#Converting asfactor `Item_Fat_Content` and `Outlet_Size`

# Conversione di 'Outlet_Size' in valori numerici
df <- df %>%
  mutate(Outlet_Size = case_when(
    Outlet_Size == "Small" ~ 1,
    Outlet_Size == "Medium" ~ 2,
    Outlet_Size == "High" ~ 3,
    TRUE ~ NA_real_  # Imposta NA per qualsiasi altro valore non specificato
  ))

df <- df%>%
  mutate(Item_Fat_Content = case_when(
    Item_Fat_Content == "Low Fat" ~ 1,
    Item_Fat_Content == "Regular Fat" ~ 2,
    TRUE ~ NA_real_
  ))
head(df)

###Handling missing values for `Item_Weight`

# Calcolare la media del peso per ogni categoria di prodotto
average_weight_per_type <- aggregate(Item_Weight ~ Item_Type, data = df, mean, na.rm = TRUE)

# Funzione per riempire i pesi mancanti
fill_missing_weights <- function(item_id, item_type) {
  # Controlla se esiste un valore non NA per lo stesso Item_Identifier
  if (any(!is.na(df$Item_Weight[df$Item_Identifier == item_id]))) {
    return(df$Item_Weight[df$Item_Identifier == item_id & !is.na(df$Item_Weight)][1])
  } else {
    # Altrimenti usa la media del Item_Type corrispondente
    return(average_weight_per_type$Item_Weight[average_weight_per_type$Item_Type == item_type])
  }
}

# Applicare la funzione ai valori NA in Item_Weight
df$Item_Weight[is.na(df$Item_Weight)] <- mapply(fill_missing_weights, df$Item_Identifier[is.na(df$Item_Weight)], df$Item_Type[is.na(df$Item_Weight)])

# Visualizzare il dataframe aggiornato
head(df)

###Handling Missing Values for `Item_Visibility`

# Find the number of zero 'Item_Visibility' values for each 'Outlet_Identifier'
zero_visibility_counts <- aggregate(Item_Visibility ~ Outlet_Identifier, data = df, function(x) sum(x == 0))

# Rename the column for better understanding
names(zero_visibility_counts)[2] <- "Zero_Item_Visibility_Count"

# Display the result
print(zero_visibility_counts)

visibility_sum_per_store <- aggregate(Item_Visibility ~ Outlet_Identifier, data = df, sum)
names(visibility_sum_per_store)[2] <- "Total_Item_Visibility"

# Display the result
print(visibility_sum_per_store)

# Filtrare i record con visibilità maggiore di zero per calcolare le medie
filtered_df <- df[df$Item_Visibility > 0, ]

# Calcolare la media di Item_Visibility per ogni combinazione di Item_Type e Outlet_Size
visibility_avg_per_type_size <- aggregate(Item_Visibility ~ Item_Type + Outlet_Size, data = filtered_df, mean)

# Creare una chiave unica per facilitare il merge
visibility_avg_per_type_size$key <- with(visibility_avg_per_type_size, paste(Item_Type, Outlet_Size, sep = "_"))
df$key <- with(df, paste(Item_Type, Outlet_Size, sep = "_"))

# Merge tra i record del dataframe originale e le medie calcolate usando un left join
df <- merge(df, visibility_avg_per_type_size, by = "key", all.x = TRUE, suffixes = c("", ".new"))

# Sostituire i valori zero di Item_Visibility con i valori medi calcolati
df$Item_Visibility[df$Item_Visibility == 0] <- df$Item_Visibility.new[df$Item_Visibility == 0]

# Rimuovere le colonne in più create dal merge
df <- df[, !names(df) %in% c("Item_Visibility.new", "key")]

# Visualizzare il dataframe aggiornato
head(df)

#dropping useless columns we used to handle missing values
df <- df %>%
  select(-Item_Type.new, -Outlet_Size.new)

# Histogram and Box Plot for Item_Visibility
p3 <- ggplot(df, aes(x = Item_Visibility)) + 
  geom_histogram(binwidth = 0.01, fill = 'green', alpha = 0.7) + 
  ggtitle("Distribution of Item Visibility") +
  xlab("Item Visibility") +
  ylab("Frequency")

p4 <- ggplot(df, aes(x = "", y = Item_Visibility)) + 
  geom_boxplot(fill = 'green', alpha = 0.7) + 
  ggtitle("Box Plot of Item Visibility") +
  xlab("") +
  ylab("Item Visibility")
print(p3)
print(p4)


#count Visibility for each store
visibility_sum_per_store <- aggregate(Item_Visibility ~ Outlet_Identifier, data = df, sum)
names(visibility_sum_per_store)[2] <- "Total_Item_Visibility"

# Display the result
print(visibility_sum_per_store)



#finding out all the uniques values for `Item_Fat_Content`


item_fat_content_uniques <- unique(df$Item_Type) #unique values of Item_Fat_Conten

print(item_fat_content_uniques)


### Reducing the numbers of `Item_Category` by grouping all the similar products
df <- df %>%
  mutate(Item_Type_Reduced = case_when(
    Item_Type %in% c("Baking Goods", "Breads", "Breakfast", "Dairy", "Meat", "Seafood") ~ "Food Basics",
    Item_Type %in% c("Canned", "Frozen Foods", "Snack Foods", "Starchy Foods") ~ "Processed Foods",
    Item_Type %in% c("Hard Drinks", "Soft Drinks") ~ "Beverages",
    Item_Type %in% c("Health and Hygiene", "Household") ~ "Non-Food Items",
    Item_Type == "Fruits and Vegetables" ~ "Produce",
    Item_Type == "Others" ~ "Others",
    TRUE ~ "Others"  # Catch-all for any undefined categories
  ))

#rename the column item_type_reduced into item_type

#drop old item_Type column

df <- df %>%
  select(-Item_Type)


# Renaming the column using dplyr
df <- df %>%
  rename(Item_Type = Item_Type_Reduced)

#-----------------------------------------------------------------------------------------------

##Multivariate Analysis

#Correlation Matrix

# Calculate Spearman's rank correlation matrix again if not already calculated
cor_matrix <- cor(df %>% select(where(is.numeric)), 
                  method = "spearman", 
                  use = "pairwise.complete.obs")

# Visualize the correlation matrix with coefficients inside the circles
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, tl.cex = 0.8, cl.cex = 0.8,
         col = colorRampPalette(c("#6BAED6", "#FFFFFF", "#FD8D3C"))(200),
         addCoef.col = "black",  # Sets color of the coefficients to black (choose based on your color scheme)
         number.cex = 0.6)  # Adjust coefficient text size appropriately


#Investigatig the realtionship between Item MRP and Item_Outlet_Sales
# Load necessary libraries
library(ggplot2)

# Specific price points to add to the plot
specific_price_points <- c(67.99, 134.49, 199.99)

# Plot the relationship between Item_MRP and Item_Outlet_Sales
p <- ggplot(df, aes(x = Item_MRP, y = Item_Outlet_Sales)) +
  geom_point(alpha = 0.5) +
  labs(title = "Relationship between Item MRP and Outlet Sales",
       x = "Item MRP", y = "Outlet Sales")

# Add vertical lines for specific price points
for (price in specific_price_points) {
  p <- p + geom_vline(xintercept = price, linetype = "dotted", color = "blue", alpha = 0.7)
  p <- p + annotate("text", x = price, y = max(df$Item_Outlet_Sales) * 0.9, label = price, color = "blue", angle = 90, vjust = -0.5, size = 3)
}

# Print the plot
print(p)

#Investigating on relationship between Item MRP and Outlet Sales by Product Type

library(dplyr)

df_summary <- df %>%
  group_by(Item_Type, MRP_Bracket = cut(Item_MRP, breaks = seq(0, max(Item_MRP), by = 20))) %>%
  summarize(Average_Sales = mean(Item_Outlet_Sales), .groups = 'drop')

ggplot(data = df_summary, aes(x = MRP_Bracket, y = Average_Sales, fill = Item_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Average Outlet Sales by MRP Bracket and Product Type",
       x = "MRP Bracket",
       y = "Average Outlet Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# item outlet sales by outlet size
ggplot(df, aes(x = Outlet_Size, y = Item_Outlet_Sales)) +
  geom_boxplot(aes(fill = Outlet_Size)) +
  labs(title = "Item Outlet Sales by Outlet Size",
       x = "Outlet Size", y = "Outlet Sales")

# Sales trends over the years
ggplot(df, aes(x = Outlet_Establishment_Year, y = Item_Outlet_Sales)) +
  geom_point(aes(color = Outlet_Establishment_Year)) +
  geom_smooth(method = "lm") +
  labs(title = "Sales Trends Over the Years",
       x = "Establishment Year", y = "Outlet Sales")

#Item_Outlet_Sales density plot
# Create a density plot
ggplot(data = df, aes(x = Item_Outlet_Sales)) +
  geom_density(fill = "turquoise", alpha = 0.5) +  # 'alpha' controls transparency
  labs(title = "Density Plot of Item Outlet Sales",
       x = "Item Outlet Sales",
       y = "Density") +
  theme_light()

#-----------------------------------------------------------------------------------------------

#Advanced visualization Visibility-Sales
# Ensure 'Item_Visibility' and 'Outlet_Sales' are treated correctly
df$Item_Visibility <- as.numeric(as.character(df$Item_Visibility))
df$Item_Outlet_Sales <- as.numeric(as.character(df$Item_Outlet_Sales))

# Convert categorical variables to factors
df$Item_Type <- as.factor(df$Item_Type)
df$Outlet_Type <- as.factor(df$Outlet_Type)

# Segmented Regression by Item Type
item_type_models <- df %>%
  group_by(Item_Type) %>%
  do(model = lm(Item_Outlet_Sales ~ Item_Visibility, data = .))

# Viewing summaries for each Item Type
item_type_summaries <- lapply(item_type_models$model, summary)

# Print the summaries for review
print(item_type_summaries)

# Creating scatter plots segmented by Item_Type
p_item_type <- ggplot(df, aes(x = Item_Visibility, y = Item_Outlet_Sales)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  facet_wrap(~ Item_Type) +
  labs(title = "Item Visibility vs Outlet Sales by Item Type",
       x = "Item Visibility", y = "Outlet Sales") +
  theme_minimal()

# Creating scatter plots segmented by Outlet_Type
p_outlet_type <- ggplot(df, aes(x = Item_Visibility, y = Item_Outlet_Sales)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  facet_wrap(~ Outlet_Type) +
  labs(title = "Item Visibility vs Outlet Sales by Outlet Type",
       x = "Item Visibility", y = "Outlet Sales") +
  theme_minimal()

# Print the plots
print(p_item_type)
print(p_outlet_type)


#Linear model to investigate on Item Visibility and Sales

# Fit a linear model
model <- lm(Item_Outlet_Sales ~ Item_Visibility + Item_Type + Outlet_Type + Item_Fat_Content + Item_MRP, data = df)

# Summary of the model to understand influences
summary(model)


#Distribution of Variables across Item Sales

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Convert categorical variables to factor if not already
df$Item_Type <- as.factor(df$Item_Type)
df$Outlet_Type <- as.factor(df$Outlet_Type)
df$Item_Fat_Content <- as.factor(df$Item_Fat_Content)
df$Outlet_Size <- as.factor(df$Outlet_Size)
df$Outlet_Location_Type <- as.factor(df$Outlet_Location_Type)

# Continuous Variables
continuous_vars <- c("Item_Weight", "Item_Visibility", "Item_MRP")

# Categorical Variables
categorical_vars <- c("Item_Type", "Outlet_Type", "Item_Fat_Content", "Outlet_Size", "Outlet_Location_Type")

# Plotting Distribution of Sales for Continuous Variables
for(var in continuous_vars) {
  p <- ggplot(df, aes_string(x = var, y = "Item_Outlet_Sales")) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", color = "blue") +
    labs(title = paste("Scatter Plot of", var, "vs Outlet Sales"),
         x = var, y = "Outlet Sales") +
    theme_minimal()
  print(p)
}

# Plotting Distribution of Sales for Categorical Variables
for(var in categorical_vars) {
  p <- df %>%
    group_by_(.dots = var) %>%
    summarise(Average_Sales = mean(Item_Outlet_Sales)) %>%
    ggplot(aes_string(x = var, y = "Average_Sales", fill = var)) +
    geom_bar(stat = "identity", color = "black") +
    labs(title = paste("Average Sales by", var),
         x = var, y = "Average Sales") +
    theme_minimal()
  print(p)
}

#item visibility vs item mrp scatterplot

# Load ggplot2

# Create a scatter plot
ggplot(df, aes(x = Item_MRP, y = Item_Visibility)) +
  geom_point(alpha = 0.5) +  # Use semi-transparent points to handle overplotting
  labs(x = "Item Maximum Retail Price (MRP)",
       y = "Item Visibility",
       title = "Relationship between Item MRP and Item Visibility") +
  theme_minimal()  # Clean minimalistic theme

# item visibility vs item mrp line graph

# Create bins for Item MRP using a reasonable interval
df$MRP_Bin <- cut(df$Item_MRP, breaks=seq(from=min(df$Item_MRP), to=max(df$Item_MRP), by=20), include.lowest=TRUE, right=TRUE)

# Calculate average visibility per MRP bin
average_visibility_per_mrp <- aggregate(Item_Visibility ~ MRP_Bin, data = df, mean)

# Create a line graph
ggplot(average_visibility_per_mrp, aes(x = MRP_Bin, y = Item_Visibility, group=1)) +
  geom_line() +  # Adds a line graph
  geom_point() +  # Adds points to each average point
  labs(x = "Item MRP Range", y = "Average Item Visibility",
       title = "Average Item Visibility Across Different MRP Ranges") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Improve x-axis label readability

#Segmented Analysis Regression 

# Segmented Regression by Outlet Type
outlet_type_models <- df %>%
  group_by(Outlet_Type) %>%
  do(model = lm(Item_Outlet_Sales ~ Item_Visibility, data = .))

# Viewing summaries for each Outlet Type
outlet_type_summaries <- lapply(outlet_type_models$model, summary)

# Print the summaries for review
print(outlet_type_summaries)

# Optional: Plotting the regression lines for each type on a scatter plot
# Plotting for Item Type
ggplot(df, aes(x = Item_Visibility, y = Item_Outlet_Sales, color = Item_Type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Item_Type) +
  labs(title = "Item Visibility vs Outlet Sales by Item Type", x = "Item Visibility", y = "Outlet Sales")

# Plotting for Outlet Type
ggplot(df, aes(x = Item_Visibility, y = Item_Outlet_Sales, color = Outlet_Type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Outlet_Type) +
  labs(title = "Item Visibility vs Outlet Sales by Outlet Type", x = "Item Visibility", y = "Outlet Sales")


###Checking for Outliers

# Histogram and Box Plot for Item_Weight
p1 <- ggplot(df, aes(x = Item_Weight)) + 
  geom_histogram(binwidth = 1, fill = 'blue', alpha = 0.7) + 
  ggtitle("Distribution of Item Weight") +
  xlab("Item Weight") +
  ylab("Frequency")

p2 <- ggplot(df, aes(x = "", y = Item_Weight)) + 
  geom_boxplot(fill = 'blue', alpha = 0.7) + 
  ggtitle("Box Plot of Item Weight") +
  xlab("") +
  ylab("Item Weight")

# Histogram and Box Plot for Item_Visibility
p3 <- ggplot(df, aes(x = Item_Visibility)) + 
  geom_histogram(binwidth = 0.01, fill = 'green', alpha = 0.7) + 
  ggtitle("Distribution of Item Visibility") +
  xlab("Item Visibility") +
  ylab("Frequency")

p4 <- ggplot(df, aes(x = "", y = Item_Visibility)) + 
  geom_boxplot(fill = 'green', alpha = 0.7) + 
  ggtitle("Box Plot of Item Visibility") +
  xlab("") +
  ylab("Item Visibility")

# Histogram and Box Plot for Item_MRP
p5 <- ggplot(df, aes(x = Item_MRP)) + 
  geom_histogram(binwidth = 5, fill = 'red', alpha = 0.7) + 
  ggtitle("Distribution of Item MRP") +
  xlab("Item MRP") +
  ylab("Frequency")

p6 <- ggplot(df, aes(x = "", y = Item_MRP)) + 
  geom_boxplot(fill = 'red', alpha = 0.7) + 
  ggtitle("Box Plot of Item MRP") +
  xlab("") +
  ylab("Item MRP")

# Histogram and Box Plot for Item_Outlet_Sales
p7 <- ggplot(df, aes(x = Item_Outlet_Sales)) + 
  geom_histogram(binwidth = 100, fill = 'purple', alpha = 0.7) + 
  ggtitle("Distribution of Item Outlet Sales") +
  xlab("Item Outlet Sales") +
  ylab("Frequency")

p8 <- ggplot(df, aes(x = "", y = Item_Outlet_Sales)) + 
  geom_boxplot(fill = 'purple', alpha = 0.7) + 
  ggtitle("Box Plot of Item Outlet Sales") +
  xlab("") +
  ylab("Item Outlet Sales")

# Print the plots individually
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)
print(p7)
print(p8)


#Item type vs Item_Sales}

# Esegui ANOVA
avg_sales_by_type <- df %>%
  group_by(Item_Type) %>%
  summarise(Average_Sales = mean(Item_Outlet_Sales, na.rm = TRUE))

# Creating a bar graph of average sales by item type
ggplot(avg_sales_by_type, aes(x = Item_Type, y = Average_Sales, fill = Item_Type)) +
  geom_bar(stat = "identity", width = 0.7) +  # Using identity to use the heights of the bars to represent values in the data
  labs(title = "Average Sales by Item Type", x = "Item Type", y = "Average Sales") +
  theme_minimal() +  # Clean minimalistic theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x labels for better visibility
        legend.title = element_blank()) +  # Remove legend title if not needed
  scale_fill_brewer(palette = "Paired")  # Optional: Use a color palette that is visually appealing

# Lower Dimensional Models

#Impact of item MRP ad Outlet Type on Sales 2 
model_mrp_outlet <- lm(Item_Outlet_Sales ~ Item_MRP * Outlet_Type, data = df)
summary(model_mrp_outlet)

# Plotting this relationship
ggplot(df, aes(x = Item_MRP, y = Item_Outlet_Sales, color = Outlet_Type)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Outlet_Type) +
  labs(title = "Impact of MRP and Outlet Type on Sales")


# Impact of item MRP ad Outlet Type on Sales 
# Adjusting the model to focus only on Item Visibility
model_visibility <- lm(Item_Outlet_Sales ~ Item_Visibility, data = df)
summary(model_visibility)
# Plotting the relationship between Item Visibility and Sales
ggplot(df, aes(x = Item_Visibility, y = Item_Outlet_Sales)) +
  geom_point(alpha = 0.3) +  # Point opacity reduced for better visualization of density
  geom_smooth(method = "lm", se = FALSE) +  # Linear model smoothing without confidence intervals
  labs(title = "Impact of Item Visibility on Sales",  # Updated title
       x = "Item Visibility",  # X-axis label
       y = "Sales")  # Y-axis label

#-----------------------------------------------------------------------------------------------

##Feature Engineering

df <- df %>%
  mutate(
    # Convert Establishment Year to Age
    Outlet_Age = as.numeric(format(Sys.Date(), "%Y")) - Outlet_Establishment_Year,
    
    # Interaction between MRP and Outlet Type
    MRP_x_OutletType = Item_MRP * as.numeric(as.factor(Outlet_Type)),
    
    
    # Simplifying Item Type
    Item_Type_Simplified = case_when(
      grepl("Foods", Item_Type) ~ "Foods",
      grepl("Drinks", Item_Type) ~ "Drinks",
      TRUE ~ "Non-Consumables"
    )
  )

# Ensure that categorical variables are in the correct format
df$Outlet_Type <- as.factor(df$Outlet_Type)
df$Item_Fat_Content <- as.factor(df$Item_Fat_Content)
df$Item_Type_Simplified <- as.factor(df$Item_Type_Simplified)
df$Outlet_Identifier <- as.factor(df$Outlet_Identifier)

df <- df %>%
  select(-c(Outlet_Age, MRP_x_OutletType))

# Changing `Outlet_Establishment_Year` into `Outlet Age`

# Add a new column 'Outlet_Age' to the dataframe
df <- df %>%
  mutate(Outlet_Age = as.numeric(format(Sys.Date(), "%Y")) - Outlet_Establishment_Year)

# Display the age of each store
# This creates a summary table with Outlet_ID and its corresponding Outlet_Age
store_ages <- df %>%
  select(Outlet_Identifier, Outlet_Age) %>%  # Select the necessary columns
  distinct() %>%  # Remove duplicate rows to ensure each store is listed once
  arrange(Outlet_Identifier)  # Optional: Sort by Outlet Identifier for easier reading

# Print the resulting table to see each store's age
#print(store_ages)

# Aggregate total sales by Outlet_ID
sales_summary <- df %>%
  group_by(Outlet_Identifier) %>%
  summarise(Total_Sales = sum(Item_Outlet_Sales)) %>%
  ungroup()
# Merge sales summary with store ages
sales_with_age <- merge(sales_summary, store_ages, by = "Outlet_Identifier")


# Plot total sales by Outlet_ID with Outlet_Age in the x-axis labels
ggplot(sales_with_age, aes(x = Outlet_Identifier, y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total Sales by Outlet ID with Age Labels",
       x = "Outlet ID (Age)",
       y = "Total Sales") +
  scale_x_discrete(labels = paste(sales_with_age$Outlet_Identifier, "\n(Age:", sales_with_age$Outlet_Age, "yrs)")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))  # Adjust text to make it readable

####counting number of sales for store perche mi sembra strano

# Sum the total sales for each outlet, grouped by Outlet Identifier and include Outlet Age
sales_summary <- df %>%
  group_by(Outlet_Identifier, Outlet_Age) %>%
  summarise(Total_Sales = sum(Item_Outlet_Sales), .groups = 'drop') %>%
  arrange(desc(Total_Sales))

# Print the summary table with total sales and outlet age
print(sales_summary)

#-----------------------------------------------------------------------------------------------

##One hot econding

# Identificazione delle variabili categoriche per l'encoding
categorical_vars <- c("Outlet_Type", "Item_Type_Simplified")

# Applicazione del one-hot encoding
dummies <- dummyVars(~ Outlet_Type + Item_Type_Simplified, data = df, fullRank = FALSE)
combined_data_ohe <- predict(dummies, newdata = df)

# Conversione in dataframe
combined_data_ohe <- as.data.frame(combined_data_ohe)

# Rimozione delle variabili categoriche originali e la colonna "Item_Type"
df <- df %>%
  select(-one_of(categorical_vars), -Item_Type) %>%
  bind_cols(combined_data_ohe)

#------------------------------------------------------------------------------------------------

# Splitting dataset

# Impostazione del seed per la riproducibilità
set.seed(123)

# Divisione del dataset in set di training e test
index <- createDataPartition(df$Item_Outlet_Sales, p = 0.8, list = FALSE)
train_data <- df[index, ]
test_data <- df[-index, ]

#-----------------------------------------------------------------------------------------------

# Model on the train set
# Carico le librerie necessarie, se non già caricato
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# Assumo che il tuo dataframe si chiami train_data

# Modello per Item_MRP
train_data_MRP <- na.omit(train_data[, c("Item_MRP", "Item_Outlet_Sales")])  # Omette NA solo nelle colonne specificate
model_Item_MRP <- lm(Item_Outlet_Sales ~ Item_MRP, data = train_data_MRP)

# Modello per Outlet_Size
train_data_Size <- na.omit(train_data[, c("Outlet_Size", "Item_Outlet_Sales")])
model_Outlet_Size <- lm(Item_Outlet_Sales ~ Outlet_Size, data = train_data_Size)

# Modello per Item_Visibility
train_data_Visibility <- na.omit(train_data[, c("Item_Visibility", "Item_Outlet_Sales")])
model_Item_Visibility <- lm(Item_Outlet_Sales ~ Item_Visibility, data = train_data_Visibility)

# Modello per Outlet_Age
train_data_Age <- na.omit(train_data[, c("Outlet_Age", "Item_Outlet_Sales")])
model_Outlet_Age <- lm(Item_Outlet_Sales ~ Outlet_Age, data = train_data_Age)

# Stampa i riepiloghi dei modelli
cat("\nRiepilogo del modello per Item MRP:\n")
print(summary(model_Item_MRP))

cat("\nRiepilogo del modello per Outlet Size:\n")
print(summary(model_Outlet_Size))

cat("\nRiepilogo del modello per Item Visibility:\n")
print(summary(model_Item_Visibility))

cat("\nRiepilogo del modello per Outlet Age:\n")
print(summary(model_Outlet_Age))


## Model on the test set}
# Assumo che il tuo dataframe si chiami test_data

# Modello per Item_MRP sul test set
test_data_MRP <- na.omit(test_data[, c("Item_MRP", "Item_Outlet_Sales")])
predictions_MRP <- predict(model_Item_MRP, newdata = test_data_MRP)

# Modello per Outlet_Size sul test set
test_data_Size <- na.omit(test_data[, c("Outlet_Size", "Item_Outlet_Sales")])
predictions_Size <- predict(model_Outlet_Size, newdata = test_data_Size)

# Modello per Item_Visibility sul test set
test_data_Visibility <- na.omit(test_data[, c("Item_Visibility", "Item_Outlet_Sales")])
predictions_Visibility <- predict(model_Item_Visibility, newdata = test_data_Visibility)

# Modello per Outlet_Age sul test set
test_data_Age <- na.omit(test_data[, c("Outlet_Age", "Item_Outlet_Sales")])
predictions_Age <- predict(model_Outlet_Age, newdata = test_data_Age)

# Calcolo delle metriche di performance (RMSE e R-squared) per ogni modello
library(Metrics)

# RMSE
rmse_MRP <- rmse(test_data_MRP$Item_Outlet_Sales, predictions_MRP)
rmse_Size <- rmse(test_data_Size$Item_Outlet_Sales, predictions_Size)
rmse_Visibility <- rmse(test_data_Visibility$Item_Outlet_Sales, predictions_Visibility)
rmse_Age <- rmse(test_data_Age$Item_Outlet_Sales, predictions_Age)

# R-squared
r2_MRP <- summary(lm(Item_Outlet_Sales ~ predictions_MRP, data = test_data_MRP))$r.squared
r2_Size <- summary(lm(Item_Outlet_Sales ~ predictions_Size, data = test_data_Size))$r.squared
r2_Visibility <- summary(lm(Item_Outlet_Sales ~ predictions_Visibility, data = test_data_Visibility))$r.squared
r2_Age <- summary(lm(Item_Outlet_Sales ~ predictions_Age, data = test_data_Age))$r.squared

# Stampa dei risultati
print(data.frame(
  Variable = c("Item_MRP", "Outlet_Size", "Item_Visibility", "Outlet_Age"),
  RMSE = c(rmse_MRP, rmse_Size, rmse_Visibility, rmse_Age),
  R_Squared = c(r2_MRP, r2_Size, r2_Visibility, r2_Age)
))

#-----------------------------------------------------------------------------------------------

## Stepwise Regression
if (!require(caret)) install.packages("caret")
if (!require(MASS)) install.packages("MASS")
library(caret)
library(MASS)

# Identificazione e rimozione delle variabili categoriche
train_data <- train_data[, sapply(train_data, is.numeric)]
test_data <- test_data[, sapply(test_data, is.numeric)]
test_data <- na.omit(test_data)  # Rimuove tutte le righe con NA
train_data <- na.omit(train_data)
# Modello iniziale con le variabili numeriche rimanenti
fit_initial <- lm(Item_Outlet_Sales ~ ., data = train_data)

# Selezione delle variabili usando il metodo backward
step_model <- stepAIC(fit_initial, direction = "backward", trace = FALSE)

# Stampa del modello finale dopo la selezione backward
print(summary(step_model))

# Previsione sul test set utilizzando il modello selezionato
predictions <- predict(step_model, newdata = test_data)

# Calcolo delle metriche di performance sul test set
actuals <- test_data$Item_Outlet_Sales
rmse_value <- sqrt(mean((predictions - actuals)^2))
r_squared <- summary(lm(actuals ~ predictions))$r.squared

# Stampa delle metriche di performance
cat("Performance Metrics on Test Set:\n")
cat("RMSE:", rmse_value, "\n")
cat("R-squared:", r_squared, "\n")

# Puoi anche desiderare di vedere come il modello si comporta sul set di training per confronto
train_predictions <- predict(step_model, newdata = train_data)
train_actuals <- train_data$Item_Outlet_Sales
train_rmse <- sqrt(mean((train_predictions - train_actuals)^2))
train_r_squared <- summary(lm(train_actuals ~ train_predictions))$r.squared

# Stampa delle metriche di performance sul train set
cat("Performance Metrics on Train Set:\n")
cat("RMSE:", train_rmse, "\n")
cat("R-squared:", train_r_squared, "\n")

#Extracting Stepwise Variables
# Extracting the names of the variables used in the final stepwise model
stepwise_variables <- names(coef(step_model))
stepwise_variables <- c(stepwise_variables, "")
stepwise_variables <- stepwise_variables[-c(1, length(stepwise_variables))]

# Print the list of variables used in the stepwise regression model
print(stepwise_variables)

#-----------------------------------------------------------------------------------------------

## Ridge Regression

if (!require(glmnet)) install.packages("glmnet")
library(glmnet)
# Preparazione dei dati
x_train <- model.matrix(Item_Outlet_Sales ~ . - 1, data = train_data)
y_train <- train_data$Item_Outlet_Sales

x_test <- model.matrix(Item_Outlet_Sales ~ . - 1, data = test_data)
y_test <- test_data$Item_Outlet_Sales
# Modello Ridge
set.seed(123)
ridge_model <- cv.glmnet(x_train, y_train, alpha = 0, lambda = 10^seq(10, -2, length = 100))
best_lambda_ridge <- ridge_model$lambda.min

# Previsione sul test set
ridge_predictions <- predict(ridge_model, s = best_lambda_ridge, newx = x_test)

# Calcolo delle metriche di performance
ridge_rmse <- sqrt(mean((ridge_predictions - y_test)^2))
ridge_r_squared <- summary(lm(y_test ~ ridge_predictions))$r.squared

cat("Ridge Regression Performance Metrics:\n")
cat("RMSE:", ridge_rmse, "\n")
cat("R-squared:", ridge_r_squared, "\n")

# Ridge using only the variables selected from stepwise
if (!require(glmnet)) install.packages("glmnet")
library(glmnet)

formula <- as.formula(paste("Item_Outlet_Sales ~", paste(stepwise_variables, collapse=" + ")))

# Creating the model matrices
x_train <- model.matrix(formula, data=train_data)
y_train <- train_data$Item_Outlet_Sales

x_test <- model.matrix(formula, data=test_data)
y_test <- test_data$Item_Outlet_Sales

# Ridge Regression Model
set.seed(123)
ridge_model <- cv.glmnet(x_train, y_train, alpha = 0, lambda = 10^seq(10, -2, length = 100))
best_lambda_ridge <- ridge_model$lambda.min

# Prediction on the test set
ridge_predictions <- predict(ridge_model, s = best_lambda_ridge, newx = x_test)

# Calculate performance metrics
ridge_rmse <- sqrt(mean((ridge_predictions - y_test)^2))
ridge_r_squared <- summary(lm(y_test ~ ridge_predictions))$r.squared

cat("Ridge Regression Performance Metrics:\n")
cat("RMSE:", ridge_rmse, "\n")
cat("R-squared:", ridge_r_squared, "\n")


## Lasso Regression
# Modello Lasso
set.seed(123)
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1, lambda = 10^seq(10, -2, length = 100))
best_lambda_lasso <- lasso_model$lambda.min

# Previsione sul test set
lasso_predictions <- predict(lasso_model, s = best_lambda_lasso, newx = x_test)

# Calcolo delle metriche di performance
lasso_rmse <- sqrt(mean((lasso_predictions - y_test)^2))
lasso_r_squared <- summary(lm(y_test ~ lasso_predictions))$r.squared

cat("Lasso Regression Performance Metrics:\n")
cat("RMSE:", lasso_rmse, "\n")
cat("R-squared:", lasso_r_squared, "\n")

#lasso with only the selected variables by stepwise reg}
if (!require(glmnet)) install.packages("glmnet")
library(glmnet)
formula <- as.formula(paste("Item_Outlet_Sales ~", paste(stepwise_variables, collapse=" + "), "- 1"))

# Creating the model matrices
x_train <- model.matrix(formula, data=train_data)
y_train <- train_data$Item_Outlet_Sales

x_test <- model.matrix(formula, data=test_data)
y_test <- test_data$Item_Outlet_Sales

# Lasso Regression Model
set.seed(123)
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1, lambda = 10^seq(10, -2, length = 100))
best_lambda_lasso <- lasso_model$lambda.min

# Prediction on the test set
lasso_predictions <- predict(lasso_model, s = best_lambda_lasso, newx = x_test)

# Calculate performance metrics
lasso_rmse <- sqrt(mean((lasso_predictions - y_test)^2))
lasso_r_squared <- summary(lm(y_test ~ lasso_predictions))$r.squared

cat("Lasso Regression Performance Metrics:\n")
cat("RMSE:", lasso_rmse, "\n")
cat("R-squared:", lasso_r_squared, "\n")

##Elastic Net

set.seed(123)
elastic_net_model <- cv.glmnet(x_train, y_train, alpha = 0.5, lambda = 10^seq(10, -2, length = 100))
best_lambda_elastic_net <- elastic_net_model$lambda.min

# Previsione sul test set
elastic_net_predictions <- predict(elastic_net_model, s = best_lambda_elastic_net, newx = x_test)

# Calcolo delle metriche di performance
elastic_net_rmse <- sqrt(mean((elastic_net_predictions - y_test)^2))
elastic_net_r_squared <- summary(lm(y_test ~ elastic_net_predictions))$r.squared

cat("Elastic Net Performance Metrics:\n")
cat("RMSE:", elastic_net_rmse, "\n")
cat("R-squared:", elastic_net_r_squared, "\n")

#elastic net using only the variables selected by stepwise
if (!require(glmnet)) install.packages("glmnet")
library(glmnet)

# Using the 'stepwise_variables' list to define the formula for the model matrix
formula <- as.formula(paste("Item_Outlet_Sales ~", paste(stepwise_variables, collapse=" + "), "- 1"))

# Creating the model matrices
x_train <- model.matrix(formula, data=train_data)
y_train <- train_data$Item_Outlet_Sales

x_test <- model.matrix(formula, data=test_data)
y_test <- test_data$Item_Outlet_Sales

# Elastic Net Model
set.seed(123)
elastic_net_model <- cv.glmnet(x_train, y_train, alpha = 0.5, lambda = 10^seq(10, -2, length = 100))
best_lambda_elastic_net <- elastic_net_model$lambda.min

# Prediction on the test set
elastic_net_predictions <- predict(elastic_net_model, s = best_lambda_elastic_net, newx = x_test)

# Calculate performance metrics
elastic_net_rmse <- sqrt(mean((elastic_net_predictions - y_test)^2))
elastic_net_r_squared <- summary(lm(y_test ~ elastic_net_predictions))$r.squared

cat("Elastic Net Performance Metrics:\n")
cat("RMSE:", elastic_net_rmse, "\n")
cat("R-squared:", elastic_net_r_squared, "\n")

#ensembling / stacking
set.seed(123)
x_train <- model.matrix(Item_Outlet_Sales ~ . - 1, data = train_data)
y_train <- train_data$Item_Outlet_Sales

x_test <- model.matrix(Item_Outlet_Sales ~ . - 1, data = test_data)
y_test <- test_data$Item_Outlet_Sales
# Modello Ridge
ridge_model <- cv.glmnet(x_train, y_train, alpha = 0, lambda = 10^seq(10, -2, length = 100))
ridge_predictions_train <- predict(ridge_model, s = ridge_model$lambda.min, newx = x_train)
ridge_predictions_test <- predict(ridge_model, s = ridge_model$lambda.min, newx = x_test)

# Modello Lasso
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1, lambda = 10^seq(10, -2, length = 100))
lasso_predictions_train <- predict(lasso_model, s = lasso_model$lambda.min, newx = x_train)
lasso_predictions_test <- predict(lasso_model, s = lasso_model$lambda.min, newx = x_test)

# Modello Elastic Net
elastic_net_model <- cv.glmnet(x_train, y_train, alpha = 0.5, lambda = 10^seq(10, -2, length = 100))
elastic_net_predictions_train <- predict(elastic_net_model, s = elastic_net_model$lambda.min, newx = x_train)
elastic_net_predictions_test <- predict(elastic_net_model, s = elastic_net_model$lambda.min, newx = x_test)
# Combinare le previsioni dei modelli base per il set di allenamento
stacked_predictions_train <- data.frame(
  Ridge = ridge_predictions_train,
  Lasso = lasso_predictions_train,
  ElasticNet = elastic_net_predictions_train
)

# Combinare le previsioni dei modelli base per il set di test
stacked_predictions_test <- data.frame(
  Ridge = ridge_predictions_test,
  Lasso = lasso_predictions_test,
  ElasticNet = elastic_net_predictions_test
)
# Addestrare il meta-learner (regressione lineare)
meta_learner <- lm(y_train ~ ., data = stacked_predictions_train)

# Previsioni del meta-learner sul set di test
meta_predictions_test <- predict(meta_learner, newdata = stacked_predictions_test)

# Calcolo delle metriche di performance
meta_rmse <- sqrt(mean((meta_predictions_test - y_test)^2))
meta_r_squared <- summary(lm(y_test ~ meta_predictions_test))$r.squared

cat("Stacking Performance Metrics:\n")
cat("RMSE:", meta_rmse, "\n")
cat("R-squared:", meta_r_squared, "\n")

## feature engineering

# Creazione della variabile Price_Per_Unit_Weight
df$Price_Per_Unit_Weight <- ifelse(is.na(df$Item_Weight), 0, df$Item_MRP / df$Item_Weight)


# Creazione della variabile logaritmica per Item_Visibility
df$Log_Item_Visibility <- log(df$Item_Visibility + 1)

# Verifica delle prime righe del dataset per controllare le nuove variabili
head(df)
sum(is.na(df$Item_MRP))
sum(is.na(df$Outlet_Size))
df <- na.omit(df, cols = "Outlet_Size")
df <- df %>%
  filter_all(all_vars(!(is.character(.) & . == "")))
df$Outlet_Size <- as.numeric((df$Outlet_Size))

df$MRP_x_Outlet_Size <- df$Item_MRP * df$Outlet_Size

#stepwise regression
if (!require(caret)) install.packages("caret")
if (!require(MASS)) install.packages("MASS")
library(caret)
library(MASS)

# Identificazione e rimozione delle variabili categoriche
train_data <- train_data[, sapply(train_data, is.numeric)]
test_data <- test_data[, sapply(test_data, is.numeric)]
test_data <- na.omit(test_data)  # Rimuove tutte le righe con NA
train_data <- na.omit(train_data)
# Modello iniziale con le variabili numeriche rimanenti
fit_initial <- lm(Item_Outlet_Sales ~ ., data = train_data)

# Selezione delle variabili usando il metodo backward
step_model <- stepAIC(fit_initial, direction = "backward", trace = FALSE)

# Stampa del modello finale dopo la selezione backward
print(summary(step_model))

# Previsione sul test set utilizzando il modello selezionato
predictions <- predict(step_model, newdata = test_data)

# Calcolo delle metriche di performance sul test set
actuals <- test_data$Item_Outlet_Sales
rmse_value <- sqrt(mean((predictions - actuals)^2))
r_squared <- summary(lm(actuals ~ predictions))$r.squared

# Stampa delle metriche di performance
cat("Performance Metrics on Test Set:\n")
cat("RMSE:", rmse_value, "\n")
cat("R-squared:", r_squared, "\n")

# Puoi anche desiderare di vedere come il modello si comporta sul set di training per confronto
train_predictions <- predict(step_model, newdata = train_data)
train_actuals <- train_data$Item_Outlet_Sales
train_rmse <- sqrt(mean((train_predictions - train_actuals)^2))
train_r_squared <- summary(lm(train_actuals ~ train_predictions))$r.squared

# Stampa delle metriche di performance sul train set
cat("Performance Metrics on Train Set:\n")
cat("RMSE:", train_rmse, "\n")
cat("R-squared:", train_r_squared, "\n")

#-----------------------------------------------------------------------------------------------

## Random forest tree using only selected variables
# Install and load the randomForest package
if (!require(randomForest)) {
  install.packages("randomForest", dependencies = TRUE)
}
library(randomForest)

# Assuming 'train_data' and 'test_data' are your datasets
# Make sure all categorical variables are converted to factors if they are not already

# Convert factors (Uncomment and adjust as necessary)
# train_data$Outlet_Type <- as.factor(train_data$Outlet_Type)
# test_data$Outlet_Type <- as.factor(test_data$Outlet_Type)
# Additional categorical variables should be handled similarly.

# Prepare data for Random Forest
train_features <- train_data[, names(train_data) != "Item_Outlet_Sales"]
train_labels <- train_data$Item_Outlet_Sales

test_features <- test_data[, names(test_data) != "Item_Outlet_Sales"]
test_labels <- test_data$Item_Outlet_Sales

# Train the Random Forest model
random_forest_model <- randomForest(x = train_features, y = train_labels, ntree = 200, mtry = 7, importance = TRUE)#mtry da 3 a 18, cambia tree

# Predictions on the test set
rf_predictions <- predict(random_forest_model, newdata = test_features)

# Calculate RMSE and R-squared
rf_rmse <- sqrt(mean((rf_predictions - test_labels)^2))
rf_r_squared <- summary(lm(test_labels ~ rf_predictions))$r.squared

# Print performance metrics
cat("Random Forest Performance Metrics on Test Set:\n")
cat("RMSE:", rf_rmse, "\n")
cat("R-squared:", rf_r_squared, "\n")

# Optionally, print variable importance
print(importance(random_forest_model), type = 1)  # type = 1 for mean decrease in accuracy


# random forest tree mtry and node size number detection}
# Install and load necessary packages
# Install and load necessary packages
if (!require("caret")) install.packages("caret", dependencies = TRUE)
if (!require("ranger")) install.packages("ranger")
library(caret)
library(ranger)

# Load your data
df <- na.omit(df)

# Prepare train and test sets
set.seed(123)  # For reproducibility
index <- createDataPartition(df$Item_Outlet_Sales, p = 0.8, list = FALSE)
train_data <- df[index, ]
test_data <- df[-index, ]

# Define training control
train_control <- trainControl(
  method = "cv",          # Use cross-validation
  number = 10,            # Number of folds in cross-validation
  savePredictions = "final",
  verboseIter = TRUE
)

# Define the tuning grid
grid <- expand.grid(
  mtry = c(2,18, by=1),                  # Number of variables randomly sampled as candidates at each split
  splitrule = c("variance"),          # Use variance as the split rule for regression
  min.node.size = c(5, 350, by=20)            # Minimum size of terminal nodes
)

# Train the model using Random Forest via 'ranger' for regression
model <- train(
  Item_Outlet_Sales ~ .,             # Regression formula
  data = train_data,                 # Training data
  method = "ranger",                 # Use the 'ranger' package
  trControl = train_control,         # Training control
  tuneGrid = grid,                   # Grid of hyperparameters
  metric = "RMSE",                   # Optimization metric for regression
  maximize = FALSE                   # Minimize the metric (since it's RMSE)
)

# View the best tuning parameters
print(model$bestTune)

# View the results of all tuning iterations
print(model$results)

#-----------------------------------------------------------------------------------------------

# Clustering

## Determining optimal number of clusters using the elbow method

if (!require("stats")) install.packages("stats")
library(stats)

#scaliamo la variabile item_MRP

df$Item_MRP <- scale(df$Item_MRP)

# Perform k-means clustering to determine the optimal number of clusters using the Elbow method
set.seed(123)
wcss <- sapply(1:10, function(k) {
  kmeans(df[, c("Item_MRP", "Outlet_Type.Grocery Store", "Outlet_Type.Supermarket Type1",
                "Outlet_Type.Supermarket Type2", "Outlet_Type.Supermarket Type3")], centers = k, nstart = 25)$tot.withinss
})

# Plotting the Elbow Curve
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
elbow_plot <- data.frame(k = 1:20, wcss = wcss)
ggplot(elbow_plot, aes(x = k, y = wcss)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow Method for Determining Optimal k",
       x = "Number of Clusters k",
       y = "Total Within-Cluster Sum of Squares (WCSS)") +
  theme_minimal()

# Silhouette analysis

# K-means con k = 9, riconfermto come ottimale
optimal_k <- 2
final_kmeans_result <- kmeans(df[, c("Item_MRP", "Outlet_Type.Grocery Store", "Outlet_Type.Supermarket Type1",
                                     "Outlet_Type.Supermarket Type2", "Outlet_Type.Supermarket Type3")],
                              centers = optimal_k, nstart = 50)

# Visualizzazione dei cluster finali
library(ggplot2)
df$Cluster <- final_kmeans_result$cluster
ggplot(df, aes(x = factor(Cluster), y = Item_MRP, fill = factor(Cluster))) +
  geom_boxplot() +
  labs(title = "Distribution of Item MRP Across Clusters",
       x = "Cluster",
       y = "Item MRP") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

# k-means clustering}
# Assuming the elbow point is at k = 9 based on the plot
optimal_k <- 2
kmeans_result <- kmeans(df[, c("Item_MRP", "Outlet_Type.Grocery Store", "Outlet_Type.Supermarket Type1",
                               "Outlet_Type.Supermarket Type2", "Outlet_Type.Supermarket Type3")], centers = optimal_k, nstart = 25)

# Add cluster labels to the original data
df$Cluster <- kmeans_result$cluster

# Summary by cluster
summary <- aggregate(cbind(Item_MRP) ~ Cluster, data = df, FUN = mean)
print(summary)

# plotting the cluster
# Plotting the clusters
ggplot(df, aes(x = Cluster, y = Item_MRP, color = factor(Cluster))) +
  geom_jitter(alpha = 0.5) +
  facet_wrap(~ Cluster) +
  labs(title = "Cluster of Item MRP across Outlet Types",
       x = "Cluster",
       y = "Scaled Item MRP",
       color = "Cluster") +
  theme_minimal()

# item outlet sales cluster mrp
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

# 1. Statistiche Descrittive per Item_Outlet_Sales per Cluster
sales_stats <- aggregate(Item_Outlet_Sales ~ Cluster, data = df, FUN = function(x) c(mean = mean(x), sd = sd(x), median = median(x), IQR = IQR(x)))
print(sales_stats)

# 2. Test di Significatività tra i due Cluster
# Controlla prima che entrambi i cluster abbiano più di un elemento
if (any(table(df$cluster) > 1)) {
  t_test_results <- t.test(Item_Outlet_Sales ~ cluster, data = df)
  print(t_test_results)
}

# 3. Visualizzazione con Box Plot
ggplot(df, aes(x = factor(Cluster), y = Item_Outlet_Sales, fill = factor(Cluster))) +
  geom_boxplot() +
  labs(title = "Distribution of Item Outlet Sales Across Clusters",
       x = "Cluster",
       y = "Item Outlet Sales",
       fill = "Cluster") +
  theme_minimal()

# outlet type vs cluster mrp 
# Rinomina le colonne rimuovendo gli spazi
names(df) <- gsub(" ", "", names(df))

# Verifica i nomi delle colonne aggiornati
print(names(df))
# Ricrea la variabile Outlet_Type dal one-hot encoding
df$Outlet_Type <- ifelse(df$Outlet_Type.GroceryStore == 1, "Grocery Store",
                         ifelse(df$Outlet_Type.SupermarketType1 == 1, "Supermarket Type1",
                                ifelse(df$Outlet_Type.SupermarketType2 == 1, "Supermarket Type2",
                                       ifelse(df$Outlet_Type.SupermarketType3 == 1, "Supermarket Type3", NA))))

# Controlla che la nuova variabile sia stata creata correttamente
table(df$Outlet_Type)

# Crea una tabella di contingenza tra i cluster di prezzo e i tipi di outlet
outlet_price_cluster_table <- table(df$Outlet_Type, df$Cluster)
print(outlet_price_cluster_table)

# Visualizzazione con barplot
barplot(outlet_price_cluster_table, beside = TRUE, legend = TRUE, 
        col = c("red", "blue"), 
        main = "Distribution of Outlet Types Across Price Clusters",
        ylab = "Count",
        xlab = "Outlet Type",
        args.legend = list(title = "Cluster", x = "topright"))

# Per una visualizzazione più dettagliata potresti utilizzare ggplot2
library(ggplot2)
df_long <- reshape2::melt(outlet_price_cluster_table)
ggplot(df_long, aes(x = Var1, y = value, fill = Var2)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Outlet Type", y = "Count", fill = "Cluster") +
  ggtitle("Distribution of Outlet Types Across Price Clusters") +
  theme_minimal()

# cluster on item fat content
# Crea sottoinsiemi per Regular e Low Fat considerando la codifica numerica
df_regular <- df[df$Item_Fat == 2, ]  # Regular Fat
df_low <- df[df$Item_Fat == 1, ]      # Low Fat

# Statistiche per prodotti Regular e Low Fat in ciascun cluster
regular_stats <- aggregate(Item_MRP ~ Cluster, data = df_regular, FUN = mean)
low_fat_stats <- aggregate(Item_MRP ~ Cluster, data = df_low, FUN = mean)

print(regular_stats)
print(low_fat_stats)

# Test t per confrontare i prezzi tra Regular Fat (2) e Low Fat (1) in ciascun cluster
t_test_cluster1 <- t.test(Item_MRP ~ Item_Fat_Content, data = df[df$Cluster == '1', ], subset = Item_Fat_Content %in% c(1, 2))
t_test_cluster2 <- t.test(Item_MRP ~ Item_Fat_Content, data = df[df$Cluster == '2', ], subset = Item_Fat_Content %in% c(1, 2))

print(t_test_cluster1)
print(t_test_cluster2)

# Visualizzazione con ggplot2
library(ggplot2)
ggplot(df, aes(x = factor(Item_Fat_Content), y = Item_MRP, fill = factor(Item_Fat_Content))) +
  geom_boxplot() +
  facet_wrap(~Cluster) +
  scale_fill_manual(values = c("blue", "red"), labels = c("Low Fat", "Regular Fat")) +
  labs(title = "Distribution of Item MRP by Item Fat Content Across Clusters",
       x = "Item Fat Content",
       y = "Item Maximum Retail Price (MRP)",
       fill = "Item Fat") +
  theme_minimal()


