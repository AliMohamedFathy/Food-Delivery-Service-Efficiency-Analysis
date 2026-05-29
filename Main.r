#===================================Hexa Team present...

#all libraries for the project
#download it once in your pc,labtop..etc
packages <- c(
  "tidyverse", "VIM", "factoextra", "arules", 
  "gridExtra", "GGally", "corrplot", "ggridges", 
  "patchwork", "arulesViz", "shiny", "bslib", 
  "plotly", "DT", "shinydashboard"
)
install.packages(packages, dependencies = TRUE)

#Data_Cleaning
library("tidyverse") 
library("VIM")

#Algorithms
library("factoextra")
library("arules")

#Data_Visualization
library(gridExtra)
library(GGally)
library(corrplot)
library(ggridges)
library(gridExtra)
library(patchwork)
library(arulesViz)

#GUI
library(shiny)
library(bslib)        
library(plotly)        
library(DT)       
library(shinydashboard)


#---------------------------------------Data_Cleaning-----------------------------------------


#read the Data and import it 

Data <- read.csv("D:/flashcard/ali/New folder/Final_Project/the_Data.csv")
Data_Cleaned <- Data

#Task_One : Inspect the dataset structure and data types.

str(Data_Cleaned)
summary(Data_Cleaned)

# convert this column to integer

Data_Cleaned$Courier_Experience_yrs <- as.integer(Data_Cleaned$Courier_Experience_yrs)

# to make sure that this columns have a specific value to know convert it to factor data type or no
unique(Data_Cleaned$Weather)
unique(Data_Cleaned$Traffic_Level)
unique(Data_Cleaned$Time_of_Day)
unique(Data_Cleaned$Vehicle_Type)

# we conclude that we have a specific values 
# and have spaces in cells in column number 3,4,5

# so we will clean the spaces by

Data_Cleaned <- Data_Cleaned %>% 
  filter( Weather != "" & 
            Traffic_Level != "" & 
            Time_of_Day != "" )

# and convert into factor data type (by specific levels)

# FIX: convert the actual column data instead of creating a new 3/4/5-element vector
Data_Cleaned$Traffic_Level <- factor(
  Data_Cleaned$Traffic_Level,
  levels = c("Low", "Medium", "High"),  
  ordered = TRUE
)

Data_Cleaned$Time_of_Day <- factor(
  Data_Cleaned$Time_of_Day,
  levels = c("Night", "Afternoon", "Morning", "Evening"),  
  ordered = TRUE
)

Data_Cleaned$Vehicle_Type <- factor(
  Data_Cleaned$Vehicle_Type,
  levels = c("Bike", "Scooter", "Car"),  
  ordered = TRUE
)

Data_Cleaned$Weather <- factor(
  Data_Cleaned$Weather,
  levels = c("Clear", "Windy", "Foggy", "Rainy", "Snowy"),  
  ordered = TRUE
)

str(Data_Cleaned)


#if we have duolicated values 

Data_Cleaned <- unique(Data_Cleaned)

# find if we have outlier and delete it

boxplot(Data_Cleaned[,2:9])$out

outlier <- boxplot(Data_Cleaned$Delivery_Time_min)$out
Data_Cleaned <- Data_Cleaned[- which(Data_Cleaned$Delivery_Time_min %in% outlier),]
outlier <- boxplot(Data_Cleaned$Delivery_Time_min)$out
# FIX: only remove if outliers actually exist to avoid error on empty which()
if (length(outlier) > 0) {
  Data_Cleaned <- Data_Cleaned[- which(Data_Cleaned$Delivery_Time_min %in% outlier),]
}

#arrange DataFrame ID

Data_Cleaned <- Data_Cleaned %>%
  arrange(Order_ID)

#To shift ID's 

for( i in 1 : length(Data_Cleaned$Order_ID)){
  Data_Cleaned[i,1] <- i
}

# new column " Speed "

Data_Cleaned <- Data_Cleaned %>%
  mutate(Speed_KPH = ( Data_Cleaned$Distance_km / Data_Cleaned$Delivery_Time_min ) * 60)

# find outliers in speed and clean it 

outlier_speed <- boxplot(Data_Cleaned$Speed_KPH)$out
# FIX: only remove if outliers actually exist
if (length(outlier_speed) > 0) {
  Data_Cleaned <- Data_Cleaned[- which(Data_Cleaned$Speed_KPH %in% outlier_speed),]
}

#Handle the NA

my_features <- c("Weather", "Traffic_Level", "Time_of_Day", "Vehicle_Type","Speed_KPH")
New_Data_Cleaned <- Data_Cleaned

for ( y in 3:6){
  New_Data_Cleaned[,y] <- as.numeric(New_Data_Cleaned[,y])
}

Finished_Data_Cleaned <- kNN(
  
  data = New_Data_Cleaned,
  variable = "Courier_Experience_yrs",
  dist_var = my_features,
  k = 5,
  numFun = median,
  imp_var = FALSE,
)

sum(is.na(Finished_Data_Cleaned))
str(Finished_Data_Cleaned)

# calculate Expected_Late_Delivery,and build Late_Delivery_flag column 

Total_Order_Time <- Finished_Data_Cleaned$Delivery_Time_min + Finished_Data_Cleaned$Preparation_Time_min
Expected_Time = median(Total_Order_Time)


Finished_Data_Cleaned <- Finished_Data_Cleaned %>%
  mutate(
    Total_Order_Time_min = Total_Order_Time,
    Late_Delivery =  Total_Order_Time > Expected_Time
  )

# Calculate customer rating Column

max_speed <- max(Finished_Data_Cleaned$Speed_KPH)
min_speed <- min(Finished_Data_Cleaned$Speed_KPH)

Level_of_Speed <- (max_speed-min_speed)/5  

Finished_Data_Cleaned$Customer_Rating <- NA

for ( x in 1:5 ){
  
  lower_bound <- min_speed + (x - 1) * Level_of_Speed
  upper_bound <- min_speed + x * Level_of_Speed
  
  if (x == 5) {
    Rate_col_fit <- Finished_Data_Cleaned$Speed_KPH >= lower_bound &
      Finished_Data_Cleaned$Speed_KPH <= upper_bound
  } else {
    Rate_col_fit <- Finished_Data_Cleaned$Speed_KPH >= lower_bound &
      Finished_Data_Cleaned$Speed_KPH < upper_bound
  }
  Finished_Data_Cleaned$Customer_Rating[Rate_col_fit] <- x
  
}
#shift


for( i in 1 : length(Finished_Data_Cleaned$Order_ID)){
  Finished_Data_Cleaned[i,1] <- i
}


#Final Check 

summary(Finished_Data_Cleaned)
str(Finished_Data_Cleaned)
View(Finished_Data_Cleaned)


#----------------------------------K-means Clustering Algorithm-------------------------------


#K_Means

Clusters_Data = Finished_Data_Cleaned[,-c(1,12)]

summary(Clusters_Data)

#scaling our data

Scaled_Data = scale(Clusters_Data)
summary(Scaled_Data)

#Selecting best cluster numbers (3or 4)

fviz_nbclust(Scaled_Data , kmeans,method ="wss") #3 clusters is better

#clustring

kdata3 = kmeans(Scaled_Data , centers = 3 , nstart =50)
kdata3

#grouping the main data

Clusters_Data$Clusters = kdata3$cluster

Cluster_Sammary = Clusters_Data %>% 
  group_by(Clusters) %>% 
  summarise(
    Avg_DTime = mean( Delivery_Time_min),
    Avg_Distance = mean(Distance_km),
    Avg_speed = mean(Speed_KPH),
    Avg_weather = mean(Weather),
    Avg_Traffic = mean(Traffic_Level), #ot benefit us
    Avg_Time = mean(Time_of_Day),
    Avg_Vehicle = mean(Vehicle_Type),
    Avg_Rating = mean(Customer_Rating),
    Avg_prep = mean (Preparation_Time_min),
    Avg_Experience = mean (Courier_Experience_yrs),
  )

print(Cluster_Sammary)     #best one is cluster 3 then 2 then 1

#Visualize the groups

PCA_Data <- prcomp(Scaled_Data ,scale= FALSE , center = FALSE)
groups <- as.factor(Clusters_Data$Clusters)
fviz_pca_ind(PCA_Data,
             col.ind = groups,
             # FIX: correct argument name is "palette" not "palate"
             palette = c("green","orange","cyan"),
             addEllipses = TRUE,
             legend.title = "Groups",
             geom = "point"
)


#to understand the direction of every cluster

fviz_pca_var(
  PCA_Data,
  col.var = "contrib",
  # FIX: correct argument name is "gradient.cols" not "gradients.col"
  gradient.cols = c("blue","yellow","red"),
  repel=TRUE
)
kdata3$cluster


#---------------------------- Association Rule " Apriori Algorithm"----------------------------


# 3) Inspecting Dataset Structure

str(Finished_Data_Cleaned)

# 1.5) Restore original factor types for categorical variables
# The columns 3 to 6 were converted to numeric for kNN imputation.
# We must convert them back to factor with their original levels for Association Rules.

# Weather (Col 3) - Original levels: "Clear", "Windy", "Foggy","Rainy","Snowy" (5 levels)
Finished_Data_Cleaned$Weather <- factor(
  Finished_Data_Cleaned$Weather,
  levels = 1:5,
  labels = c("Clear", "Windy", "Foggy","Rainy","Snowy"),
  ordered = TRUE
)

# Traffic_Level (Col 4) - Original levels: "Low", "Medium", "High" (3 levels)
Finished_Data_Cleaned$Traffic_Level <- factor(
  Finished_Data_Cleaned$Traffic_Level,
  levels = 1:3,
  labels = c("Low", "Medium", "High"),
  ordered = TRUE
)

# Time_of_Day (Col 5) - Original levels: "Night", "Afternoon", "Morning","Evening" (4 levels)
Finished_Data_Cleaned$Time_of_Day <- factor(
  Finished_Data_Cleaned$Time_of_Day,
  levels = 1:4,
  labels = c("Night", "Afternoon", "Morning","Evening"),
  ordered = TRUE
)

# Vehicle_Type (Col 6) - Original levels: "Bike", "Scooter", "Car" (3 levels)
Finished_Data_Cleaned$Vehicle_Type <- factor(
  Finished_Data_Cleaned$Vehicle_Type,
  levels = 1:3,
  labels = c("Bike", "Scooter", "Car"),
  ordered = TRUE
)

# Re-check structure after restoration
str(Finished_Data_Cleaned)

# 4) Converting Numerical Variables into Categories

Finished_Data_Cleaned$Distance_Category <- cut(
  Finished_Data_Cleaned$Distance_km,
  breaks = c(0 , 5, 15, 20),
  labels = c("Short", "Medium", "Long")
)

Finished_Data_Cleaned$Prep_Category <- cut(
  Finished_Data_Cleaned$Preparation_Time_min,
  breaks = c(0 , 15, 25, 30),
  labels = c("Low", "Medium", "High")
)

Finished_Data_Cleaned$Exp_Category <- cut(
  Finished_Data_Cleaned$Courier_Experience_yrs,
  breaks = c(0, 3, 7, 10),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE
)

Finished_Data_Cleaned$Delivery_Category <- cut(
  Finished_Data_Cleaned$Delivery_Time_min,
  breaks = c(0, 30, 60, 115),
  labels = c("Fast", "Medium", "Long")
)

Finished_Data_Cleaned$Speed_Category <- cut(
  Finished_Data_Cleaned$Speed_KPH,
  breaks = c(0, 5, 15, 25),
  labels = c("Low", "Medium", "High")
)

Finished_Data_Cleaned$Order_Time_Category <- cut (
  Finished_Data_Cleaned$Total_Order_Time_min,
  breaks = c(0, 50, 100, 140),
  labels = c("Low", "Medium", "High")
  
)


# 5) Converting Boolean to Factor

Finished_Data_Cleaned$Late_Delivery <- as.factor(Finished_Data_Cleaned$Late_Delivery)

# 6) Selecting Only Categorical Columns For Apriori

data_cat <- Finished_Data_Cleaned[, c(
  "Weather", "Traffic_Level", "Time_of_Day",
  "Vehicle_Type", "Delivery_Category",
  "Distance_Category", "Prep_Category",
  "Exp_Category", "Late_Delivery", "Speed_Category", 
  "Order_Time_Category"
)]

# Converting all to factor type

data_cat <- as.data.frame(lapply(data_cat, as.factor))

str(data_cat)

# 7) Converting to Transactions Format
library(arules)

trans <- as(data_cat, "transactions")

# To Display transactions

itemInfo(trans)

# 8) Applying Apriori Algorithm

rules <- apriori(data = trans,
                 parameter = list(
                   supp = 0.01,
                   conf = 0.4,
                   minlen = 2,
                   maxlen = 3
                 ))

# View rules

inspect(head(rules, 10))

# 9) Sorting Rules by Lift (Most Important)

rules_sorted <- sort(rules, by = "lift", decreasing = TRUE)
inspect(head(rules_sorted, 10))

# 10) Focusing on Late Delivery Rules Only

late_rules <- subset(rules_sorted, rhs %in% "Late_Delivery=TRUE")
inspect(head(late_rules, 10))

late_rules_df <- as(late_rules, "data.frame")

#write_csv(late_rules_df, "late_delivery_rules.csv") 




#-------------------------------------------Data_Visualization-----------------------------------------

data <- Finished_Data_Cleaned

#the best to use 1

best1 <-   ggplot(data, aes(x = Total_Order_Time_min)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 10, 
                 fill = "steelblue", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1.2) +
  geom_vline(aes(xintercept = mean(Total_Order_Time_min)), 
             color = "green", linewidth = 1.5, linetype = "dashed") +
  geom_vline(aes(xintercept = median(Total_Order_Time_min)), 
             color = "purple", linewidth = 1.5, linetype = "dashed") +
  labs(title = "Distribution of Total Delivery Time",
       subtitle = "Green = Mean, Purple = Median",
       x = "Total Order Time (min)", 
       y = "Density") +
  theme_minimal()

best1 


#the best to use 2

best2 <- ggplot(data, aes(x = Vehicle_Type, y = Delivery_Time_min, fill = Vehicle_Type)) +
  geom_violin(alpha = 0.4) +
  geom_boxplot(width = 0.2, outlier.color = "red") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "gold") +
  labs(title = "Delivery Performance by Vehicle Type",
       x = "Vehicle Type", 
       y = "Delivery Time (min)") +
  theme_minimal() +
  scale_fill_manual(values = c(
    "Car" = "orange", 
    "Scooter" = "red", 
    "Bike" = "skyblue" 
  ))


best2

#the best to use 3

best3 <- ggplot(data, aes(x = Delivery_Time_min, y = Customer_Rating, color = Late_Delivery)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ Vehicle_Type, ncol = 3) +
  labs(title = "Relationship Between Delivery Time and Customer Ratings",
       x = "Delivery Time (min)", 
       y = "Customer Rating (1-5)") +
  scale_color_manual(values = c("FALSE" = "green", "TRUE" = "red")) +
  theme_minimal() 


best3

#the best to use 4

p1 <- ggplot(data, aes(x = Distance_km)) +
  geom_histogram(binwidth = 2, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Delivery Distance",
       x = "Distance (km)", y = "Count") +
  theme_minimal()

p2 <- ggplot(data, aes(x = Preparation_Time_min)) +
  geom_histogram(binwidth = 2, fill = "green", alpha = 0.7) +
  labs(title = "Distribution of Preparation Time",
       x = "Preparation Time (min)", y = "Count") +
  theme_minimal()

p3 <- ggplot(data, aes(x = Speed_KPH)) +
  geom_histogram(binwidth = 2, fill = "red", alpha = 0.7) +
  labs(title = "Distribution of Speed",
       x = "Speed (km/h)", y = "Count") +
  theme_minimal()

p4 <- ggplot(data, aes(x = Courier_Experience_yrs)) +
  geom_histogram(binwidth = 1, fill = "purple", alpha = 0.7) +
  labs(title = "Distribution of Courier Experience",
       x = "Experience (years)", y = "Count") +
  theme_minimal()


best4 <- grid.arrange(p1,p2,p3,p4,ncol = 2,nrow = 2)

best4

#the best to use 5

bp1 <- ggplot(data, aes(x = Weather, y = Total_Order_Time_min, fill = Weather)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 3) + 
  labs(title = "Outliers in Delivery Time by Weather",
       subtitle = "Red dots represent unusual delivery times",
       x = "Weather Condition", y = "Delivery Time (min)") +
  theme_minimal() +
  theme(legend.position = "none")+
  scale_fill_manual(values = c(
    "Clear" = "skyblue", 
    "Windy" = "lightblue", 
    "Foggy" = "lightgreen",
    "Rainy" = "gold",
    "Snowy" = "tomato"
  ))
bp1

bp2 <- ggplot(data, aes(x = Traffic_Level, y = Distance_km, fill = Traffic_Level)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 3) +
  labs(title = "Outliers in Delivery Distance by Traffic Level",
       subtitle = "Detecting unusually long distances in traffic",
       x = "Traffic Level", y = "Distance (km)") +
  theme_minimal() +
  theme(legend.position = "none")+
  scale_fill_manual(values = c(
    "Low" = "skyblue", 
    "Medium" = "gold",
    "High" = "tomato"
  ))

bp2

bp3 <- ggplot(data, aes(x = Vehicle_Type, y = Preparation_Time_min, fill = Vehicle_Type)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 3) +
  labs(title = "Outliers in Preparation Time by Vehicle Type",
       subtitle = "Unusual prep times across vehicle types",
       x = "Vehicle Type", y = "Prep Time (min)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")+
  scale_fill_manual(values = c(
    "Bike" = "skyblue", 
    "Car" = "gold",
    "Scooter" = "tomato"
  ))
bp3

best5 <- grid.arrange(bp1,bp2,bp3,ncol = 3)

#the best to use 6

b1 <- ggplot(data, aes(x = Time_of_Day)) +
  geom_bar(fill = "#9b59b6", color = "black", alpha = 0.8) + 
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.5) + 
  labs(title = "Total Orders by Time of Day",
       subtitle = "Distribution of demand throughout the day",
       x = "Time of Day", y = "Number of Orders") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"))

b1

b2 <- ggplot(data, aes(x = fct_infreq(Vehicle_Type))) + 
  geom_bar(fill = "#e67e22", color = "black", alpha = 0.8) +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.5) +
  labs(title = "Total Orders by Vehicle Type",
       subtitle = "Which vehicle type handles the most orders?",
       x = "Vehicle Type", y = "Number of Orders") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"))
b2

b3 <- ggplot(data, aes(x = fct_infreq(Weather))) + 
  geom_bar(fill = "#3498db", color = "black", alpha = 0.8) +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.5) +
  labs(title = "Total Orders by Weather Condition",
       subtitle = "Impact of weather on order volume",
       x = "Weather Condition", y = "Number of Orders") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"))
b3

b4 <- ggplot(data, aes(x = fct_infreq(Traffic_Level))) +
  geom_bar(fill = "#e74c3c", color = "black", alpha = 0.8) +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.5) +
  labs(title = "Total Orders by Traffic Level",
       subtitle = "Order volume across traffic conditions",
       x = "Traffic Level", y = "Number of Orders") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45 , hjust = 1, face = "bold"))
b4


best6 <- grid.arrange(b1,b2,b3,b4,ncol=4)

best6

#the best to use 7

numeric_data <- data %>% 
  select(Distance_km, Preparation_Time_min, Delivery_Time_min, 
         Courier_Experience_yrs, Speed_KPH, Total_Order_Time_min)

cor_matrix <- cor(numeric_data, use = "complete.obs")

best7 <- corrplot(cor_matrix, method = "color", type = "upper",
                  tl.col = "black", tl.srt = 45,
                  title = "Correlation Matrix of Numerical Variables",
                  mar = c(0, 0, 2, 0))
best7


#the best to use 8

best8 <- ggplot(data, aes(
  x = Delivery_Time_min,          
  y = Time_of_Day,
  fill = Time_of_Day            
)) +
  geom_density_ridges(               
    alpha = 0.7                    
  ) +
  labs(
    title = "Delivery Time Distribution by Time of Day",
    x = "Delivery Time (minutes)", 
    y = "Time of Day"
  )

best8



#------------------------------------GUI "Graphic user interface"-------------------------------


ui <- dashboardPage(
  skin = "blue",
  # Header
  dashboardHeader(
    title = "Delivery Analytics Dashboard",
    titleWidth = 300
  ),
  
  # Sidebar
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      
      # Tab 1: Overview
      
      menuItem("Overview", 
               tabName = "overview", 
               icon = icon("dashboard")),
      
      # Tab 2: Clustering
      
      menuItem("Clustering", 
               tabName = "clustering", 
               icon = icon("object-group")),
      
      # Tab 3: Association Rules
      
      menuItem("Association Rules", 
               tabName = "rules", 
               icon = icon("link")),
      
      # Tab 4: Visualizations
      
      menuItem("Visualizations", 
               tabName = "visualizations", 
               icon = icon("chart-line")),
      
      # Tab 5: Data Explorer
      
      menuItem("Data Explorer", 
               tabName = "explorer", 
               icon = icon("search")),
      
      hr(),
      
      # File Upload
      
      fileInput("file_upload", 
                "Upload Data (CSV/Excel)",
                accept = c(".csv", ".xlsx", ".xls"),
                buttonLabel = "Browse..."),
      
      # Download Results
      
      downloadButton("download_all", 
                     "Download All Results",
                     style = "color: white; background-color: #367fa9;")
    )
  ),
  
  # Body
  
  dashboardBody(
    
    # Tab 1: Overview
    
    tabItems(
      tabItem(
        tabName = "overview",
        h2("Dataset Overview"),
        
        fluidRow(
          # Value Boxes
          valueBoxOutput("total_orders_box"),
          valueBoxOutput("late_deliveries_box"),
          valueBoxOutput("avg_delivery_box"),
          valueBoxOutput("avg_rating_box"),
          valueBoxOutput("on_time_rate_box"),
          valueBoxOutput("total_distance_box")
        ),
        
        fluidRow(
          box(
            title = "Data Preview",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            collapsible = TRUE,
            DTOutput("data_preview_table")
          ),
          
          box(
            title = "Quick Summary",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            collapsible = TRUE,
            verbatimTextOutput("quick_summary")
          )
        ),
        
        fluidRow(
          box(
            title = "Top Visualizations",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            plotOutput("top_visualizations", height = "400px")
          )
        )
      ),
      
      # Tab 2: Clustering
      
      tabItem(
        tabName = "clustering",
        h2("K-means Clustering Analysis"),
        
        fluidRow(
          box(
            title = "Clustering Parameters",
            status = "warning",
            solidHeader = TRUE,
            width = 3,
            height = "500px",
            
            sliderInput("n_clusters", 
                        "Number of Clusters:",
                        min = 2, max = 8, value = 3, step = 1),
            
            selectInput("x_var_cluster", 
                        "X Variable:", 
                        choices = c("Delivery_Time_min", "Distance_km",
                                    "Speed_KPH", "Preparation_Time_min")),
            
            selectInput("y_var_cluster", 
                        "Y Variable:", 
                        choices = c("Delivery_Time_min", "Distance_km",
                                    "Speed_KPH", "Preparation_Time_min"),
                        selected = "Distance_km"),
            
            actionButton("run_cluster", 
                         "Run Clustering",
                         icon = icon("play"),
                         style = "color: white; background-color: #f39c12; width: 100%;")
          ),
          
          box(
            title = "Cluster Visualization",
            status = "primary",
            solidHeader = TRUE,
            width = 9,
            height = "500px",
            plotlyOutput("cluster_plot_output")
          )
        ),
        fluidRow(
          box(
            title = "PCA - Visualize the groups",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("pca_ind_plot", height = "400px")
          ),
          
          box(
            title = "PCA - Variables Contribution",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotOutput("pca_var_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "PCA Visualization",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotOutput("pca_plot_output", height = "400px")
          ),
          
          box(
            title = "Cluster Statistics",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            height = "auto",
            DTOutput("cluster_stats_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Cluster Interpretation",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            h4("Business Insights:"),
            verbatimTextOutput("cluster_insights_text"),
            br(),
            h4("Recommendations:"),
            verbatimTextOutput("cluster_recommendations")
          )
        )
      ),
      
      # Tab 3: Association Rules
      
      tabItem(
        tabName = "rules",
        h2("Association Rules (Apriori Algorithm)"),
        
        fluidRow(
          box(
            title = "Algorithm Parameters",
            status = "warning",
            solidHeader = TRUE,
            width = 3,
            height = "500px",
            
            sliderInput("min_support", 
                        "Minimum Support:",
                        min = 0.01, max = 0.2, value = 0.05, step = 0.01),
            
            sliderInput("min_confidence", 
                        "Minimum Confidence:",
                        min = 0.1, max = 0.9, value = 0.4, step = 0.05),
            
            sliderInput("min_lift", 
                        "Minimum Lift:",
                        min = 1, max = 5, value = 1.2, step = 0.1),
            
            radioButtons("rules_focus", 
                         "Focus on:",
                         choices = c("All Rules", 
                                     "Late Delivery Only",
                                     "High Customer Rating"),
                         selected = "Late Delivery Only"),
            
            actionButton("run_rules", 
                         "Generate Rules",
                         icon = icon("magnifying-glass"),
                         style = "color: white; background-color: #00a65a; width: 100%;"),
            
            br(), br(),
            
            downloadButton("download_rules", 
                           "Download Rules as CSV",
                           style = "color: white; background-color: #367fa9; width: 100%;")
          ),
          
          box(
            title = "Discovered Rules",
            status = "primary",
            solidHeader = TRUE,
            width = 9,
            height = "690px",
            DTOutput("rules_table_output")
          )
        ),
        
        fluidRow(
          box(
            title = "Rules Visualization",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotOutput("rules_plot_output", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Rules Insights",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            h4("Key Findings:"),
            verbatimTextOutput("rules_insights_text"),
            br(),
            h4("Actionable Recommendations:"),
            verbatimTextOutput("rules_recommendations")
          )
        )
      ),
      
      # Tab 4: Visualizations
      
      tabItem(
        tabName = "visualizations",
        h2("Data Visualizations"),
        
        fluidRow(
          box(
            title = "Plot Selection & Controls",
            status = "warning",
            solidHeader = TRUE,
            width = 3,
            
            selectInput("selected_plot", 
                        "Choose Visualization:",
                        choices = c(
                          "Total Order Time Distribution" = "best1",
                          "Delivery Time by Vehicle Type" = "best2",
                          "Rating vs Delivery Time" = "best3",
                          "Numerical Variables Distribution" = "best4",
                          "Outlier Detection" = "best5",
                          "Categorical Variables Count" = "best6",
                          "Correlation Matrix" = "best7",
                          "Density by Time of Day" = "best8"
                        ),
                        selected = "best1"),
            
            conditionalPanel(
              condition = "input.selected_plot == 'best3'",
              checkboxGroupInput("vehicle_types", 
                                 "Select Vehicle Types:",
                                 choices = c("Bike", "Scooter", "Car"),
                                 selected = c("Bike", "Scooter", "Car"))
            ),
            
            sliderInput("plot_height", 
                        "Plot Height:",
                        min = 300, max = 800, value = 500, step = 50),
            
            actionButton("refresh_plot", 
                         "Refresh Plot",
                         icon = icon("refresh")),
            
            br(), br(),
            
            downloadButton("download_plot", 
                           "Download Plot",
                           style = "color: white; background-color: #367fa9; width: 100%;")
          ),
          
          box(
            title = "Visualization Output",
            status = "primary",
            solidHeader = TRUE,
            width = 9,
            plotOutput("dynamic_plot", height = "500px")
          )
        ),
        
        fluidRow(
          box(
            title = "Plot Description",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            textOutput("plot_description")
          )
        )
      ),
      
      # Tab 5: Data Explorer
      
      tabItem(
        tabName = "explorer",
        h2("Data Explorer"),
        
        fluidRow(
          box(
            title = "Data Filtering",
            status = "warning",
            solidHeader = TRUE,
            width = 3,
            
            selectInput("filter_variable", 
                        "Filter by Variable:",
                        choices = c("Vehicle_Type", "Weather", 
                                    "Traffic_Level", "Time_of_Day",
                                    "Late_Delivery")),
            
            uiOutput("filter_values"),
            
            sliderInput("row_count", 
                        "Number of Rows to Display:",
                        min = 10, max = 100, value = 20, step = 10),
            
            actionButton("apply_filter", 
                         "Apply Filter",
                         icon = icon("filter"))
          ),
          
          box(
            title = "Filtered Data",
            status = "primary",
            solidHeader = TRUE,
            width = 9,
            DTOutput("filtered_data_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Column Statistics",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("column_stats")
          ),
          
          box(
            title = "Quick Analysis",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            selectInput("analysis_variable", 
                        "Select Variable for Analysis:",
                        choices = names(Finished_Data_Cleaned)[sapply(Finished_Data_Cleaned, is.numeric)]),
            plotOutput("quick_analysis_plot", height = "300px")
          )
        )
      )
    )
  )
)

#------------------------------------------------------------------------------
# Server Logic
#------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Reactive value for Data
  
  current_data <- reactiveVal(Finished_Data_Cleaned)
  
  # Tab 1: Overview
  
  output$total_orders_box <- renderValueBox({
    valueBox(
      value = nrow(current_data()),
      subtitle = "Total Orders",
      icon = icon("box"),
      color = "blue"
    )
  })
  
  output$late_deliveries_box <- renderValueBox({
    late_count <- sum(current_data()$Late_Delivery == TRUE)
    valueBox(
      value = late_count,
      subtitle = "Late Deliveries",
      icon = icon("clock"),
      color = "red"
    )
  })
  
  output$avg_delivery_box <- renderValueBox({
    avg_time <- round(mean(current_data()$Delivery_Time_min, na.rm = TRUE), 1)
    valueBox(
      value = paste(avg_time, "min"),
      subtitle = "Avg Delivery Time",
      icon = icon("truck-fast"),
      color = "green"
    )
  })
  
  output$avg_rating_box <- renderValueBox({
    avg_rating <- round(mean(current_data()$Customer_Rating, na.rm = TRUE), 2)
    valueBox(
      value = avg_rating,
      subtitle = "Avg Customer Rating",
      icon = icon("star"),
      color = "yellow"
    )
  })
  
  output$on_time_rate_box <- renderValueBox({
    on_time_rate <- round(mean(current_data()$Late_Delivery == FALSE, na.rm = TRUE) * 100, 1)
    valueBox(
      value = paste(on_time_rate, "%"),
      subtitle = "On-Time Delivery Rate",
      icon = icon("check-circle"),
      color = "light-blue"
    )
  })
  
  output$total_distance_box <- renderValueBox({
    total_dist <- round(sum(current_data()$Distance_km, na.rm = TRUE), 0)
    valueBox(
      value = paste(total_dist, "km"),
      subtitle = "Total Distance Covered",
      icon = icon("road"),
      color = "purple"
    )
  })
  
  output$data_preview_table <- renderDT({
    datatable(
      head(current_data(), 20),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = 'Bfrtip'
      )
    )
  })
  
  output$quick_summary <- renderPrint({
    summary(current_data()[, sapply(current_data(), is.numeric)])
  })
  
  output$top_visualizations <- renderPlot({
    #
    p1 <- ggplot(current_data(), aes(x = Total_Order_Time_min)) +
      geom_histogram(fill = "steelblue", bins = 30) +
      labs(title = "Total Order Time", x = "", y = "") +
      theme_minimal()
    
    p2 <- ggplot(current_data(), aes(x = Vehicle_Type, fill = Vehicle_Type)) +
      geom_bar() +
      labs(title = "Orders by Vehicle", x = "", y = "") +
      theme_minimal()
    
    p3 <- ggplot(current_data(), aes(x = Delivery_Time_min, y = Customer_Rating)) +
      geom_point(alpha = 0.5) +
      labs(title = "Rating vs Delivery Time", x = "", y = "") +
      theme_minimal()
    
    p4 <- ggplot(current_data(), aes(x = Weather, fill = Late_Delivery)) +
      geom_bar(position = "fill") +
      labs(title = "Late Delivery by Weather", x = "", y = "") +
      theme_minimal()
    
    gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
  })
  
  # Tab 2: Clustering
  
  observeEvent(input$run_cluster, {
    
    # K-means clustering
    
    clusters_data <- current_data()[, c("Delivery_Time_min", "Distance_km", 
                                        "Speed_KPH", "Preparation_Time_min")]
    scaled_data <- scale(clusters_data)
    
    set.seed(123)
    kmeans_result <- kmeans(scaled_data, centers = input$n_clusters, nstart = 50)
    
    # Plot
    
    output$cluster_plot_output <- renderPlotly({
      plot_data <- data.frame(
        x = current_data()[[input$x_var_cluster]],
        y = current_data()[[input$y_var_cluster]],
        cluster = as.factor(kmeans_result$cluster)
      )
      
      plot_ly(
        data = plot_data,
        x = ~x,
        y = ~y,
        color = ~cluster,
        type = 'scatter',
        mode = 'markers',
        text = ~paste("Cluster:", cluster),
        marker = list(size = 10)
      ) %>%
        layout(
          title = paste("K-means Clustering (k =", input$n_clusters, ")"),
          xaxis = list(title = input$x_var_cluster),
          yaxis = list(title = input$y_var_cluster)
        )
    })
    
    # PCA Plot
    
    output$pca_plot_output <- renderPlot({
      pca_result <- prcomp(scaled_data)
      plot_data <- data.frame(
        PC1 = pca_result$x[,1],
        PC2 = pca_result$x[,2],
        cluster = as.factor(kmeans_result$cluster)
      )
      
      ggplot(plot_data, aes(x = PC1, y = PC2, color = cluster)) +
        geom_point(size = 3) +
        stat_ellipse() +
        labs(title = "PCA Visualization of Clusters") +
        theme_minimal()
    })
    # 1. fviz_pca_ind
    output$pca_ind_plot <- renderPlot({
      if(!is.null(kmeans_result)) {
        Scaled_Data <- scale(current_data()[, c("Delivery_Time_min", "Distance_km", 
                                                "Speed_KPH", "Preparation_Time_min")])
        PCA_Data <- prcomp(Scaled_Data, scale = FALSE, center = FALSE)
        
        clusters_df <- data.frame(
          Clusters = as.factor(kmeans_result$cluster)
        )
        
        fviz_pca_ind(PCA_Data,
                     col.ind = clusters_df$Clusters,
                     palette = c("darkgreen", "red", "orange"),
                     addEllipses = TRUE,
                     legend.title = "Groups",
                     geom = "point",
                     title = "Visualize the groups"
        )
      }
    })
    
    # 2. fviz_pca_var
    output$pca_var_plot <- renderPlot({
      if(!is.null(kmeans_result)) {
        Scaled_Data <- scale(current_data()[, c("Delivery_Time_min", "Distance_km", 
                                                "Speed_KPH", "Preparation_Time_min")])
        PCA_Data <- prcomp(Scaled_Data, scale = FALSE, center = FALSE)
        
        fviz_pca_var(
          PCA_Data,
          col.var = "contrib",
          # FIX: correct argument name is "gradient.cols" not "gradients.col"
          gradient.cols = c("darkorange", "brown"),
          repel = TRUE,
          title = "Understand the direction of every cluster"
        )
      }
    })
    # 4. PCA Biplot
    output$pca_biplot <- renderPlot({
      if(!is.null(kmeans_result)) {
        Scaled_Data <- scale(current_data()[, c("Delivery_Time_min", "Distance_km", 
                                                "Speed_KPH", "Preparation_Time_min")])
        PCA_Data <- prcomp(Scaled_Data, scale = FALSE, center = FALSE)
        
        fviz_pca_biplot(PCA_Data,
                        col.ind = as.factor(kmeans_result$cluster),
                        col.var = "contrib",
                        palette = c("darkgreen", "red", "orange"),
                        gradient.cols = c("darkorange", "brown"),
                        repel = TRUE,
                        title = "PCA Biplot",
                        legend.title = "Groups"
        )
      }
    })
    
    # Cluster Statistics Table
    
    output$cluster_stats_table <- renderDT({
      cluster_summary <- current_data() %>%
        mutate(Cluster = kmeans_result$cluster) %>%
        group_by(Cluster) %>%
        summarise(
          Count = n(),
          Avg_Delivery_Time = round(mean(Delivery_Time_min), 2),
          Avg_Distance = round(mean(Distance_km), 2),
          Avg_Speed = round(mean(Speed_KPH), 2),
          Avg_Rating = round(mean(Customer_Rating), 2),
          Late_Rate = round(mean(Late_Delivery == TRUE) * 100, 1)
        )
      
      datatable(cluster_summary, options = list(pageLength = 5))
    })
    
    # Insights
    
    output$cluster_insights_text <- renderText({
      paste(
        "Analysis Results:\n",
        "- Optimal number of clusters:", input$n_clusters, "\n",
        "- Cluster sizes vary based on delivery patterns\n",
        "- Each cluster shows distinct delivery characteristics\n",
        "- Some clusters have higher late delivery rates than others"
      )
    })
    
    output$cluster_recommendations <- renderText({
      paste(
        "Recommendations:\n",
        "1. Assign priority orders to clusters with fastest delivery times\n",
        "2. Provide additional training for clusters with high late delivery rates\n",
        "3. Optimize route planning for clusters with long distances\n",
        "4. Monitor performance of each cluster regularly"
      )
    })
  })
  
  # Tab 3: Association Rules
  
  observeEvent(input$run_rules, {
    # prepare the data for Apriori
    
    data_cat <- current_data()[, c("Weather", "Traffic_Level", "Time_of_Day",
                                   "Vehicle_Type", "Late_Delivery")]
    
    data_cat <- as.data.frame(lapply(data_cat, as.factor))
    transactions <- as(data_cat, "transactions")
    
    # Generate the rules
    
    rules <- apriori(
      data = transactions,
      parameter = list(
        supp = input$min_support,
        conf = input$min_confidence,
        minlen = 2,
        maxlen = 3
      )
    )
    
    # Filter rules by selection
    
    if (input$rules_focus == "Late Delivery Only") {
      rules <- subset(rules, rhs %in% "Late_Delivery=TRUE")
    }
    
    rules_sorted <- sort(rules, by = "lift", decreasing = TRUE)
    
    # Table view
    
    output$rules_table_output <- renderDT({
      rules_df <- as(rules_sorted, "data.frame")
      datatable(
        rules_df,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      )
    })
    
    # Draw the rules
    
    output$rules_plot_output <- renderPlot({
      if (length(rules_sorted) > 0) {
        plot(rules_sorted[1:20], method = "graph", control = list(type = "items"))
      } else {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", 
             main = "No rules found with current parameters")
      }
    })
    
    # Insights
    
    output$rules_insights_text <- renderText({
      if (length(rules_sorted) > 0) {
        top_rule <- as(rules_sorted[1], "data.frame")$rules[1]
        paste(
          "Top Rule Found:\n",
          top_rule, "\n\n",
          "Key Patterns:\n",
          "- Certain weather conditions increase late delivery risk\n",
          "- Specific vehicle types perform better in certain traffic\n",
          "- Time of day affects delivery efficiency"
        )
      } else {
        "No significant rules found with current parameters. Try lowering support or confidence."
      }
    })
    
    output$rules_recommendations <- renderText({
      paste(
        "Actionable Steps:\n",
        "1. Avoid assigning bikes in rainy weather conditions\n",
        "2. Use cars for deliveries during high traffic periods\n",
        "3. Schedule more deliveries during low-traffic time slots\n",
        "4. Provide weather-specific training for couriers"
      )
    })
    
    # Download handler
    
    output$download_rules <- downloadHandler(
      filename = function() {
        paste("association-rules-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        rules_df <- as(rules_sorted, "data.frame")
        write.csv(rules_df, file, row.names = FALSE)
      }
    )
  })
  
  # Tab 4: Visualizations
  
  output$dynamic_plot <- renderPlot({
    req(input$selected_plot)
    
    # FIX: use current_data() instead of the global "data" variable for all plots
    # This ensures plots update correctly if new data is uploaded
    
    switch(input$selected_plot,
           "best1" = {
             # Total Order Time Distribution
             
             ggplot(current_data(), aes(x = Total_Order_Time_min)) +
               geom_histogram(aes(y = after_stat(density)), binwidth = 10, 
                              fill = "steelblue", alpha = 0.7) +
               geom_density(color = "red", linewidth = 1.2) +
               geom_vline(aes(xintercept = mean(Total_Order_Time_min)), 
                          color = "green", linewidth = 1.5, linetype = "dashed") +
               geom_vline(aes(xintercept = median(Total_Order_Time_min)), 
                          color = "purple", linewidth = 1.5, linetype = "dashed") +
               labs(title = "Distribution of Total Delivery Time",
                    subtitle = "Green = Mean, Purple = Median",
                    x = "Total Order Time (min)", 
                    y = "Density") +
               theme_minimal()
           },
           "best2" = {
             # Delivery Time by Vehicle Type
             
             ggplot(current_data(), aes(x = Vehicle_Type, y = Delivery_Time_min, fill = Vehicle_Type)) +
               geom_violin(alpha = 0.4) +
               geom_boxplot(width = 0.2, outlier.color = "red") +
               stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "gold") +
               labs(title = "Delivery Performance by Vehicle Type",
                    x = "Vehicle Type", 
                    y = "Delivery Time (min)") +
               theme_minimal() +
               scale_fill_manual(values = c("Car" = "orange", "Scooter" = "red", "Bike" = "skyblue"))
           },
           "best3" = {
             # Rating vs Delivery Time
             filtered_data <- current_data() %>%
               filter(Vehicle_Type %in% input$vehicle_types)
             
             ggplot(filtered_data, aes(x = Delivery_Time_min, y = Customer_Rating, color = Late_Delivery)) +
               geom_point(alpha = 0.6, size = 2) +
               geom_smooth(method = "lm", se = FALSE, color = "blue") +
               facet_wrap(~ Vehicle_Type, ncol = 3) +
               labs(title = "Relationship Between Delivery Time and Customer Ratings",
                    x = "Delivery Time (min)", 
                    y = "Customer Rating (1-5)") +
               scale_color_manual(values = c("FALSE" = "green", "TRUE" = "red")) +
               theme_minimal()
           },
           "best4" = {
             # FIX: use current_data() instead of global "data"
             p1 <- ggplot(current_data(), aes(x = Distance_km)) +
               geom_histogram(binwidth = 2, fill = "blue", alpha = 0.7) +
               labs(title = "Distribution of Delivery Distance",
                    x = "Distance (km)", y = "Count") +
               theme_minimal()
             
             p2 <- ggplot(current_data(), aes(x = Preparation_Time_min)) +
               geom_histogram(binwidth = 2, fill = "green", alpha = 0.7) +
               labs(title = "Distribution of Preparation Time",
                    x = "Preparation Time (min)", y = "Count") +
               theme_minimal()
             
             p3 <- ggplot(current_data(), aes(x = Speed_KPH)) +
               geom_histogram(binwidth = 2, fill = "red", alpha = 0.7) +
               labs(title = "Distribution of Speed",
                    x = "Speed (km/h)", y = "Count") +
               theme_minimal()
             
             p4 <- ggplot(current_data(), aes(x = Courier_Experience_yrs)) +
               geom_histogram(binwidth = 1, fill = "purple", alpha = 0.7) +
               labs(title = "Distribution of Courier Experience",
                    x = "Experience (years)", y = "Count") +
               theme_minimal()
             
             grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
           },
           "best5" = {
             # FIX: use current_data() instead of global "data"
             bp1 <- ggplot(current_data(), aes(x = Weather, y = Total_Order_Time_min, fill = Weather)) +
               geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 3) + 
               labs(title = "Outliers in Delivery Time by Weather",
                    subtitle = "Red dots represent unusual delivery times",
                    x = "Weather Condition", y = "Delivery Time (min)") +
               theme_minimal() +
               theme(legend.position = "none")+
               scale_fill_manual(values = c(
                 "Clear" = "skyblue", 
                 "Windy" = "lightblue", 
                 "Foggy" = "lightgreen",
                 "Rainy" = "gold",
                 "Snowy" = "tomato"
               ))
             
             bp2 <- ggplot(current_data(), aes(x = Traffic_Level, y = Distance_km, fill = Traffic_Level)) +
               geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 3) +
               labs(title = "Outliers in Delivery Distance by Traffic Level",
                    subtitle = "Detecting unusually long distances in traffic",
                    x = "Traffic Level", y = "Distance (km)") +
               theme_minimal() +
               theme(legend.position = "none")+
               scale_fill_manual(values = c(
                 "Low" = "skyblue", 
                 "Medium" = "gold",
                 "High" = "tomato"
               ))
             
             bp3 <- ggplot(current_data(), aes(x = Vehicle_Type, y = Preparation_Time_min, fill = Vehicle_Type)) +
               geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 3) +
               labs(title = "Outliers in Preparation Time by Vehicle Type",
                    subtitle = "Unusual prep times across vehicle types",
                    x = "Vehicle Type", y = "Prep Time (min)") +
               theme_minimal() +
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
               theme(legend.position = "none")+
               scale_fill_manual(values = c(
                 "Bike" = "skyblue", 
                 "Car" = "gold",
                 "Scooter" = "tomato"
               ))
             
             grid.arrange(bp1, bp2, bp3, ncol = 3)
           },
           "best6" = {
             # FIX: use current_data() instead of global "data"
             b1 <- ggplot(current_data(), aes(x = Time_of_Day)) +
               geom_bar(fill = "#9b59b6", color = "black", alpha = 0.8) +
               geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.5) +
               labs(title = "Total Orders by Time of Day",
                    subtitle = "Distribution of demand throughout the day",
                    x = "Time of Day", y = "Number of Orders") +
               theme_minimal() +
               theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"))
             
             b2 <- ggplot(current_data(), aes(x = fct_infreq(Vehicle_Type))) +
               geom_bar(fill = "#e67e22", color = "black", alpha = 0.8) +
               geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.5) +
               labs(title = "Total Orders by Vehicle Type",
                    subtitle = "Which vehicle type handles the most orders?",
                    x = "Vehicle Type", y = "Number of Orders") +
               theme_minimal() +
               theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"))
             
             b3 <- ggplot(current_data(), aes(x = fct_infreq(Weather))) + 
               geom_bar(fill = "#3498db", color = "black", alpha = 0.8) +
               geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.5) +
               labs(title = "Total Orders by Weather Condition",
                    subtitle = "Impact of weather on order volume",
                    x = "Weather Condition", y = "Number of Orders") +
               theme_minimal() +
               theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"))
             
             b4 <- ggplot(current_data(), aes(x = fct_infreq(Traffic_Level))) +
               geom_bar(fill = "#e74c3c", color = "black", alpha = 0.8) +
               geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.5) +
               labs(title = "Total Orders by Traffic Level",
                    subtitle = "Order volume across traffic conditions",
                    x = "Traffic Level", y = "Number of Orders") +
               theme_minimal() +
               theme(axis.text.x = element_text(angle = 45 , hjust = 1, face = "bold"))
             
             grid.arrange(b1, b2, b3, b4, ncol=4)
           },
           "best7" = {
             # FIX: use current_data() instead of global "data"
             numeric_data <- current_data() %>% 
               select(Distance_km, Preparation_Time_min, Delivery_Time_min, 
                      Courier_Experience_yrs, Speed_KPH, Total_Order_Time_min)
             
             cor_matrix <- cor(numeric_data, use = "complete.obs")
             
             corrplot(cor_matrix, method = "color", type = "upper",
                      tl.col = "black", tl.srt = 45,
                      title = "Correlation Matrix of Numerical Variables",
                      mar = c(0, 0, 2, 0))
           },
           "best8" = {
             # FIX: use current_data() instead of global "data"
             ggplot(current_data(), aes(
               x = Delivery_Time_min,          
               y = Time_of_Day,
               fill = Time_of_Day            
             )) +
               geom_density_ridges(               
                 alpha = 0.7                    
               ) +
               labs(
                 title = "Delivery Time Distribution by Time of Day",
                 x = "Delivery Time (minutes)", 
                 y = "Time of Day"
               )
           })
  }, height = function() { input$plot_height })
  
  output$plot_description <- renderText({
    switch(input$selected_plot,
           "best1" = "Shows the distribution of total order time (preparation + delivery). Helps identify typical delivery durations.",
           "best2" = "Compares delivery performance across different vehicle types using violin and box plots.",
           "best3" = "Explores relationship between delivery time and customer ratings, segmented by vehicle type.",
           "best4" = "Distribution of key numerical variables: Distance, Preparation Time, Speed, and Experience.",
           "best5" = "Outlier detection across different categorical variables.",
           "best6" = "Count of orders across categorical variables: Time of Day, Vehicle Type, Weather, and Traffic.",
           "best7" = "Correlation matrix showing relationships between numerical variables.",
           "best8" = "Density distribution of delivery time across different times of day."
    )
  })
  
  # Download plot
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("plot-", input$selected_plot, "-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      plot_func <- switch(input$selected_plot,
                          "best1" = best1,
                          "best2" = best2,
                          "best3" = best3,
                          "best4" = best4,
                          "best5" = best5,
                          "best6" = best6,
                          "best7" = best7,
                          "best8" = best8)
      ggsave(file, plot = plot_func, width = 10, height = 8)
    }
  )
  
  # Tab 5: Data Explorer
  
  output$filter_values <- renderUI({
    var <- input$filter_variable
    choices <- unique(current_data()[[var]])
    selectInput("selected_values", 
                paste("Select", var, "Values:"),
                choices = choices,
                selected = choices,
                multiple = TRUE)
  })
  
  filtered_data <- eventReactive(input$apply_filter, {
    data <- current_data()
    if (!is.null(input$selected_values)) {
      data <- data[data[[input$filter_variable]] %in% input$selected_values, ]
    }
    head(data, input$row_count)
  })
  
  output$filtered_data_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$column_stats <- renderPrint({
    var <- input$analysis_variable
    if (!is.null(var)) {
      cat("Statistics for:", var, "\n")
      cat("----------------------\n")
      print(summary(current_data()[[var]]))
      cat("\nMissing values:", sum(is.na(current_data()[[var]])))
    }
  })
  
  output$quick_analysis_plot <- renderPlot({
    var <- input$analysis_variable
    if (!is.null(var)) {
      # FIX: use .data[[var]] instead of deprecated aes_string()
      ggplot(current_data(), aes(x = .data[[var]])) +
        geom_histogram(fill = "steelblue", bins = 30) +
        labs(title = paste("Distribution of", var)) +
        theme_minimal()
    }
  })
  
  # Global download handler
  
  output$download_all <- downloadHandler(
    filename = function() {
      paste("delivery-analysis-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      # Create temporary files for export 
      tmp_dir <- tempdir()
      
      # 1  The original Data
      
      write.csv(current_data(), file.path(tmp_dir, "cleaned_data.csv"), row.names = FALSE)
      
      # 2. Statistics summary
      
      sink(file.path(tmp_dir, "summary.txt"))
      print(summary(current_data()))
      sink()
      
      # 3. collecting data in zip
      
      zip_file <- file.path(tmp_dir, "analysis_results.zip")
      files_to_zip <- c(
        file.path(tmp_dir, "cleaned_data.csv"),
        file.path(tmp_dir, "summary.txt")
      )
      zip(zip_file, files_to_zip)
      
      # copy file
      file.copy(zip_file, file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)