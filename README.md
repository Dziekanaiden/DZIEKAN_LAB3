# Spatial Autocorrelation and Census Data 
## By Aiden Dziekan, October 17th 2024

# Tutorial Purpose
The goal of this tutorial is to guide users through a spatial analysis in R, with a particular focus on understanding spatial autocorrelation. By analyzing  data from the Canadian Census, users will learn how to detect and interpret spatial patterns and variability. This tutorial will provide context, practical R code, and visualizations to support the exploration of spatial autocorrelation across a Canadian City. Methods and concepts from this tutorial can be modified, enabling users to apply these techniques to a broader range of spatial datasets and research questions.

To begin, it is important to have an understanding of the concept of spatial autocorrelation and the importance of spatially analyzing Census data:
# What is Spatial Autocorrelation? 
A fundamental concept in geography is Tobler’s First Law of Geography: “Everything is related to everything else, but near things are more related than distant things” (Waters, 2017). This infers that objects that are spatially closer to one another are more likely to be similar, and as distance increases, the similarity decreases. This law forms the foundation for spatial autocorrelation (“Tobler's first law of geography”, n.d.).


Spatial autocorrelation refers to the relationship of a variable with itself over a given area and can be classified as either negative or positive. When adjacent observations have similar, indistinguishable data values, this indicates a clustering pattern known as positive spatial autocorrelation (Sharm, 2021). In contrast, negative spatial autocorrelation occurs when neighboring observations have distinct or contrasting values, reflecting a dispersed pattern (Sharm, 2021). When there is no spatial autocorrelation, the data observations are independent of their spatial locations and exhibit a random distribution (Gangodagamage et al., 2008).

The classic (and most common) way of measuring the degree of spatial autocorrelation is the Moran’s I statistic, which will be discussed and used in the subsequent sections of this tutorial.

# Spatial Autocorrelation and the Census Data
Spatial autocorrelation measures the correlation of a feature based on its geographic location (X and Y coordinates) and the associated values or attributes of that feature (Spatial Autocorrelation (Global Moran’s I), n.d.). For effective autocorrelation analysis, it is essential that the data or observations include a location. Census data meets these requirements, as it is collected at the household level, providing specific geographic locations. Additionally, this data can be aggregated to various geographic levels, such as provincial, community, or census tract levels (Statistics Canada, 2022), enabling a comprehensive analysis of spatial relationships and patterns.

Canadian Census data is collected every five years by Statistics Canada (Kalbach et al., 2006), with the most recent census being conducted in 2021. Analyzing spatial autocorrelation can reveal underlying patterns in the distribution of observations, leading to further examination to gain insights into the reasons behind this spatial variation (Li et al., 2012). Consequently, census data emerges as a valuable resource for understanding spatial autocorrelation, supporting  decision-making (Statistics Canada, 2022).

With the theory of spatial autocorrelation and the importance of Census data behind us, we can now begin by launching RStudio.

# Installing Packages and Loading Libraries
R packages are collections of functions and datasets; they enhance R's capabilities by improving existing base R features or introducing new ones (“R Packages: A Beginner's Tutorial”, 2019).  For this analysis the following packages are needed: 

**knitr**: Allows  users to embed R code within text documents, enabling the automatic generation of tables, figures, and results (Sachs, n.d.).

**tmap**: Thematic map tool to visualize spatial data (Tennekes et al., 2023).

**spdep**: Generates spatial weights matrix objects based on polygon contiguities, point patterns by distance, and tessellations, this summarizes data, and allows it to be used n spatial data analysis (Bivand et al., 2024).

**Raster**: Provides functions for manipulating/working with geographic (spatial) data in raster format (Hijmans et al., 2024).

**Shinyjs**: Allows the use of common JavaScript operations in Shiny applications without needing any knowledge of JavaScript (Attali, 2022).

**e1071**: Provides functions for statistical modelling and machine learning, including support vector machines, naive Bayes classifiers, fuzzy clustering, and more. It also includes tools for computing various statistical measures and preprocessing data (Meyer et al., 2024).

**sf**: Provides features for geographic data, allowing users to work with vector data (points, lines, and polygons) (Pebesma et al., 2024).

**terra**: Includes vector techniques (points, lines, polygons) and raster techniques (grids). Vector methods involve intersection and buffering, while raster techniques include local, focal, and zonal operations. Predictive methods support regression models for spatial predictions (Hijmans et al., 2024). 

```
install.packages("knitr")
install.packages("tmap")
install.packages("spdep")
install.packages("raster")
install.packages("shinyjs")
install.packages("e1071")
install.packages("sf")
install.packages("terra") 
```
After the installation, import the library into your R session. This will enable you to utilize all the functions specific to that package, which can be called just like any of the built-in R functions.

```
library(knitr)
library(tmap)
library(spdep)
library(raster)
library(shinyjs)
library(e1071)
library(sf)
library(terra)
```
# Setting your Working Directory
The working directory is the folder path on your computer that determines where R looks for files to load and where it saves any files you create (Phillips, 2018).

```
dir <- "Copy Your Directory Here"
setwd(dir)
```
# Brining in and Cleaning Data to R 
For this analysis we will be using a csv of 2021 cencus data from Statistics Canada as well as a and the shape file of dissemination areas of Canada. 
```
csv <- read.csv("Copy your CSV Here")

shp <- st_read("Copy your shp here")
st_crs(shp) <- 3005 #set your CRS depending on your city of intrest
```
After R reads the shape file a summary will be generated. This includes the number of spatial objects, number of attribute feilds, geometry type, spatail data dimension, the extent of the spatial data, and the coordinate reference system used. An example of this output is seen below: 

![38210E02-37E0-471D-8DF0-12F71EC7586E_4_5005_c](https://github.com/user-attachments/assets/a6c82da5-820e-4ddb-b707-069329cf8455)


If you open the CSV file, you will see that some columns are missing names. Therefore, we need to assign names to these columns for us to understand what they are. The following lines of code create a vector called cols, which contains the names you want to assign to your columns and renames the columns in your CSV file.

```
cols <- c("GEO UID", "Province code", "Province name", "CD code",
          "CD name", "DA name", "Population", "Land area", 
          "Median total income", "Income Sample Size", "French Knowledge", 
          "Language Sample Size")

colnames(csv) <- cols
```

Each data line in the CSV contains a GEO UID which is a geographic unique identifier which are variable length depending on their geographic level (“Dictionary, Census of Population”, 2016). We can use the “nchar” to count the number of characters in the string (Harris, 2024), and we assign them to a new column called LEN which can then be used to differentiate between geographic levels. This can be executed by the following code: 
```
csv$len <- nchar(csv$`GEO UID`)
```
For this analysis, we are focussing on dissemination areas. Thus, we must tell R what data entries these are.  We can use the following code, to only use rows where our Len is 8 characters, and place them in a new csv called csv_clean. 

```
csv_clean <- subset(csv, csv$len == 8)
```

The CSV file contains locational data but lacks explicit spatial features or geometry required for mapping. The shapefile, however, includes the necessary geometry and can be combined with the CSV to integrate the attribute data with the shapefile's spatial components. The following code merges the shapefile and the csv_clean file by matching the shapefile's DAUID with the CSV’s GEO_UID. 


```
census_DAs <- merge(shp, csv_clean, 
                    by.x = "DAUID", 
                    by.y = "GEO UID", 
                    all.x = TRUE)

```

For this tutorial Windsor Ontario, will be selected, however, you can choose any city provided in the CSV. The following line of code will select only data from the chosen city and create a new data frame called “Municp”.

```
Municp <- subset(census_DAs, census_DAs$CMANAME == "Windsor")
```
Next, we need to convert the French knowledge data into percentages in order to standardize values, making it easier to understand the population's proportions (Frost, n.d.) compared to raw counts. This is done by dividing the count of individuals with French knowledge by the total language sample size and multiplying it by 100.

```
Municp$PercFrench <- (Municp$`French Knowledge` / Municp$`Language Sample Size`) * 100
```

When data contains 0 or NA values and they are not removed, these values will be reflected in the analysis results, ultimately altering the outcomes. Therefore, it is better to exclude 0 or NA values so that the results accurately reflect true measures. The following lines of code will identify polygons with NA values and remove them from our “Municp” data frame, creating two new data frames: “Income_noNA” and “French_noNA.
```
Income_noNA <- Municp[!is.na(Municp$`Median total income`), ]
French_noNA <- Municp[!is.na(Municp$PercFrench), ]
```


Now that our data has been imported and cleaned, we can begin our analysis. Descriptive stats are used to summarize and describe a data set, and that is what we are going to do with our data. The two variables of interest are median total income and percentage of French language knowledge. The following code is used to calculate, the mean, standard deviation and skewness. 

```
#following three lines are for median total income variable 
meanIncome <- mean(Income_noNA$`Median total income`)
stdevIncome <- sd(Income_noNA$`Median total income`)
skewIncome <- e1071::skewness(Income_noNA$`Median total income`)


#following three lines are for percentage of french knowledge 
meanFrench <- mean(French_noNA$PercFrench)
stdevFrench <- sd(French_noNA$PercFrench)
skewFrench <- e1071::skewness(French_noNA$PercFrench)


data <- data.frame( #creates a new data fram 
  Variable = c("Income", "French Language"), the selected variables
  Mean = c(round(meanIncome, 2), round(meanFrench, 2)), #round the mean to two decimals 
  StandardDeviation = c(round(stdevIncome, 2), round(stdevFrench, 2)), #round the standard deveation to two decimals
  Skewness = c(round(skewIncome, 2), round(skewFrench, 2)) #round the skewness to two decimals 
)

kable(data, caption = paste0("Descriptive statistics for selected ", 2021, " census variables")) #create a table for the descriptive stats 

```

![57B75A8A-F8C0-4199-92E3-69EAD655DBAC](https://github.com/user-attachments/assets/34ae1269-e7aa-40f0-9858-ff33792eff21)


Now we will make a choropleth map for each variable and its distribution across Windsor. The tm_shape (“Documentation for package ‘tmap’ version 3.3-4”, n.d), function in the tmap package is used to define a layer of spatial data as the "master shape element" for a thematic map. This master layer determines the map's bounding box, and projection settings (“Documentation for package ‘tmap’ version 3.3-4”, n.d), and in this case it is the Municp. The codes with/under tm_polygonare apply the Fisher classification method for categorization and use a blue-green colour palette divided into six classes. The polygon borders are made transparent, and areas with missing income data are filled with grey. Additionally, the layout places the legend at the top right of the map and sets its width to 0.64.  Each of these lines is described with a  # beside them with the information provided from Documentation for package ‘tmap’ version 3.3-4 (n.d.). 

```{r StudyArea, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Vancouver census dissemination areas showing median total income (left) and percentage of respondants with knowledge of french (right)."}
map_Income <- tm_shape(Municp) + 
  tm_polygons(col = "Median total income",  ##The base for the pay where Municp is polygons for Windsor 
              title = "Median Total Income", ##Adds polygons to the map
              style = "fisher",  #classification Method
              palette = "BuGn", n = 6, ##Blue and green palette with 6 classes
              border.alpha = 0, ##No polygon borders
              colorNA = "grey") + #Colour for the areas with NA values

  tm_layout(legend.position = c("RIGHT", "TOP"), 
            legend.width = 0.64)  


map_French <- tm_shape(Municp) + 
  tm_polygons(col = "PercFrench", 
              title = "Percentage with \n French Knowledge", 
              style = "fisher", 
              palette = "BuGn", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"), 
            legend.width = 0.64)  


tmap_arrange(map_Income, map_French, ncol = 2, nrow = 1)
```

<img width="1004" alt="Screenshot 2024-10-17 at 4 56 48 PM" src="https://github.com/user-attachments/assets/b8c02034-5b3d-4b83-bcee-4ba43a0a387b">


The produced maps used a fisher-Jenks classification method which aims to group data that minimizes the within-group variance and maximizes the between-group variance (National Center for Health Statistics, 2022). A total of 6 classes have been chosen each representing a certain range of incomes for the left map and percentages of French knowledge for the right map. Darker red colours denote higher values and lower values by lighter colours. 

# **Defining Neighbourhoods**

As spatial Auto correlation compares observations across space to see similarities and differences, we need to define what is suitable to be compared to. In other words, we need to define what is in our neighbourhood and what is not. In this tutorial, we will be looking at two options for defying a neighbourhood. First is the Rook Weights which considers two spatial units as neighbours if they share a common edge, the second is the queen’s weight defines neighbours as spatial units that share either a common edge or a common corner (Anselin et al., 2023). Depending on which option you use will have an effect on the interpretation and results, it is important to choose the one that best suits your analysis. 

The following code will produce three maps, intended to provide a a visual understanding of how the Rook and Queen methods define neighbourhoods, the function to create the neighbourhood is “poly2nb()”. 

```{r Neighboursmap, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Kamloops census dissemination areas showing median total income neighbours queens weight (left)  rooks weight (middle) and the combination of the two (right)."}

Income.nb <- poly2nb(Income_noNA) #Creating neighbours using the queen-weight which is default if not specified 

Income.net <- nb2lines(Income.nb, coords=st_coordinates(Income_noNA)) #Uses the Income.nb to create lines connecting coneecting the polygons (neighbours)

crs(Income.net) <- crs(Income_noNA) #provides the CRS to make sure everything is in the same projection, which has been determined to be CRS 3005

Income.nb2 <- poly2nb(Income_noNA, queen = FALSE) #Creating neighbours but determining that queen weight is false, therefore determining it to use the Rooks weight to define neighbours. 

Income.net2 <- nb2lines(Income.nb2, coords=st_coordinates(Income_noNA))
crs(Income.net2) <- crs(Income_noNA)

#following lines are the same as what was used for defining neighbourhoods for the income variable but this time using the French knowledge variable. 

French.nb <- poly2nb(French_noNA)
French.net <- nb2lines(French.nb, coords=st_coordinates(French_noNA))
crs(French.net) <- crs(French_noNA)

French.nb2 <- poly2nb(French_noNA, queen = FALSE)
French.net2 <- nb2lines(French.nb2, coords=st_coordinates(French_noNA))
crs(French.net2) <- crs(French_noNA)


IncomeQueen <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
  tm_shape(Income.net) + tm_lines(col='green')


IncomeRook <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
  tm_shape(Income.net2) + tm_lines(col='blue', lwd = 2)

IncomeBoth <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
  tm_shape(Income.net) + tm_lines(col='red', lwd = 2) +
  tm_shape(Income.net2) + tm_lines(col='yellow', lwd = 2)


tmap_arrange(IncomeQueen, IncomeRook, IncomeBoth, ncol = 3, nrow = 1) #Arranging the maps to be side by side
```
![A5DC9169-586C-4028-A16F-DD3339C4968C](https://github.com/user-attachments/assets/26f3459d-a913-400a-b60e-e428f408195c)

The first map on the left shows the Queen's weight, which connects spatial units that share either a common edge or a corner. This results in a lot of neighbours being used for comparison. The second map in the middle shows the Rook weights, where connection units share a common edge only. This results in a more fragmented representation of spatial relationships with fewer neighbours being used for comparison. The map on the right overlays the two making it easy to identify their differences.

# Weighted Matrix 

A weighted matrix is used to assign how much influence neighbours should have on a particular area, allowing us to model spatial relationships and account for the varying degrees of impact nearby regions may have compared to those farther away (“Spatial weights”, n.d.). Weights are assigned by the  nb2listw() function. The Zero policy argument is used to address neighbours that do not have neighbour links. When set to TRUE, regions without neighbour links will not contribute to the analysis, by assigning weight vectors of zero. The style argument determines how the weights are distributed. There are different values that the style can take on different weights by assigning its type. W sums the links in a region and normalizes weights based on the regions total connections resulting in each neighbour having equal influence, C is similar to W but weights are assigned by the overall connectivity globally (across all regions), U is Equalized, which takes the globally standardized weights and dives it by the number of neighbours. B is a basic binary coding which assigns to all neighbours a weight of 1, and non-neighbours are assigned 0, this gives all neighbours the same influence and does not account for differentiation “Spatial weights”, n.d.).

For this tutorial, we will be using (W). The following lines of code will create two objects (Income.lw and French.lw), which are spatial weights lists derived from the neighbour lists Income.nb and French.nb, which were created using the rooks weight method. Additionally, the last line of code will produce the results for the first three regions as an example to show the weights associated with their neighbouring regions. 

```{r Final weights, echo=TRUE, eval=TRUE, warning=FALSE}
Income.lw <- nb2listw(Income.nb, zero.policy = TRUE, style = "W")


French.lw <- nb2listw(French.nb, zero.policy = TRUE, style = "W")

head(Income.lw[["weights"]])[c(1:3)]



```

![75B6F866-615C-4053-9C0F-0F3BDC1F4CE6_4_5005_c](https://github.com/user-attachments/assets/4c7860c3-a326-4a82-9a7a-d19f9feaed80)

For the first two units, each has a weight of 0.2 for 5 neighbours each, and each of those neighbours contributes equally to the weight. The third unit has weights of 0.142857, for 7 neighbours which each contribute equally. Overall, these are normalized weights for deterring the effect they have on comparing income spatially.

# Global Moran’s I
As previously mentioned in the section “What is spatial autocorrelation?” Moran’s I statistic is the most common way of measuring the degree of spatial autocorrelation. There are two types of Moran’s I, global, and local. Firstly, we will look at global Morans I and discuss local Morans I in subsequent sections of this tutorial.

Global Moran’s I provides a single value that indicates whether the entire dataset as a whole presents clustering, dispersion, or randomness (“How Spatial Autocorrelation (Global Moran's I) works”, n.d.).  Basically, the mean and variance for the attributes under analysis (Income and French Knowledge) are calculated. Each feature's value is compared to the mean, and the difference (or deviation) is determined. These deviations are then multiplied for neighbouring features to create a cross-product, which helps determine if neighbouring features are similar or different. Positive cross-products show that similar values (both high or both low) are close together, while negative cross-products indicate a mix of high and low values. The summed cross-products are normalized by variance to produce the Global Moran's I statistic (“How Spatial Autocorrelation (Global Moran's I) works”, n.d.). A value of -1 implies perfect clustering of diverse values, resembling a perfect dispersed pattern/negative spatial autocorrelation, a value of 0 indicates a lack of autocorrelation, reflecting total randomness, while a value of +1 demonstrates perfect clustering of similar values/positive spatial autocorrelation (“Moran’s I: Definition, Examples”, n.d.).

Global Morans I can be calculated by the following equation:

![B68F9F3A-F324-4297-A489-527D066E18B5](https://github.com/user-attachments/assets/5f94acbf-fa5a-4afc-8b2a-866a4caa8bea)

The following information is based on (Paula, 2023) and (“How Spatial Autocorrelation (Global Moran's I) works”, n.d.). Within the equation, you will see x which is the variable of interest and the subscript of x or j which refers to different regions. xi is the value of the variable we're looking at in region i, which is our area of focus. xj is the value in region j, which is a neighbouring area. We have already decided what our neighbours by using the Queen’s weight (see section "Defining Neighbourhoods"). The weights in the matrix W,i,j are multiplied by how much xi (the value in region i) differs from the average of x and how much xj (the value in region j) differs from the average of x.

The following block of code will perform the global Moran's I spatial autocorrelation analysis on median total income and the percentage of French speakers. 

```{r Global Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
miIncome <- moran.test(Income_noNA$`Median total income`, Income.lw, zero.policy = TRUE)


mIIncome <- miIncome$estimate[[1]]
eIIncome <- miIncome$estimate[[2]]
varIncome <- miIncome$estimate[[3]]


miFrench <- moran.test(French_noNA$PercFrench, French.lw, zero.policy = TRUE)


mIFrench <- miFrench$estimate[[1]]
eIFrench <- miFrench$estimate[[2]]
varFrench <- miFrench$estimate[[3]]
```

For this analysis, the calculation for Moran's I Statistics produced a result of 0.78 for income and 0.33 for the percentage of French speakers. For income, this suggests positive spatial autocorrelation, indicating that areas with higher median incomes are clustered together (similar to their neighbours) and areas with lower median incomes are also clustered together. Regarding French speakers there is also a positive autocorrelation suggesting the clustering of areas with higher French speakers and a cluster of areas with lower French speakers, however, this is not as strongly positive as income Moran’s I statistic as 0.78 is closer to 1. 


However, there are still two other results that were produced from that code: the expected Value of Moran's I, as well as the variance. The expected value of Moran’s I would be the result of a random distribution of the variable of interest. For median income, this value is -0.00186 and for French speakers, it is -0.00183. We can then use these values along with the variance of the Morans I statistic to analyze weather or not our results significantly differ from that of the expected random distribution. This will be discussed in further detail, through this tutorial but first we will explore the range of Moran's I. 

The minimum value of Moran’s I is the lowest region of a Moran’s I value based on our data, it indicates the maximum amount of negative spatial autocorrelation. The maximum value of Morans I is the upper region of a Morans I value based on our data; it indicates the maximum amount of positive spatial autocorrelation. This is important because it helps indicate the strength and direction of our actual Moran’s I value which should be somewhere in between the maximum and minimum. The following code is an example of how to calculate this using the income variable:

```{r Global Morans Range, echo=TRUE, eval=TRUE, warning=FALSE}
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}


range <- moran.range(Income.lw)
minRange <- range[1]
maxRange <- range[2]

```
The minimum range for income is -0.664, and the maximum range for income is 1.037. This range establishes the theoretical bounds for Moran's I, where the minimum indicates potential negative spatial autocorrelation (dissimilar incomes dispersed) and the maximum indicates strong positive autocorrelation (similar incomes clustered).

# Testing Significance

This is where those two other values come into play the variance and the expected Value of Moran's I come into play. In statistics, we can do something called a hypothesis test, in order to determine if the results that we obtain are by chance or not by chance and are actually what is expected from the data. To do this we determine a null hypothesis which states that there is no spatial autocorrelation, and the distribution is random, and the alternative hypothesis is that there is spatial autocorrelation indicating clusters (positive autocorrelation) or dispersion (negative autocorrelation). For this analysis, we will use a significance level of 95% or alpha 0.05 which indicates that we are 95% confident that our result is correct. To do this we will conduct a z-test. 
We can compare our z-score to our critical z-score of plus or minus 1.96 (for a 95% significance level). If our calculated z-score falls above + 1.96 we can conclude that our Morans I value is significantly higher and represents positive spatial autocorrelation than what would be expected under the null hypothesis of no spatial autocorrelation, and therefore reject the null hypothesis. If the calculated z-score falls below – 1.96 we can conclude that our Morans I value is significantly lower and represents negative spatial autocorrelation than the expected value under the null hypothesis and therefore reject our null hypothesis. However if our z-score falls between -1.96 and + 1.96 it suggests that our morans I value is not significantly different from the expected random value, and therefore we fail to reject the null hypothesis. 

To calculate the z-score we use the following code: 
```{r Global Morans ZScore, echo=TRUE, eval=TRUE, warning=FALSE}
zIncome <- (mIIncome - eIIncome) / (sqrt(varIncome))

zFrench <- (mIFrench - eIFrench) / (sqrt(varFrench))
```

Regarding income, the z score is 30.63 which is higher than our critical value of 1.96. This infers that at a 95% significance level, we reject the null hypothesis, and conclude that there is evidence for positive spatial autocorrelation (clustering) suggesting that areas with similar income levels are spatially grouped rather than randomly distributed. 


Regarding French speakers, the z score is 13.34 which is higher than our critical value of 1.96. This infers that at a 95% significance level, we reject the null hypothesis, and conclude that there is evidence for positive spatial autocorrelation (clustering) suggesting that areas with higher concentrations of French speakers are spatially grouped rather than randomly distributed.

# Local spatial autocorrelation/Local Moran's I 

We have already covered Global Moran’s I and now we can start exploring local Moran’s I. Global refers to having ONE value that represents our entire data set to describe our pattern compared to local which refers to a value that is applied to every observation. Therefore, a local Moran’s I test is calculated for each census tract (polygon) in Windsor to produce their own Moran’s I value, expected Moran I value and variance. This is particularly useful because we can detect areas with high or low spatial autocorrelation, (hotspots or cold spots), and areas where values are significantly different from their neighbours, (spatial outliers) (Anselin, 2020). The equation for Local Morans I is calculated by the following formula: 

<img width="974" alt="Screenshot 2024-10-17 at 10 24 41 PM" src="https://github.com/user-attachments/assets/3cda8f6c-42e2-418d-93f6-96661164a33a">


To do this in R  we can use the function localmoran(), and supply it with our desired weighting influence, and the variable of interest, very similar to the code for Global Morans I. For this analysis, we will be using Income.lw and French.lw which if you remember are weights that were calculated by summing the links in a region and normalizing weights based on the region’s total connections, and neighbours determined by the rook’s weight method. This can be done with the following code: 

```{r Local Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
lisa.testIncome <- localmoran(Income_noNA$`Median total income`, Income.lw) #Calculate local Moran’s I for Median Income 

Income_noNA$Ii <- lisa.testIncome[,1] # Local Moran's I statistic value 
Income_noNA$E.Ii<- lisa.testIncome[,2] # Expected value of Local Moran's I
Income_noNA$Var.Ii<- lisa.testIncome[,3] # Variance of Local Moran's I value 
Income_noNA$Z.Ii<- lisa.testIncome[,4] # Z-score 
Income_noNA$P<- lisa.testIncome[,5] # P-value 

lisa.testFrench <- localmoran(French_noNA$PercFrench, French.lw) #Calculate local Moran’s I for French speakers 
French_noNA$Ii <- lisa.testFrench [,1]
French_noNA$E.Ii<- lisa.testFrench [,2]
French_noNA$Var.Ii<- lisa.testFrench [,3]
French_noNA$Z.Ii<- lisa.testFrench [,4]
French_noNA$P<- lisa.testFrench [,5]

```
The results produced can be used for hypothesis testing, similar to the results from the Moran's I statistic. We have also calculated the p-value, which represents the probability of obtaining a result as extreme as, or more extreme than, what we observed, assuming that the null hypothesis is true (indicating no real effect or difference). This value shows how likely it is that any observed differences between groups occurred by chance (Dahiru, 2008). A low p-value (below 0.05) suggests that we can reject the null hypothesis, although there remains a small risk up to 5% that this rejection is a Type I error meaning we may incorrectly conclude there is a difference when there is not.

As the Local Moran’s I produce results for each observation, it would take a lot of time to elevate each one, instead, we can produce a map showing the z scores for each observation. This is done with the following code: 

```{r MappingLocalMoransI, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Windser census dissemination areas showing LISA z-scores for median total income (left) and percentage of respondants with knowledge of french (right)."}
map_LISA_Income <- tm_shape(Income_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(Income_noNA$Z.Ii),-1.96,1.96,max(Income_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))


map_LISA_French <- tm_shape(French_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(French_noNA$Z.Ii),-1.96,1.96,max(French_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

tmap_arrange(map_LISA_Income, map_LISA_French, ncol = 2, nrow = 1)
```
<img width="1025" alt="5C801D7D-555D-4AE3-9F1E-22AD245B75AA" src="https://github.com/user-attachments/assets/f4345850-89f2-49ba-83f2-dca78ba2edbb">

By using a critical z value of plus or minus 1.96 we can infer: 

That median income red areas have Z-scores from 1.960 to 5.420 indicating significant clustering of high values, reflecting positive spatial autocorrelation where similarly high values are concentrated. Grey areas have Z-scores between -1.960 and 1.960 showing no significant spatial autocorrelation, indicating a random distribution of values. Blue areas have Z-scores from -2.586 to -1.960 indicating significant clustering of low values, suggesting negative spatial autocorrelation where low values are associated with one another. Regarding French speakers red areas have Z-scores from 1.960 to 9.119 indicating significant clustering of high concentrations of French speakers, reflecting strong positive spatial autocorrelation. Grey areas have Z-scores between -1.960 and 1.960 and show no significant spatial autocorrelation, suggesting a random distribution of French speakers. Blue areas have Z-scores from -4.297 to -1.960 indicating significant clustering of low concentrations, indicating negative spatial autocorrelation where low values are spatially associated

Another way to interpret these results is by plotting the values of the local Moran’s I on a scatter plot. This allows for the interpretation of each location’s observation and its relationship with the mean. The x-axis represents the values at a certain location, while the y-axis represents the average value of its neighbours. The scatter plot is divided into four quadrants: The upper right quadrant indicates that both the value at this location and the average value of its neighbours are higher than the mean, suggesting clustering of high values (positive spatial autocorrelation). The lower left quadrant indicates that both the value at this location and the average value of its neighbours are lower than the mean, also suggesting clustering of low values (positive spatial autocorrelation). The upper left quadrant shows that the value at this location is high, while the average value of its neighbours is low, indicating a high-value outlier near low-value areas (negative spatial autocorrelation). The lower right quadrant indicates that the value at this location is low while the average value of its neighbours is high, suggesting a low-value outlier near high-value areas (negative spatial autocorrelation) (“Spatial autocorrelation: Global and local measures”, n.d.). The solid line is a line of best fit that represents the overall trend, whether that be negative or positive spatial autocorrelation. The slope of this line dictates the strength and direction (negative or positive) of the relationship. Points that are represented as diamonds are those that produced p-values below the 0.05 significance level, indicating statistically significant spatial autocorrelation in the distribution of the variable of interest. 

We can create this plot for median income with the following code: 

```{r MoransIScatter, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for median total income."}
moran.plot(Income_noNA$`Median total income`, Income.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Median Total Income ($)", 
           ylab="Spatially Lagged Median Total Income ($)", quiet=NULL)
```

And for percentage of french speakers: 
```{r MoransIScatter2, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for percentage of respondants with knowledge of french."}
moran.plot(French_noNA$PercFrench, French.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Respondants with knowledge of French (%)", 
           ylab="Spatially Lagged knowledge of French (%)", quiet=NULL)
```
These code will produce the following outputs: 

<img width="1119" alt="Screenshot 2024-10-18 at 5 04 37 PM" src="https://github.com/user-attachments/assets/1346cf94-3add-41c8-98d5-6946a567e931">

<img width="1119" alt="Screenshot 2024-10-18 at 5 03 11 PM" src="https://github.com/user-attachments/assets/9571cf16-40f5-4102-9229-7fa1d109e6f6">

They may look very chaotic but we can do a general intrepration of the use of these plots: 
Moran's I scatter plot for median income in Windsor, ON, shows census tracts on the x-axis and the neighbourhood average on the y-axis. Most points are concentrated in the bottom-left and top-right quadrants, representing low-income areas surrounded by similarly low-income neighbours and high-income areas clustered with high-income neighbours. This suggests significant positive spatial autocorrelation. Points in the top-left indicate low-income tracts with high-income neighbours, while the bottom-right shows high-income tracts surrounded by lower-income neighbours. The line through the plot emphasizes this autocorrelation: points on the line have incomes similar to their neighbours, while points above or below it differ from the average.

Similarly, Moran's I scatter plot for the percentage of French speakers shows census tracts on the x-axis and the neighbourhood average on the y-axis. Most points cluster near the center, suggesting that many census tracts have moderate percentages of French speakers, with neighbouring tracts showing similar values. Points in the top-right represent high percentages of French speakers surrounded by similarly high percentages, while the bottom-left indicates low percentages with low neighbours. Points in the top-left and bottom-right highlight tracts where the percentage of French speakers differs from neighbouring areas. The line indicates positive spatial autocorrelation, with points on or near it showing values similar to their neighbours, while those above or below the line differ from the neighbourhood average.

Overall, these plots allow us to observe how individual census tracts differ from or align with the median income or percentage of French speakers in surrounding areas.

# Conclusion 

Throughout this tutorial, we have successfully analyzed Canadian census data and conducted a spatial autocorrelation analysis in R, completing both global and local Moran’s I statistics to test for spatial autocorrelation. This process included defining neighbourhoods and creating weighted matrices. The Global Moran's I analysis demonstrates significant positive spatial autocorrelation for both median total income and the percentage of French speakers in the Windsor ONT, indicating clustering patterns where regions with similar income levels and concentrations of French speakers are spatially grouped rather than randomly distributed. The local Moran’s I provide a more detailed analysis by examining individual census tracts, while the global Moran’s I offers a single value for the entire dataset. The local Moran's I maps highlight areas of significant clustering, with red indicating high values and blue indicating low values, while grey areas suggest random distributions. Additionally, scatter plots reveal that many points cluster in the bottom-left and top-right quadrants, further indicating positive spatial autocorrelation and individual points can be analyzed to assess their relation to the overall average.

Future spatial autocorrelation analyses should consider incorporating underlying variables to better understand their influence on spatial patterns. This analysis could be conducted on both larger scales across multiple regions and smaller scales within localized areas, allowing for a more detailed examination of the factors driving spatial relationships.


	
# Refrences 




Anselin, L. (2020). Local spatial autocorrelation (1): LISA and local Moran. GeoDa Center. Retrieved from https://geodacenter.github.io/workbook/6a_local_auto/lab6a.html#local-moran

Attali, D. (2021, December 23). shinyjs - Perform common JavaScript operations in Shiny apps using plain R code. https://www.rdocumentation.org/packages/shinyjs/versions/0.6 

Bivand, R., Altman, M., Anselin, L., Assunção, R., Bera, A., Berke, O., Blanchet, F. G., Carvalho, M., Christensen, B., Chun, Y., Dormann, C., Dray, S., Dunnington, D., Gómez-Rubio, V., Koley, M., Kossowski, T., Krainski, E., Legendre, P., Lewin-Koh, N., Li, A., Millo, G., Mueller, W., Ono, H., Parry, J., Peres-Neto, P., Pietrzak, M., Piras, G., Reder, M., Sauer, J., Tiefelsdorf, M., Westerholt, R., Wilk, J., Wolf, L., & Yu, D. (n.d.). spdep: Spatial dependence: Weighting schemes, statistics and models. https://cran.r-project.org/web/packages/spdep/index.html 

Centers for Disease Control and Prevention. (2022). Jenks natural breaks. Retrieved from https://www.cdc.gov/nchs/hus/sources-definitions/jenks-natural-breaks.htm

Dahiru T. P - value, a true test of statistical significance? A cautionary note. Ann Ib Postgrad Med. 2008 Jun;6(1):21-6. doi: 10.4314/aipm.v6i1.64038. PMID: 25161440; PMCID: PMC4111019.

Esri. (n.d.). How spatial autocorrelation (Global Moran's I) works. In ArcGIS Pro documentation. Retrieved from https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/h-how-spatial-autocorrelation-moran-s-i-spatial-st.htm 

Esri. (n.d.). Spatial weights. In ArcGIS Pro documentation. Retrieved from https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/spatial-weights.htm 

Gangodagamage, C., Zhou, X., & Lin, H. (2008). Autocorrelation, spatial. Encyclopedia of GIS, 32–37. https://doi.org/10.1007/978-0-387-35973-1_83 
Government of Canada, Statistics Canada. (2022, February 9). Census Profile, 2021 Census of Population. https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/index.cfm?Lang=E 

Harris, M. (2024). The nchar function in R. StatswithR. https://www.statswithr.com/r-functions/the-nchar-function-in-r/ 

Hijmans, R. J. (2024). Spatial data analysis. In R. J. Hijmans, R. Bivand, E. Cordano, K. Dyba, E. Pebesma, & M. D. Sumner. https://cran.r-project.org/web/packages/terra/terra.pdf 

Hijmans, R. J., van Etten, J., Sumner, M., Cheng, J., Baston, D., Bevan, A., Bivand, R., Busetto, L., Canty, M., Fasoli, B., Forrest, D., Ghosh, A., Golicher, D., Gray, J., & Greenberg, J. A. (2024). raster: Geographic data analysis and modeling. https://cran.r-project.org/web/packages/raster/raster.pdf

Kalbach, W. E., Abra, E. J., & Edmonston, B. (2006, February 7). Canadian census. The Canadian Encyclopedia . https://thecanadianencyclopedia.ca/en/article/demographic-datacollection#:~:text=How%20Often%20is%20the%20Census,were%20conducted%20every%2010%20years. 

Li, L., Jiang, Z., Duan, N., Dong, W., Hu, K., & Sun, W. (2012). An approach to optimize police patrol activities based on the spatial pattern of crime hotspots. Service Science, Management, and Engineering:, 141–163. https://doi.org/10.1016/b978-0-12-397037-4.00008-9 

Lovelace, R., Nowosad, J., & Muenchow, J. (2019). Geocomputation with R: Hands-on spatial data analysis. Retrieved from https://spatialanalysis.github.io/handsonspatialdata/index.html 

Meyer, D., Dimitriadou, E., Hornik, K., Weingessel, A., & Leisch, F. (2024). e1071: Misc functions of the department of statistics (e1071), TU Wien. https://cran.r-project.org/web/packages/e1071/e1071.pdf

 Moraga, P. (2023). Spatial statistics for data science: Theory and practice with R. Chapman & Hall/CRC Data Science Series. ISBN 9781032633510. 

Pebesma, E., Bivand, R., Racine, E., Sumner, M., Cook, I., Keitt, T., Lovelace, R., Wickham, H., Ooms, J., Müller, K., Pedersen, T. L., Baston, D., & Dunnington, D. (2024). sf: Simple features for R. https://cran.r-project.org/web/packages/sf/sf.pdf

Penn State E-Education. (n.d.). Spatial autocorrelation: Global and local measures. Retrieved from https://www.e-education.psu.edu/geog586/node/672

Phillips, N. D. (2018). YaRrr! The pirate’s guide to R. Retrieved from https://bookdown.org/ndphillips/YaRrr/

Sachs, M. (n.d.). Introduction to knitr. https://sachsmc.github.io/knit-git-markr-guide/knitr/knit.html#:~:text=knitr%20is%20an%20R%20package,are%20all%20in%20one%20place. 

Sharma, V., Sharma, M., Pandita, S., Kour, J., & Sharma, N. (2021). Application of geographic information system and remote sensing in Heavy Metal Assessment. Heavy Metals in the Environment, 191–204. https://doi.org/10.1016/b978-0-12-821656-9.00011-0 

Spatial autocorrelation (Global Moran’s I) (spatial statistics). Spatial Autocorrelation (Global Moran’s I) (Spatial Statistics)-ArcGIS Pro | Documentation. (n.d.). https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/spatial-autocorrelation.htm 

Statistics Canada. (2016). Dictionary, Census of Population, 2016. Statistics Canada. https://www12.statcan.gc.ca/census-recensement/2016/ref/dict/index-eng.cfm

Statistics How To. (n.d.). Moran's I: Spatial autocorrelation. Retrieved from https://www.statisticshowto.com/morans-i/#:~:text=What%20is%20Moran's%20I%3F,the%20observations%20are%20not%20independent.

Team, D. (2019, March 25). R packages tutorial: How to download & install R packages. https://www.datacamp.com/tutorial/r-packages-guide 

 Tennekes, M., Nowosad, J., Gombin, J., Jeworutzki, S., Russell, K., Zijdeman, R., Clouse, J., Lovelace, R., & Muenchow, J. (n.d.). TMAP: Thematic maps. https://cran.r-project.org/web/packages/tmap/tmap.pdf

Tobler’s first law of geography. Tobler’s first law of geography | EPFL Graph Search. (n.d.). https://graphsearch.epfl.ch/en/concept/2269821 

Waters, N. (2017). Tobler’s first law of geography. International Encyclopedia of Geography, 1–13. https://doi.org/10.1002/9781118786352.wbieg1011 
