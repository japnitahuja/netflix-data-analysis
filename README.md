# Analysis of Movies and TV shows on Netflix in R
***Author: Japnit Kaur Ahuja***

The scope of this report is to analyse and visualize the [Netflix Movies and TV Shows](https://www.kaggle.com/datasets/shivamb/netflix-shows) retrieved from Kaggle on 31st March 2023. The data contains TV shows and Movies on the streaming platform Netlfix until mid-2021. This dataset is interesting as Netflix is one of the largest streaming services in the world. As of September 2021, Netflix has over 209 million subscribers in more than 190 countries worldwide. Due to its popularity and vast collection of movies and TV shows, the Netflix dataset is a great source for analysis.

In this notebook `R` is used for the data analysis and visualisation. Notable libraries used are `dplyr` and `tidyverse` for data manipulation and `ggplot2` from `tidyverse` for constructing graphs.

## **Preliminary Data Analysis**

The Netflix dataset contains 8807 TV shows and Movies with the following 12 features describing each row:

-   show_id: A unique alpha-numeric ID for each TV show or movie
-   type: Either a "TV Show" or a "Movie"
-   title: Name of the TV show or movie
-   director: Name of the director of the show
-   cast: Comma separated list of names of actors
-   country: Country of production
-   date_added: Date it was added on Netflix
-   release_year: Actual Release year of the move / show
-   rating: Rating of the show
-   duration: Total Duration - in minutes or number of seasons
-   listed_in: Genres of the TV show or movie
-   description: Description of the TV show or movie

```
#loading the libraries
library(tidyverse)
library(dplyr)
library(maps)
library(sf)
library(lubridate)

#loading the data
netflix_df <- read.csv("C:/Users/lenovo/Desktop/InfoViz/netflix_titles.csv")
glimpse(netflix_df)
```
![alt text](https://github.com/japnitahuja/airbnb-price-data-analysis/blob/main/Images/1.jpg)

For future usage, the date_added column is changed from String to Date format.

```{r}
#mutating the date to be in the correct format
netflix_df <- netflix_df |>
  mutate(date_added= as.Date(lubridate::parse_date_time(date_added, '%b %d, %Y')))
```

The dataset has missing values 6 columns namely: director, cast, country, date_added, rating and duration. Since except the director column, all other columns have missing values for only \< 10% of the entries, we will be omitting these rows when using them for visualisation.

```{r echo=TRUE}
#replace empty strings with NA
netflix_df[netflix_df == ''] <- NA

#Count the number of missing values by counting the number of NAs
sapply(netflix_df, function(x) sum(is.na(x)))
```
![alt text](https://github.com/japnitahuja/airbnb-price-data-analysis/blob/main/Images/2.jpg)

## **Data Visualizations**

Visualisations will be created using ggplot based on Netflix's colour theme.

```{r}
#specifying the colour scheme used for visualisations
netflix_colour_scheme = c("black","#db0000","#ffffff","#564d4d","#831010")
```

### **Figure 1: TV Shows and Movies Added to Netflix Throughout the Years**

Figure 1 shows the trend of the number of TV shows and movies that were added to Netflix each year. This graph provides insights into the growth of Netflix's content library over the years, which is a critical factor in the company's success. By examining the changes in the number of TV shows and movies added each year, we can gain a better understanding of Netflix's content strategy. The x-axis of the graph represents the year, while the y-axis shows the number of TV shows and movies added to Netflix. The graph is an area chart, with the shaded area representing the number of TV shows and movies added each year.

```{r echo=TRUE}
#omitting rows with NA values in the date added column
content_by_year <- netflix_df[!is.na(netflix_df$date_added),]

##Figure 1 Plot
ggplot(content_by_year, aes(x=year(date_added), fill=type)) + 
  geom_area(stat="count") +
  scale_fill_manual(values=netflix_colour_scheme) +
  labs(title="Content added to Netflix by Year",
       subtitle="How does the volume of content added differ by year?",
       x = "",
       y = "",
       caption="Figure 1") +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title= element_text(face="bold", size=10,vjust=-20),
        plot.subtitle= element_text(size=9, vjust=-20),
        legend.title=element_blank())
        
```
![alt text](https://github.com/japnitahuja/airbnb-price-data-analysis/blob/main/Images/3.jpg)

As can be seen from figure 1, the volume of content added to Netflix by each year has been consistently increasing till 2019 which is when most content was added. After 2019, there is a steady decline which may be attributed to covid and the reduction in production due to lock down restrictions. Another observation is that number of movies are almost double the quantity of TV shows. This shows that Netflix's content strategy is focused more on Movies than TV shows.

### **Figure 2: Average duration of movies in each country**

The average movie duration graph is useful for understanding the differences in movie duration across different countries and identifying potential trends or patterns.

Before plotting the graph, the country column needs to be fixed as there are multiple comma separated values for each movie. There are also missing values to deal with.

```{r echo=TRUE}
netflix_df |>
  select(country)
```

![alt text](https://github.com/japnitahuja/airbnb-price-data-analysis/blob/main/Images/4.jpg)

Data is manipulated using the `seperate_rows` function from tidyverse which creates multiple rows for a column with multiple values i.e. for a movie with multiple countries there would be more than one row with a different country listed as a singular value in each.

```{r echo=TRUE}
#seperating the rows if there are multiple countries listed
separate_country_df <- separate_rows(netflix_df, country, sep = ",")
#removing whitespaces
separate_country_df$country <- trimws(separate_country_df$country)
#removing rows with NA values for country
separate_country_df <- separate_country_df[!is.na(separate_country_df$country),]
#previewing changes
separate_country_df |> 
  select(title,country) |> 
  head(10)
```
![alt text](https://github.com/japnitahuja/airbnb-price-data-analysis/blob/main/Images/5.jpg)

```{r echo=TRUE}
#Filtering the dataset to remove TV shows and converting the duration 
#from "X mins" format to numeric format
movie_duration <- separate_country_df |> 
  filter(type == "Movie") |>
  mutate(duration = as.numeric(gsub("[^[:digit:]]", "", duration)))
#removing any NA values in country and duration
movie_duration <- na.omit(movie_duration[c("country","duration")])
#grouping movie by country and calculating the average duration
movie_duration <- movie_duration |> 
  group_by(country) |>
  summarise(avg_duration = mean(duration))
  
#changing names of countries that do not match the world map used later
movie_duration$country[movie_duration$country == "United States"] <- "USA"
movie_duration$country[movie_duration$country == "United Kingdom"] <- "UK"

#calculating the mean duration of all movies
mean_duration <- mean(movie_duration$avg_duration)

# categorising each country as above average, average or below average duration
# five mins above and below the mean duration is still considered the mean duration
movie_duration <- movie_duration |> 
  mutate(color = ifelse(movie_duration$avg_duration > mean_duration + 5, "#db0000",
  ifelse(movie_duration$avg_duration < mean_duration -5, "black", "#564d4d")))
  
#getting the world map
worldmap <- st_as_sf(map('world', plot = FALSE, fill = TRUE))

#left joining the world map with movie duration to have all columns
movie_duration_plot <- left_join(worldmap,movie_duration,by = c("ID" = "country")) 

#Figure 2 Plot
ggplot(data = movie_duration_plot) +
  #plotting the world graph
  geom_sf(aes(fill=color), color="white") +
  #creating the legend
  scale_fill_manual(values = c("#db0000", "#564d4d","black"), 
                    breaks = c("#db0000", "#564d4d","black"), 
                    labels = c("above average","average", "below average"),
                    na.value = "#D3D3D3")+
  #adding titles
  labs(title = "Figure 2: Average Duration (mins) of movies in each country",
        subtitle = "The average duration for a movie in all countries is 102 mins",
       caption="Figure 2",) +
  #setting theme to black and white
  theme_bw() +
  #fine tuning the design
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.title= element_text(face="bold", size=10),
        plot.subtitle= element_text(size=9),
        legend.title=element_blank(),
        legend.key = element_rect(color = NA),
        legend.key.size = unit(0.3, 'cm'),
        legend.position = "bottom",
        )
```

![alt text](https://github.com/japnitahuja/airbnb-price-data-analysis/blob/main/Images/6.jpg)

Figure 2 shows that North America, South America and most of Eurasia have movies with the average duration less than 97mins. Australia, South African Countries, some parts of Europe and East Asia make movies between 97mins to 107mins. South Asia, some parts of Africa, South East Asia and some parts of Europe make movies with an average duration greater than 107mins. These trends may have roots in the culture of the countries and history of their respective cinemas. However, this analysis may be limited if certain countries do not have enough movies on Netflix to be representative of their movie industry.

### **Figure 3: Movies by Genre**

The top 4 genres of the movies will be analysed with respect to countries. This would reveal insights about what kind of movies each country produces.

```{r echo=TRUE}
#separate multiple values in genres in each row
separate_genre_df <- separate_rows(netflix_df, listed_in, sep = ",")
separate_genre_df$listed_in <- trimws(separate_genre_df$listed_in)

#count the number of tv shows and content in each genre
separate_genre_df <- separate_genre_df |>
  group_by(listed_in) |>
  tally()
#order the count of each genre in descending order
separate_genre_df <- separate_genre_df[order(-separate_genre_df$n),]

#remove international movies and tv shows since those are specific to movies and tv shows
separate_genre_df <- separate_genre_df[!grepl("International Movies", separate_genre_df$listed_in),]
separate_genre_df <- separate_genre_df[!grepl("International TV Shows", separate_genre_df$listed_in),]

#take out the top 4 genres based on count
top_4_genre <- head(separate_genre_df,4)
top_4_genre
```
![alt text](https://github.com/japnitahuja/airbnb-price-data-analysis/blob/main/Images/7.jpg)

The top 4 genres of movies are Drama, Comedy, Documentary and Action & Adventure.

```{r echo=TRUE}
#counting the number of movies and shows by countries
country_count <- separate_country_df |> 
  group_by(country) |>
  tally() 
#sorting in decreasing order
country_count <- country_count[order(country_count$n, decreasing = TRUE),]
#take out the top 5 countries based on count of content in netflix
top_5_countries <- head(country_count,5)
top_5_countries
```
![alt text](https://github.com/japnitahuja/airbnb-price-data-analysis/blob/main/Images/8.jpg)

The top 5 countries are US, India, UK, Canada and France.

```{r echo=TRUE}
#separate both genre and country in each row as both have multiple values
separate_genre_country_df <- separate_rows(separate_country_df, listed_in, sep = ",")
#removing white space
separate_genre_country_df$listed_in <- trimws(separate_genre_country_df$listed_in)

#Figure 3 Plot
separate_genre_country_df |>
  #filter by top 5 countries
  filter(country %in% top_5_countries$country) |>
  #filter by 4 most popular genres
  filter(listed_in %in% top_4_genre$listed_in) |>
  #plot countries on the x axis
  ggplot(aes(x=country),) + 
  #plot bars based on count and fill red colour
  geom_bar(stat="count",fill="#db0000") +
  #create faceting groups by genre
  facet_wrap(~listed_in) +
  #change labels
  scale_x_discrete(labels = c("Canada", "France","India","UK","US")) +
  labs(title="Movies by Genre",
       subtitle = "Distribution of Movies in the top 4 genres within the top 5 producing countries",
       caption="Figure 3",
       x = "Countries",
       y = "Number of Movies") +
  theme_bw() +
  #finetune styling in themes
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size = 10, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 9)
  )
```

![alt text](https://github.com/japnitahuja/airbnb-price-data-analysis/blob/main/Images/9.jpg)

In figure 3 it can be seen that US generates the most number of movies in all the top 4 genres. India is second in the Action & Adventure and Comedies category being a close second in Drama category. UK is the second most documentary producing country even though it only produces 25% of the documentaries US does.

### **Figure 4: Movies and TV Shows by Age Ratings**

Ratings are guidance for viewership. The age ratings column has been transformed from multiple ratings to three categories of audiences: Kids, Teens and Adults.

```{r}
#transform ratings into the three categories
ratings_df <- netflix_df |>
  mutate(rating = case_when(
    rating %in% c("TV-Y","TV-Y7","TV-Y7 FV","TV-G","TV-PG","G","PG") ~ "Kids",
    rating %in% c("TV-14","PG-13") ~ "Teens",
    rating %in% c("TV-MA","R","NC-17") ~ "Adults",
  ))
  
#Figure 4 Plot
ratings_df |> filter(rating %in% c("Kids","Teens","Adults")) |>
  ggplot(aes(x=1,fill=rating)) +
  geom_bar(stat="count") +
  #make pie chart
  coord_polar("y", start=0) +
  theme_void() +
  #manually setting the fill labels
  scale_fill_manual(values = c("#db0000", "#564d4d","black","black"), 
                    breaks = c("Adults", "Teens","Kids","NA"), 
                    labels = c("Adults", "Teens","Kids","NA")) +
  #setting titles
  labs(title = "Movies and TV shows by Age Ratings",
       subtitle= "Most movies and shows are for an adult audience",
       caption = "Figure 4") +
  #fine tuning styling
  theme(plot.title= element_text(face="bold", size=12),
        plot.subtitle= element_text(size=10),
        legend.title=element_blank(),
        legend.key = element_rect(color = NA),
        legend.key.size = unit(0.5, 'cm'))
  
```
![alt text](https://github.com/japnitahuja/airbnb-price-data-analysis/blob/main/Images/10.jpg)

The highest type of content on Netflix is for the adult viewership followed up by teens and then kids. Thus, currently the platform is focusing on content for adults.

## **Conclusion**

This report aimed to analyze and visualize the Netflix Movies and TV Shows dataset retrieved from Kaggle. The dataset contains 8807 TV shows and Movies with 12 features describing each row. The preliminary data analysis showed that the dataset has missing values in six columns, but they are negligible, except the director column.

In conclusion, the information visualisation of the Netflix dataset have given some interesting insights into the streaming platform. The number of TV shows and movies added to Netflix has consistently increased over the years, with a sharp decline in 2020, possibly due to COVID-19. The analysis also suggests that Netflix's content strategy is more focused on movies than TV shows. Additionally, the average duration of movies in each country was examined, with the western world having shorter movies than their eastern counter parts. Looking at genres, US dominated most with India being a close second in the drama category. Netflix also mostly catered to an adult audience.

These insights can be useful in understanding Netflix's content strategy, the growth of its library, and identifying trends of it's audience.

## License

[MIT](https://choosealicense.com/licenses/mit/)



