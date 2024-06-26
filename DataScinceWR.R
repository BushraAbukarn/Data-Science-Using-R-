title: "Review"
output: html_document
date: "2023-11-13"

```{r "setup, include=FALSE"}
rm(list=ls())
```


```{r "subset by row with filter()"}
diamonds_sm <- diamonds %>% 
  filter(cut == "Ideal",
         price > 1000)
mpg %>% 
  filter(cty >= 20)
mpg %>% 
  filter(manufacturer == "ford")
```

```{r "filter for missing values"}
diamonds %>% filter(is.na(price))
```

```{r "subset by column"}
#1 
diamonds_sm1 <- diamonds %>% 
  select(cut, color)
#2 
diamonds %>% 
  select(1:4)
#3 
diamonds %>% 
  select(starts_with("p"))
#4 
diamonds %>% 
  select(contains("c"))
```

```{r "moving the column or changing the column place"}
diamonds %>% 
  select( price, 
          everything())
# eleminate a specfic coulmn 
diamonds %>% 
  select(-cut)
```


```{r "reorder row with arrange"}
diamonds_arr <- diamonds %>% 
  arrange(color)
#arrange by numerical value
diamonds_arr <- diamonds %>% 
  arrange(carat)
#arrange by multiple criteria
diamonds_arr <- diamonds %>% 
  arrange(color, carat)
#arrange by desc order 
diamonds_arr <- diamonds %>% 
  arrange(desc(carat))
```

```{r "transposed version of print"}
glimpse(diamonds)
```


```{r "add or modofy columns with mutate"}
#convert carat to gram & modify column names & create a new column
diamonds_new <- diamonds %>% 
  mutate(mass_g = .20 * carat,
         price_per_carat = price / carat, 
         cut = tolower(cut),
         expensive_TF = price > 1000)
```


```{r "grouped summarise with group_by() and summarise()"}
diamonds %>% 
  group_by(cut) %>% 
  summarise(avg_price = mean(price),
            sd_price = sd(price))
# Group by multiple variables
diamonds %>% 
  group_by(cut, color) %>% 
  summarise(avg_price = mean(price),
            sd_price = sd(price))
# Count observations
diamonds %>% 
  count(color)

# Group by price > 1000
diamonds %>% 
  group_by(Expensive = price > 1000) %>% 
  summarise(avg_price = mean(price),
            sd_price = sd(price),
            count = n())
```


```{r "other smaller verbs"}
?slice_max
?blind_rows
?left_join
?rename
?case_when
```

ggplot

```{r "The basics"}
crickets %>% 
  ggplot(aes(x = temp, y = rate, color = species)) +
  geom_point() + labs( x = "Temperature", y = "Chrip Rate", 
                       color = "Species", 
                       title = "Cricket chrips") + 
  scale_color_brewer(palette = "Dark2")
```

```{r "Modifiying basic properties of plot"}
crickets %>% 
  ggplot(aes(x = temp, y = rate)) +
  geom_point(color = "red", size = 2, shape = "square") + 
  labs( x = "Temperature", y = "Chrip Rate", 
        title = "Cricket chrips")
```


```{r "Learn more about the options for the ggplot"}
#Adding another layer
crickets %>% 
  ggplot(aes(x = temp, y = rate)) +
  geom_point(color = "blue", size = 2) + 
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  labs( x = "Temperature", y = "Chrip Rate", 
        title = "Cricket chrips")

```

```{r "two linear regression lines"}
crickets %>% 
  ggplot(aes(x = temp, y = rate, color = species)) +
  geom_point() + geom_smooth( method = "lm", se = FALSE) +
  labs( x = "Temperature", y = "Chrip Rate", 
        color = "Species", 
        title = "Cricket chrips") + 
  scale_color_brewer(palette = "Dark2")
```

```{r "Other plots"}
#One quantitive variable 
crickets %>% 
  ggplot(aes (x = rate)) +
  geom_histogram( bins = 15)
#One qualitative variable 
crickets %>% 
  ggplot(aes(x = species)) + 
  geom_bar(color = "blue", fill = "lightblue")
#Other way & hiding the legend
crickets %>% 
  ggplot(aes(x = species, fill = species)) + 
  geom_bar(show.legend = FALSE) + 
  scale_fill_brewer(palette = "Dark3")
```


```{r}
#One qualitative variable and one quantitive variable 
crickets %>% 
  ggplot(aes(x = species, y = rate, color = species)) + 
  geom_boxplot() + 
  scale_color_brewer(palette = "Dark2") + 
  theme_minimal()
```

```{r "Faceting, Side by side plots for the two different species"}
crickets %>% 
  ggplot( aes( x= rate,fill = species)) + 
  geom_histogram(bins = 15, show.legend = FALSE) + 
  facet_wrap(~species)
```

If-else statment

```{r}
my_word <- "hello"
if(nchar(my_word) <= 5) {
  print("It's a short string")
} else if (nchar(my_word) <= 8) {
  print("It's a mid-length string") 
} else { 
  print("It's a long string")
}
```

```{r "A handy shortcut if x < 5 then y = 0, else y = 10"}
x <- 4
y <- ifelse (x < 5, 0 , 10)
y 
```


```{r "glimpse command gives you a sort top level overview of the dataset"}
glimpse(mpg)
```

Ch 3 Data transformation 

```{r "dplyr basics"}
flights %>% 
  filter(dest == "IAH") %>% 
  group_by(year, month, day) %>% 
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE))
```

```{r "A shorter way to select flights that departed in Jan or Feb"}
flights %>% 
  filter(month %in% c(1,2))
```

```{r "Remove duplicate rows"}
flights %>% 
  distinct()
# We can specify the columns
```

```{r "Finding the number of occurrence"}
flights %>% 
  count(origin, dest, sort =TRUE)
```


```{r}
flights %>% 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .before = 2
  )
# We can use after as well!
```

```{r "Alternatively, you can control which variables are kept with the .keep argument.  We are going to learn ".keep = used" which specifies that we only keep the columns that were involved in the calculation"}
flights %>% 
  mutate( 
    gain = dep_delay - arr_delay,
    hours = air_time / 60, 
    gain_per_hour = gain / hours,
    .keep = "used")
```


```{r "trnasmute"}
flights %>% 
  transmute(
    gain = dep_delay - dep_time, 
    hours = air_time/60,
    speed = distance/hours
  )
```

```{r "We are going to use "integer division" and "remainder operator" to separte infromation in dep_time"}
flights %>% 
  transmute(dep_time,
            hr = dep_time %/% 100,
            min = dep_time %% 100)
```


```{r "Select function"}
#Remove variabels start form year to arr_delay
flights %>% select(-(year:arr_delay))

#Rename a variable
flights %>% rename(tail_num = tailnum) 

#Compare with, reording the variable  
flights %>% select(flight, tail_num = tailnum, everything())

#Selecting variables that are characters
flights %>% 
  select(where(is.character))

flights %>% select(starts_with("arr"))

flights %>% select(end_with("time")) 

flights %>% select(contains("dep"))
```

#Relocate function moves variable around.
```{r "relcoate functions"}
flights %>% relocate(time_hour, air_time)

flights %>% relocate(year:dep_time, .after = distance)

flights %>% relocate(starts_with("arr"), .before = dep_delay)
```


```{r}
flights %>% 
  filter(dest == "IAH") %>% 
  mutate(origin, speed = round(distance/air_time * 60, 1),
         .keep = "used") %>% 
  arrange(desc(speed))
```


```{r "group_by and summarise function"}
flights %>% group_by(month) %>% 
  summarise(
    avg_dep_delay = round(mean(dep_delay, na.rm =TRUE),3), 
    avg_arr_delay = round(mean(arr_delay, na.rm = TRUE),3),
    n = n()
  ) %>% 
  arrange(desc(avg_dep_delay))
```

The slice function 
* `slice_head(n=1)` : takes the first row from each group.
* `slice_tail(n=1)` : takes the last row in each group. 
* `slice_min(column, n =1)` : takes the row with smallest value in a `column`. 
* `slice_max(column, n =1)` : takes the row with largest value in a `column`. 
* `slice_sample(n=1)` : takes on random row in a group. 


```{r "slice function"}
flights %>% group_by(dest) %>% 
  slice_max(arr_delay, n=1) %>% 
  select(origin, dest, arr_delay)
```

#Per_operation grouping by by.argument 
```{r ".by argument"}
flights %>% 
  summarise(
    avg_dep_delay = round(mean(dep_delay, na.rm=T),1),
    n = n(),
    .by = month) %>% 
  arrange(desc(avg_dep_delay))

#Group by multiple variables

flights %>% 
  summarise(
    avg_dep_delay = round(mean(dep_delay, na.rm = T),1),
    n = n(), 
    .by = c(origin, dest)
  ) %>% 
  arrange(desc(avg_dep_delay))
```

#Ch3 Exercise.
```{r "CASE I: some visuliaztion with flight data"}
flights %>% 
  summarise(
    avg_dep_delay = round(mean(dep_delay, na.rm = T),1),
    n = n(),
    .by = month
  ) %>% 
  
  ggplot(aes(x=month, y= avg_dep_delay)) + 
  geom_point() + 
  geom_smooth(se=F)
```

```{r "visualization by destination"}
flights %>% 
  summarise(
    avg.dist = round(mean(distance, na.rm = T),1),
    avg.speed = round(mean(distance/arr_time*60, na.rm = T),1),
  ) %>% 
  ggplot(aes(x=avg.dist, y=avg.speed)) + 
  geom_point(alpha = .3) + 
  geom_smooth(se = F)
```

#Case 2: 
```{r "Compare what propprtion pf times a player gets a hit (H) based on the number of times they try to put the ball in play"}
Batting %>% group_by(playerID) %>%
  summarise(
    performance = round(sum(H)/sum(AB),3),
    n = sum(AB)
  ) %>% 
  filter(n > 100) %>% 
  arrange(desc(performance)) %>% 
  ggplot(aes(x = n, y = performance)) + 
  geom_point(alpha = .1) + 
  geom_smooth(se=F)
```

Data Tyding 
There are 3 interrelated rules that make a data set tidy:
  1. Each Variable is a column.
2. Each observation is a row. 
3. Eahch value is a cell. 

```{r}
#Compute the rate of TB per 10,000
table1 %>% 
  mutate(TBrate = round(cases/population*1000,3))

#Compute total cases per year 
table1 %>% group_by(year) %>% 
  summarise(
    tot_cases = sum(cases), 
    tot_pop = sum(population), 
    TBrate = round(tot_cases/tot_pop*1000,3),
    n = n()
  )

#Visulaize change over time
table1 %>% 
  ggplot(aes(x= year, y =cases)) + 
  geom_point(aes(shape=country)) + 
  geom_line(aes(color=country)) + 
  scale_x_continuous(breaks = c(1999,2000)) + 
  labs(
    title = "TB incidence", 
    x = "Year", 
    y = "Cases", 
    shape = "Country", 
    color = "Country"
  )
```

#Lengthening data (Long format)
```{r "Make long format using pivot_longer function"}
billboard %>% 
  pivot_longer(
    #Whihc columns need to be pivoted
    cols = starts_with("wk"), 
    names_to = "wks",
    values_to = "rank",
    values_drop_na = T
  ) %>% 
  select(!date.entered) %>%  #Removing date.entered column
  #Now we want to change week varaible to just numbers as numeric
  mutate(wks = parse_number(wks)) %>% 
  #Data visulization
  ggplot(aes(x=wks, y = rank, group = track)) + geom_line(alpha = .2) +
  scale_y_reverse()
```

```{r "Exercise for pivot_longer with a toy data"}
df1 = tribble(
  ~id, ~bp1, ~bp2, ~bp3,
  "A", 100, 120, 124, 
  "B", 140, 115, 132, 
  "C", 120, 125, 129, 
  "D", 139, 119, 139
) 
df1 %>% 
  pivot_longer(
    cols = bp1:bp3,
    names_to = "BP_measurments", #The new column name that contain the columns. 
    values_to = "BP_value" #The new column name that contain the values.
  ) %>% 
  mutate(BP_measurments = parse_number(BP_measurments))
```

#More complex data, called `who2`
```{r "Exercise with who2 TB incidence data"}
who2 %>% 
  pivot_longer(
    cols = !(country:year), #Manipulating all columns expect country to year columns. 
    names_to = c("diagnosis", "gender", "age_gp"),
    names_sep = "_",
    values_to = "TBcount",
    values_drop_na = T
  )
```

```{r "Exercise with household data"}
household %>% 
  pivot_longer(
    cols = !family, 
    names_to = c(".value", "child"),
    names_sep = "_", 
    values_drop_na = T
  ) %>% 
  mutate(child = parse_number(child))
```

#Widening Data
Pivote_wider makes datasets wider by increasing columns and reducing rows and helps when one observation is spread across multiple rows
```{r "pivot_wider function"}
cms_patient_experience %>% 
  pivot_wider(
    id_cols = starts_with("org"),
    names_from = measure_cd, 
    values_from = prf_rate
  )
```

```{r "Simple exercise"}
df2 = tribble(
  ~id, ~measurement, ~value, 
  "A", "bp1", 100, 
  "B", "bp1", 140, 
  "B", "bp2", 115, 
  "A", "bp2", 124, 
  "A", "bp3", 103,
)

df2 %>% 
  pivot_wider(
    id_cols = id, 
    names_from = measurement,
    values_from = value
  )
```

#Data Import
The basics of reading data files into R.
```{r}
df2 = read.csv("https://pos.it/r4ds-students-csv",
               na = c("N/A", "")) 
df3 = df2 %>% 
  mutate(meal_plan = factor(mealPlan),
         age= parse_number(
           if_else(AGE == "five", "5", AGE)
         ))

#To save this dataset in the PC. 
write.csv(df3, "C:\\Users\\bushr\\OneDrive\\Desktop\\File\\df3.csv") 

#If we want to read multiple files and append those. 
sales_files <- c(
  "https://pos.it/r4ds-01-sales",
  "https://pos.it/r4ds-02-sales",
  "https://pos.it/r4ds-03-sales"
)
read_csv(sales_files, id = "file")
```

#Layer in data visualization.

```{r} 
#Here, geom_smooth() separates the cars into three lines based on their drv value, which describes a car’s drive train.
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) + 
  geom_point() +
  geom_smooth(aes(linetype = drv))
```

#We can use the same idea to specify different data for each layer. 
```{r}
#Here, we use red points as well as open circles to highlight two-seater cars.  
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_point(
    data = mpg %>%  filter(class == "2seater"), 
    color = "red"
  ) +
  geom_point(
    data = mpg %>%  filter(class == "2seater"), 
    shape = "circle open", size = 3, color = "red"
  )
```

*Comparison of distributions using `ggridges` packge. 
```{r}
mpg %>% 
  ggplot(aes(x=hwy, y=drv, fill=drv)) + 
  geom_density_ridges(alpha=.5, show.legend = F)
```

```{r}
#What if we want to reorder boxplot based on the median value of hwy
mpg %>% 
  ggplot(aes(x=class, y=hwy)) +
  geom_boxplot()

mpg %>% 
  ggplot(aes(x=fct_reorder(class, hwy, median),
             y = hwy)) + 
  geom_boxplot() + 
  coord_flip() + 
  labs(
    x = "Class", 
    y = "Hwy mileage (MPG)", 
    caption = "Data from fueleconomy.gov"
  )
```

```{r}
#Two categorical variables 
mpg %>% 
  count(drv,class) %>% 
  ggplot(aes(x = drv, y=class)) +
  geom_count() +
  geom_tile(aes(fill=n))
```

#`facet_wrap()` command splits a plot into subplots that each display one subset of the data based on a categorical variable.
```{r}
mpg %>% 
  ggplot(aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_wrap(~cyl)
#To facet your plot with the combination of two variables, switch from facet_wrap() to facet_grid(). The first argument of facet_grid()
mpg %>% 
  ggplot(aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_grid(drv ~ cyl)
```

#Labeling and Annotations
```{r "Math symbols on the label"}
df1 = tibble(
  x = 1:10, 
  y = cumsum(x^2)
) 
df1 %>% 
  ggplot(aes(x,y)) + 
  geom_point() + 
  labs(
    x = expression(italic(x[i])), 
    y = expression(italic(x[i]^2, i=1,n))
  )
```

```{r}
label_info = mpg %>% 
  group_by(drv) %>% 
  arrange(desc(displ)) %>% 
  slice_head(n=1) %>% 
  mutate(
    drive_type = case_when(
      drv == "f" ~ "front-wheel drive", 
      drv == "r" ~ "rare-wheel drive", 
      drv == "4" ~ "4-wheel frive"
    )
  ) %>% 
  select(displ, hwy, drv, drive_type)
```

```{r "Annotation via geom_text function"}
mpg %>% 
  ggplot(aes(x = displ, y = hwy, color = drv, shape = drv)) + 
  geom_point(alpha =.5) + 
  geom_smooth(se=F) + 
  geom_text(
    data = label_info, 
    aes(x = displ, y = hwy, label = drive_type), 
    hjust = "right", vjust = "bottom", 
    nudge_y = 1, 
    family = "Times New Roman"
  ) + 
  theme(legend.position = "none") 
```

#Transform 
```{r "Logical vectors"} 
#A logical vector is a vector that only contains TRUE and FALSE values.
flights %>% 
  filter(
    dep_time > 800 & dep_time < 1800 & abs(arr_delay) < 15
  ) %>% 
  mutate(
    daytime_flight = dep_time > 800 & dep_time < 1800,
    ontime_flight = abs(arr_delay) < 15, 
    .keep = "used"
  )

#Alternatively, 
flights %>% 
  mutate(
    daytime_flight = dep_time > 800 & dep_time < 1800, 
    ontime_flight = abs(arr_delay) < 15, 
    .keep = "used"
  ) %>% 
  filter(daytime_flight & ontime_flight)
```

```{r "Missing values"}
#What if we want to drop missing values & save into a new dataframe 
flights %>% 
  filter(!is.na(dep_time))
```

* logical summaries 
There are two main logical summaries: 
  * `any` is equivalent to "or" operator.
* `all` is equivalent to "and" operator.
```{r}
flights %>% 
  group_by(year, month, day) %>% 
  summarise(
    all_dep_delayed = all(dep_delay <= 60,  na.rm = T), 
    any_long_arr_delay = any(arr_delay >= 300, na.rm = T), 
    n = n()
  )  
```

```{r "A review of case_when function"}
flights %>% 
  mutate(
    flight_status = case_when(
      is.na(arr_delay) ~ "Cancelled", 
      arr_delay < -30 ~ "Very early arival", 
      arr_delay < -15 ~ "Early arrival", 
      abs(arr_delay) <= 15 ~ "On time", 
      arr_delay < 60 ~ "Late", 
      arr_delay < Inf ~ "Very late"
    ),
    .keep = "used"
  )
```

#Numeric vectors
```{r "Count function"}
flights %>% 
  count(dest, sort=T) 

#Alternative way
flights %>% 
  group_by(dest) %>% 
  summarise(
    n= n()
  ) %>% 
  arrange(desc(n))
```

#Let's look at variants of `n()` and `count()`. 
Introduce `n_distinct(x)` : count the number of distinct/unique values of a variable. 
```{r}
# We could figure out which destinations are served by the most carriers
flights %>% 
  group_by(dest) %>% 
  summarise(
    carriers = n_distinct(carrier)
  ) %>%  arrange(desc(carriers))
```

```{r "Another example of using count function, we could “count” the number of miles each plane flew"}
flights %>% 
  group_by(tailnum) %>% drop_na(tailnum) %>%  
  summarise(
    tot_miles = sum(distance, na.rm = T) 
  ) %>% 
  arrange(desc(tot_miles))

#Use of count function 
flights %>% 
  count(tailnum, wt = distance) %>% drop_na(tailnum) %>% 
  arrange(desc(n))
```

#Introduction to `pmin` and `pmax` functions. 
```{r}
df1 = tribble(
  ~x, ~y, 
  1, 3, 
  5, 2, 
  7, NA
) 
#Returns the (regular or parallel) maxima and minima of the input value.
df1 %>% 
  mutate(
    minimum = pmin(x, y, na.rm = T), 
    maximum = pmax(x, y, na.rm = T )
  )
```


```{r}
#In R, %/% does integer division and %% computes the remainder
flights %>%  
  mutate(
    hour = sched_dep_time %/% 100,
    minute = sched_dep_time %% 100,
    .keep = "used"
  )
```

Exercise:We want to visualize how the proportion of cancelled flights varies over the course of the day. 
```{r}
flights %>% 
  group_by(hourly = sched_dep_time %/% 100) %>% 
  summarise( 
    prop_canelled = mean(is.na(dep_time)), 
    n = n()) %>% 
  filter(hourly > 1) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x = hourly, y = prop_canelled)) + 
  geom_point(aes(size=n)) + 
  geom_line(color = "blue") +
  scale_y_continuous(labels = scales::percent)
#A line plot with scheduled departure hour on the x-axis, and proportion of cancelled flights on the y-axis. Cancellations seem to accumulate over the course of the day until 8pm, very late flights are much less likely to be cancelled
```

#String vectors
Introduce `str_length` function via `babynames::babynames` dataset. 
```{r}
babynames %>% 
  count(name, name_length = str_length(name), wt = n, sort = T) %>%
  filter(name_length == 7)
```

Q: Find all most popular names containing a lower-case. 
```{r}
babynames %>% 
  filter(str_detect(name, "x")) %>% 
  count(name, wt = n, sort = T)
```

#Factors (Categorical variables)
* Introduce `fct_reorder()` function: This function has 3 arguments 
1. `f`: the factor whose levels you want to modify. 
2. `x`: a numeric vector that you want to use to reorder the levels. 
3. `fun`: a function is used if there are multiple values `x` for each value of `f`. 
The default function is `median`. 
Use `forcats::gss_cat` data
```{r}
gss_cat %>% 
  group_by(relig) %>% 
  summarise(
    avg_tvhrs = round(mean(tvhours, na.rm = T), 1), 
    n=n()
  ) %>% 
  ggplot(aes(x=avg_tvhrs, y=fct_reorder(relig, avg_tvhrs))) + 
  geom_point()
```

Q: What if we create a similar plot looking at how *average age* varies across *reported income level*. 
```{r}
#To pull “Not applicable” to the front with the other special levels.
gss_cat %>% 
  group_by(rincome) %>% 
  summarise(
    avg_age = round(mean(age, na.rm = T),1) 
  ) %>% 
  ggplot(aes(x=avg_age, y=fct_relevel(rincome, "Not applicable"))) + 
  geom_point()
```

```{r "Useful color line plot"}
gss_cat %>% 
  filter(!is.na(age)) %>% 
  count(age, marital) %>% 
  group_by(age) %>% 
  mutate(
    prop = n/sum(n)
  ) %>% 
  ggplot(aes(x= age, y= prop, 
             lty= marital)) + 
  geom_line()

```

Making Pareto chart based on martial status via `fct_infreq()` function 
```{r}
#We can use fct_infreq() to order levels in decreasing frequency
gss_cat %>% 
  mutate( 
    sorted_marital = 
      marital %>% fct_infreq() #for Pareto chart
  ) %>% 
  ggplot(aes(x=sorted_marital))+ 
  geom_bar()
```

* Demonstrate how modify factor levels via `fct_recode()` & `fct_collapse` functions.
```{r}
#Change factor levels by hand through `fct_recode`. 
gss_cat %>% 
  count(partyid) 

gss_cat %>% 
  mutate(
    party_id = fct_recode(partyid, 
                          "Republican_strong" = "Strong republican", 
                          "Republican_weak" = "Not str republican", 
                          "Independent_near_rep" = "Ind,near rep", 
                          "Independent_near_dem" = "Ind,near dem", 
                          "Democrat_weak" = "Not str democrat", 
                          "Democrat_strong" = "Strong democrat", 
                          "Other" = "No answer", 
                          "Other" = "Don't know", 
                          "Other" = "Other party")
  ) %>% 
  count(party_id)

#Use `fct_collapse` function:collapse factor levels into manually defined groups
gss_cat %>% 
  mutate(
    party_id = fct_collapse(partyid, 
                            "Republican" = c("Strong republican", "Not str republican"), 
                            "Independent" = c("Ind,near rep", "Ind,near dem"), 
                            "Democrar" = c("Not str democrat", "Strong democrat"),
                            "Other" = c("No answer", "Don't know", "Other party")
    )
  ) %>% 
  count(party_id)
```

```{r "fct_lump_lowfre function"} 
#Lump uncommon factor together levels into "other"
gss_cat %>%  
  count(relig) %>% 
  arrange(desc(n))

gss_cat %>% 
  mutate(
    relig_lump = fct_lump_n(relig, n=5)
  ) %>% 
  count(relig_lump) %>% 
  arrange(desc(n))
```

#Basic joins 
learn `left_join` and `semi_join` functions
```{r}
flights2 = flights %>%  
  select(origin, dest, tailnum, carrier, time_hour)

flights2 %>% 
  left_join(planes %>% 
              select(tailnum, type, model, seats, speed))

flights2 %>% 
  filter(tailnum == "N3ALAA")

planes %>% 
  filter(tailnum == "N3ALAA")
```

So far we learned about *natural* join. Now we are going to learn how to use `join_by` argument. 
```{r}
flights2 %>% 
  left_join(airports, join_by(origin==faa))
```

*filtering* join
Q: We want to filter `airports` dataset to show just the origin airports using `semi_join` function. 

```{r}
airports %>% 
  semi_join(flights2 , join_by(faa==origin))
```

#Databases 
Learning basics of DBI(database interface) package.
-How to use it to connect a database and then retrieve data with a SQL (structured query language) queries.

DBI is a low-level interface that *connects* to database from R, upload data and executes SQL queries by `dbplyr::dbplyr` that is a high level interface that translate `dbplr` code to SQL queries then executes them with DBI. 

-How to create a database connection in R? 
  via `DBI::dbconnect()`

For this class, we use in-process DBMS via R package `duckdb`. 
```{r}
con <- DBI::dbConnect(duckdb::duckdb())
```

```{r "Acess some data to connection"}
#Add two datasets in ggplot package: mpg, diamonds using DBI::dbWriteTable()
dbWriteTable(con, "mpg", ggplot2::mpg, overwrite=T)
dbWriteTable(con, "diamonds", ggplot2::mpg, overwrite=T)

dbListTables(con)

con %>% 
  dbReadTable("diamonds") %>% 
  as_tibble()
```

The top-level components of SQL are *statements*. Common statments include: 
  - `CREATE` for defending new tables. 
- `INSERT` for adding data.
_ `SELECT` for retrieving data.

A query is made up of *clauses*: `SELECT`, `FROM`, `WHERE`, `ORDER BY`, and `GROUP_BY`.
Every query must have `SELECT` and `FROM` clauses. 
```{r}
flights %>% 
  filter(dest == "IAH") %>% 
  arrange(desc(dep_delay)) %>% 
  show_query()
```

#Functions
```{r "defention"}
f_name = function(a,b) {
  a-3*b
}
f_name(5,6)
```

```{r "example: creating a tibble dataframe"}
df1 = tibble(
  a = c(rnorm(10), NA, Inf),
  b = c(rnorm(10), NA, Inf),
  c = c(rnorm(10), NA, Inf), 
  d = c(rnorm(10), NA, Inf)
)
```

Q: Scale each column having numbers between 0 and 1.
```{r}
rng = range(df1$a, na.rm = T, finite = T)
x = df1$a
(x-rng[1]) / (rng[2]-rng[1])
```

```{r "build a rescaling function"}
rescale01 = function(x){
  rng = range(x, na.rm=T, finite=T) 
  (x-rng[1]) / (rng[2]-rng[1])
}
rescale01(df1$a) 
rescale01(df1$b)
rescale01(df1$c)
rescale01(df1$d)
```

```{r "basic looping"}
df2 = df1 
for(x in seq_along(df1)){
  df2[[x]] = rescale01(df1[[x]])
}
df3 = apply(df1, 2, rescale01) 
df3
```

```{r "more in function"}
z_score = function(x){
  x = x[!is.infinite(x)]
  (x - mean(x, na.rm = T))/sd(x, na.rm = T)
} 

x = df1$a 
z_score(x)
```

```{r "dataframe function"}
gp_means = function(df, gp_var, avg_var){
  df %>% 
    group_by({{gp_var}}) %>% 
    summarise(
      average = mean({{avg_var}})
    )
}

diamonds %>% 
  gp_means(color, carat) %>% 
  arrange(desc(average))
```

```{r "Let's practice more embracing"}
summary_stat = function(df, v){ 
  df %>% 
    summarise(
      minimum = min({{v}}, na.rm = T), 
      maximum = max({{v}}, na.rm = T), 
      average = mean({{v}}, na.rm = T), 
      median = median({{v}}, na.rm = T), 
      n = n(), 
      n_miss = sum(is.na({{v}})), 
      .groups = "drop"
    )}
diamonds %>% 
  group_by(color) %>% 
  summary_stat(price) 
```

```{r}
count_stat = function(df, v, sort = T){ 
  df %>% 
    count({{v}}, sort = sort) %>% 
    mutate(
      percentage = paste0(round(n/sum(n)*100,1),"%")
    )
} 
diamonds %>% count_stat(clarity)
```

* Find the sorted unique values of a variable for a subset of the data:
  ```{r}
#Find all the destinations in Dec
unique_destination = function(df, condtion, v){ 
  df %>% 
    filter({{condtion}}) %>% 
    distinct({{v}}) %>% 
    arrange({{v}})}
flights %>% 
  unique_destination(month == 12, dest)
```

```{r}
#We want to find the missing obs in departure time for flights per day 
count_missing <- function(df, group_vars, x_var) {
  df %>%  
    group_by(pick({{ group_vars }})) %>% 
    summarize(
      n_miss = sum(is.na({{ x_var }})),
      .groups = "drop"
    )
}

flights %>%  
  count_missing(c(year, month, day), dep_time)
```

* 2D table of counts: 
  - group: clarity and color
- Tgt var: cut

```{r}
count_2d <- function(df, rows, columns){
  df %>%  
    count(pick(c({{rows}}, {{columns}}))) %>% 
    pivot_wider(
      names_from = {{columns}}, 
      values_from = n
    )
}

diamonds %>% count_2d(c(clarity, color), cut)
```

* Plot functions
```{r}
pareto_chart <- function(df, v){
  df %>% 
    mutate({{v}}:= fct_infreq({{v}})) %>% 
    ggplot(aes(x = {{v}})) + 
    geom_bar()
}
diamonds %>% pareto_chart(color)
```

* Further study for `across` function:
  ```{r}
gp_means = function(df, gp_v, tgt_v = where(is.numeric)){
  df %>% 
    group_by({{gp_v}}) %>% 
    summarise(
      across({{tgt_v}}, list(
        avg = ~ mean(.x, na.rm=T),
        SD = function(x) sd(x, na.rm=T),
        n_miss = ~ sum(is.na(.x))
      )               
      ),
      n=n()
    )
}

diamonds %>% gp_means(cut, c(carat, price))
```

* Connection between `across` and `pivot_` functions
```{r}
df %>% 
  pivot_longer(a:d) %>% 
  group_by(name) %>% 
  summarise(
    avg = mean(value, na.rm = T),
    SD  = sd(value, na.rm = T),
    n_miss = sum(is.na(value))) %>% 
  pivot_wider(
    names_from = name,
    values_from = c(avg, SD, n_miss),
    names_vary = "slowest", 
    names_glue = "{name}_{.vale}"
  )
```

* Apply functions
```{r}
(df1 = matrix(c(1:10, 11:20, 21:30), nrow = 10, ncol = 3))
df2 = as.data.frame(df1) 

df2 = tibble(
  a = 1:10,
  b = seq(11,20), 
  c = 21:30
) 
apply(df2, 2, mean)
apply(df2, 2, function(x) x**2)

avg_std.err = function(x){
  c(avg = mean(x, na.rm=T),
    std.err = sd(x, na.rm=T)/sqrt(length(x)))
}
apply(df2, 2, avg_std.err)
```

```{r}
df3 = list(
  a = 1:10, 
  b = exp(-3:3), 
  c = c(T, F, T, F)
)
lapply(df3, quantile)
sapply(df3, quantile)
```

* Tapply function 
```{r}
df2 = cbind(df2, gp=sample(1:2, 10, replace = T))
tapply(df2$a, df2$gp, avg_std.err)

df2 %>% 
  group_by(gp) %>% 
  summarise(
    across(a:c, list(
      avg = function(x) mean(x, na.rm = T), 
      SD = ~ sd(.x, na.rm=T), 
      median = function(x) median(x, na.rm=T)
    ))
  )
```

* Using `lapply` function on list of models
```{r}
(df1 = tibble(
  x = seq(1,10), 
  y = sin(x)^2
))

model_list = list(
  model1 = lm(y ~ x, data=df1),
  model2 = lm(y ~ x + I(x^2), data = df1)
)

sapply(model_list, AIC)

do.call(rbind,
        lapply(names(model_list), function(x)
          data.frame(
            model = x, 
            AIC = AIC(model_list[[x]]), 
            AICc = AICc(model_list[[x]]), 
            p_val = summary(model_list[[x]])$cofficients[,"Pr(>|t|"],
            r_sq = summary(model_list[[x]])$r.squared
          )
        )
)
```

* Univariate analysis
```{r}
df3 = read.csv("https://stats.idre.ucla.edu/stat/data/hsb2.csv")
var_list = names(df3)[8:11]

models = lapply(var_list, function(x){
  lm(substitute(read ~ i, list(i = as.name(x))), data = df3)
})
for(i in 1:4){
  p_val = lapply(models, summary)[[i]]$coefficients[2,4]
  print(p_val)
}

#Alternatively, we can do without for loop
tbl_pval = do.call(
  rbind.data.frame,
  lapply(models, function(x){
    summary(x)$coefficients[2,4]
  })
)
names(tbl_pval)="p_val"
```

```{r}
df1 = Private.Facility.Outpatient.Data %>% drop_na()

df2 = df1 %>% 
  mutate(
    patient_outcome = 
      factor(ifelse(`Outcome.score` >= 1.5, "Good", "Bad")), 
    age_gp = factor(ifelse(Age > 60, 1, 0)),
    Sex = factor(Sex),
    Ethnicity = factor(Ethnicity),
    Eth_gp = fct_collapse(Ethnicity, "Others" = c("Asian", "Indian", "Polynesian"))
  )

df2 %$%  table(patient_outcome, Sex)
df2 %$%  table(patient_outcome, Eth_gp)
df2 %$%  table(patient_outcome, age_gp)

df2 %>% select(Sex, Eth_gp, age_gp) %>% 
  lapply(function(x){
    df2 %$%  table(patient_outcome, x)
  })
```

* Demographics table for publication using `tbl_summary` function
```{r}
df2 %>% 
  select(Age, Sex, Ethnicity, patient_outcome) %>% 
  gtsummary::tbl_summary(
    by = patient_outcome, 
    statistic = list(
      all_continuous() ~ "{mean} ({sd})" 
    ), 
    digits = list(
      all_continuous() ~ 1
    )
  ) %>%  
  add_p(pvalue_fun = ~ style_pvalue(.x, digits =2)) %>% 
  add_overall() %>% 
  modify_header(label ~ "**Factor**") %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ 
                           "**Patient Outcome**") %>% 
  bold_labels() %>% 
  as_gt() %>% 
  gt::gtsave(filename="Tabeldemo.docx")
```
