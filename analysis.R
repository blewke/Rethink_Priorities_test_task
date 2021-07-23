library(gsheet)
library(tidyverse)
library(GGally)
library(psych)


#### load and clean the data ####


url = 'https://docs.google.com/spreadsheets/d/1pdWo_Q4FyHkL_E12zrp63HQA3txPs4TNGP2MgvO3Kfk/edit#gid=0'

# extract the data from the google sheet
dat = as_tibble(gsheet2tbl(url))

summary(dat)


##make the column headings more convenient
#convert to lower case
colnames(dat) = lapply(colnames(dat), tolower)

#replace white space with _
colnames(dat) = gsub(" ", "_", colnames(dat))


### get rid of the text in the numbered responses

dat$engagement_int = substr(dat$engagement, 2,2)

dat[dat == '(1) Not at all important'] = '1'
dat[dat == '(7) Highly important'] = '7'





###### make the pairs plot ####


### a little helper function to make a heatmap
my_bin <- function(data, mapping, ..., low = 'blue', high = 'yellow') {
  ggplot(data = data, mapping = mapping) +
    geom_bin2d(...) +
    scale_fill_gradient(low = low, high = high)
}

### make the pairs plot
ggpairs(dat[3:8],
        upper = list(discrete = my_bin),
        diag = list(discrete = "barDiag"),
        lower  = NULL,
        legend = c(1,4)
        )



###### factor analysis ####


#convert to integers

dat$global_poverty = as.integer(dat$global_poverty)
dat$homelessness = as.integer(dat$homelessness)
dat$mental_health = as.integer(dat$mental_health)
dat$asteroid_collisions = as.integer(dat$asteroid_collisions)
dat$nuclear_warfare = as.integer(dat$nuclear_warfare)
dat$pandemic_preparedness = as.integer(dat$pandemic_preparedness)
dat$engagement_int = as.integer(dat$engagement_int)



# adapted from https://alice86.github.io/2018/04/08/Factor-Analysis-on-Ordinal-Data-example-in-R-(psych,-homals)/

#compute correlations for ordinal data
poly_cor = polychoric(dat[3:8])
rho = poly_cor$rho

#correlations
cor.plot(poly_cor$rho, numbers = T, upper = FALSE, main = "Polychoric Correlation", show.legend = FALSE)

# look at the scree plot to see how many factors should be used
fa.parallel(rho, n.obs = 2000, fm = "pa", fa = "fa", main = "Scree Plot")


#I choose to use only one factor, not 2
poly_model = fa(dat[3:8], nfactor=1, cor="poly", fm="mle", rotate = "none")

# look at the factor loadings
poly_model$loadings

# compute the score on this dimenstion for every respondent and append it to the data frame
dat$present_orientedness = poly_model$scores



#### plots for charactersitics vs present-orientedness


ggplot(dat) +
  geom_density(aes(x = present_orientedness, fill = region), alpha = 0.5)

dat$student = ifelse(dat$student == '0', 'No', 'Yes') 

ggplot(dat) +
  geom_density(aes(x = present_orientedness, fill = student), alpha = 0.5)

ggplot(dat) +
  geom_density(aes(x = present_orientedness, fill = engagement), alpha = 0.5)

ggplot(dat) +
  geom_density(aes(x = present_orientedness, fill = when_joined_ea), alpha = 0.5)


# plot when joined and engagement
ggplot(dat) +
  geom_bar(aes(x = when_joined_ea, fill = engagement))
  
