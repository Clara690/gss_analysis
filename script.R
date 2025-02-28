library(tidyverse)
library(socviz)
library(dplyr)
library(modelsummary)
library(broom)
library(scales)
library(DescTools)
library(lmtest)
library(gt) # for makeing graph and table
```

```{r}
#| echo: false
#| warning: false
# load the data
gss <- gss_sm
# str(gss)
```

```{r}
#| echo: false
#| warning: false
# handling religios categories 
gss <- gss %>% 
  mutate(religion = case_when(
    religion == 'Jewish' ~ 'Other',
    TRUE ~ as.factor(religion)
  ))
# turn 'religion' into a factor variable
gss$religion <- as.factor(gss$religion)

# forceing R to use 'None' as reference level
gss <- within(gss, religion <- relevel(religion, ref = 'None'))

# handling income variables
gss <- gss %>% 
  mutate(
    income_rc_char = as.character(income_rc),  # turn the factor into character 
    income_rc_char = na_if(income_rc_char, "NA"),  # keep NAs
    income_rc_char = as.numeric(str_remove_all(income_rc_char, "[$,Gt ]"))
  )  %>%
  mutate(income_rc = case_when(
    income_rc_char < 25000 ~ 'low',
    income_rc_char < 130000 ~ 'middle',
    income_rc_char > 130000 ~ 'high'
  ))
# extracting all the categories (for checking)
# for (i in as.factor(unique(gss$income_rc))){
#   print(i)
# }
# for (j in as.factor(unique(gss$religion))){
#   print(j)
# }

```

# Descriptive Table on Sample Attributes

The original dataset contains 33 variables and 2867 observations. However, after ruling out obervations with missing values, there are 1522 obervations and 4 attributes, sex, income, religion and age are used to conduct this analysis. The religion variable contains 4 categories, with Jewish being clustered into `Other`. The income variable is a nominal variable with three levels. Individuals are labeled based on the reported income. The table illustrates individual level attributes and the results are group by their votes. Approximately 63% of the observations voted for Obama and on all recorded attributes, the number of individuals voting for Obama is higher than that of those who did not. Interestingly, the ratios of sex among non-Obama voters is rather balanced while among Obama voters, we see \~ 12% more female voters.

```{r}
#| echo: false
#| warning: false
# subset the data and remove observations with missing values 
gss_filtered <- gss %>% 
  select(c(age, sex, religion, income_rc, obama)) %>% 
  drop_na()

# rename the column
gss_filtered <- rename(gss_filtered, income = income_rc)

# creating a table
datasummary((`Age` = age) + (`Sex` = sex) + 
              (`Religion` = religion) + (`Income` = income) + 1 
            ~ obama * (mean + SD + Percent()), 
            data = gss_filtered %>% 
              mutate(obama = factor(obama, levels = c(0, 1),
                                    labels = c("Non-Obama Voters", "Obama Voters"))))
```

# Logistic Regression

In this section, I constructed three logistic regression models. Firstly, in Model A, there is religion as the only variable. None is used as the reference category, and this is the same for Model B and Model C. Figures in the table is presented in odd ratios. At first glance, it seems that both Catholic and Protestant are less likely to vote for Obama compared to individuals without any religious affiliation.

In Model B, income is added as another independent variable. The coefficient for Catholic remains the same as in Model A but the coefficient for Protestant decreases slightly from 0.28 to 0.26. Individuals with low and middle income are more likely to vote for Obama compared to individuals with high income.

Finally, I added sex and age into the model and the results are shown under the column Model C. Similar to the results obtained from Model B, the coefficient does not change at all for Catholic. The coefficient for Protestant and dropped slightly while the coefficients for individuals with both income categories increased. The increase is noticeable with individuals with low income. Compared to men, women are more likely to vote for Obama. As for the age effect of voting for Obama, for every one-year increase in age, one becomes less likely to vote for Obama.

```{r}
#| echo: false
#| warning: false
# model a, only religion as an independent variable
md_a <- glm(obama ~ religion, data = gss_filtered, 
            family = 'binomial'(link = 'logit'))
# add household income as an independent variable
md_b <- glm(obama ~ religion + income, data = gss_filtered,
            family = 'binomial'(link = 'logit'))
# add sex and age as independent variables
md_c <- glm(obama ~ religion + income + sex + age, 
            data = gss_filtered, 
            family = 'binomial'(link = 'logit'))
md_lst <- list('Model A'= md_a, 'Model B' = md_b, 'Model C' = md_c)


# making a table containing OR and conf int
modelsummary(md_lst, fmt = 2,
             stars = TRUE,
             exponentiate = TRUE,
             coef_rename = c('Intercept', 'Catholic', 'Other',
                             'Protestant', 'Low Income',
                             'Middle Income', 'Female', 'Age'),
             gof_omit = 'F|RMSE',
             title = 'Probabillity of Voting for Obama',
             notes = list('Income','low: < $25000', 'middle: $25000~ $129999', 'high: > 130000'))

# a graph illustrating the OR from model 3
estimates <- tidy(md_c, exponentiate = TRUE, conf.int = TRUE)
estimates %>% 
  filter(term != '(Intercept)') %>% 
  mutate(term = recode(term, 
                       'religionCatholic' = 'Catholic',
                       'religionOther'= 'Other',
                       'religionProtestant' = 'Protestant',
                       'incomelow' = 'Low Income',
                       'incomemiddle' = 'Middle Income',
                       'sexFemale' = 'Female',
                       'age' = 'Age')) %>% 
  ggplot(aes(term, estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, width = .1)) + labs(title = 'Figure 1:  Estimated Odds Ratios from Model C',
                                                                           y = 'Odds Ratio', x = 'Variables') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

# Predicted Probability

For the purspose of demonstration, `Figure 2` shows the predicted values of a female with middle income voting for Obama. The overall tread reamins roughly consistant for all religion categories. Older females are less likely to vote for Obama, and among all religions, Protestant are least likely to vote for Obama. As mentioned earlier, religion might be a proxy for one's political ideology. Take Christianity for example, Catholics are relatively more on the conservative end while Protestants are more on the liberal end. Thus, it seems completely reasonalble to anticipate that being a Protestant makes someone more likely to vote for Obama. However, the predicted results show the opposite.

```{r}
#| echo: false
#| warning: false
#| message: false
library(modelr)
# predicted probability of a female with middle income voting for Obama
# get all combinations type = df
female_mid <- expand_grid(religion = c('None', 'Catholic', 
                         'Protestant', 'Other'),
            age = seq(21, 89),
            sex = 'Female',
            income = 'middle') 
# make the prediction &
predicted_values <- predict(md_c, newdata = female_mid, 
                      type = 'response', se.fit = TRUE)

# combine the predicted values with the origianl df
female_mid <- bind_cols(female_mid, as_tibble(predicted_values))

female_mid %>% 
  mutate(lower = fit - 1.96 * se.fit,
         upper = fit + 1.96 * se.fit) %>% 
ggplot(aes(x= age, y = fit, color = religion, fill = religion)) + 
  geom_line() +
  scale_y_continuous(labels = percent_format()) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1) +
  labs(title = 'Figure 2: Probability of A Female with Middle Income
Voting for Obama ',
       subtitle = 'Results from Model C',
       x = 'Age',
       y = 'Predicted Value',
       caption = 'The shadow around the line indicats the confidence interval')

```

# Model Fit

In total, there are three models in this article, Model A, Model B and Model C. Model A is nested in Model B and Model B is nested in Model C. In the following section, I will examine the goodness of fit of the models by looking at three aspects, namely **likelihood ratio test**, **Negelkerke's pseudo** $R^2$ and **the share of observations correctly predicted by the models**.

## Likelihood Ratio Test

The test is based on comapring two model and the the model being compared is the one that entails the other. The null hypothese is the two model being compared are the same in terms of their ability to predict the data. When comparing Model B to Model A and Model C to Model B, both results are statistically significant, indicating that the former is better than the latter.

## Pseudo $R^2$

The idea of Rsedo $R^2$ is similar to $R^2$ used to evaluate linear regression model. There are more than one type of pseudo $R^2$ and the pseudo $R^2$ used in this article is the one by Nagelkerke. The value can range from 0 to 1, with higher valuing suggesting better performance of the model on prediction. In this study, Model C has the highest score among the three models.

## Share of Correct Prediction

Share of correct predition is calculated as the following equation shows:\
$P = \frac{the number of correct prediction}{total of observation}$\
Model C has the best performance on predicting outcomes.

```{r}
#| echo: false
#| warning: false
#| message: false
# likelihood ratio test
# compare model b to model a
lr_a_to_b <- lrtest(md_a, md_b)
# compare model c to model b
lr_b_to_c <- lrtest(md_b, md_c)

# Negelkerke's pseuso's R^2
pse_a <- round(PseudoR2(md_a, which = 'Nagelkerke'), 2)
pse_b <- round(PseudoR2(md_b, which = 'Nagelkerke'), 2)
pse_c <- round(PseudoR2(md_c, which = 'Nagelkerke'), 2)

#the share of observations correctly predicted 
# Model A
md_a_pred <- augment(md_a,
                     type.predict = 'response') %>% 
  mutate(.pred = .fitted > 0.5)
share_md_a <- round(mean(md_a_pred$obama 
                         == md_a_pred$.pred) * 100, 2)
# Model B
md_b_pred <- augment(md_b,
                     type.predict = 'response') %>% 
  mutate(.pred =.fitted > 0.5)
share_md_b <- round(mean(md_b_pred$obama 
                         == md_b_pred$.pred) * 100, 2)
# Model C
md_c_pred <- augment(md_c,
                     type.predict = 'response') %>% 
  mutate(.pred = .fitted > 0.5)
share_md_c <- round(mean(md_c_pred$obama 
                         == md_c_pred$.pred) * 100, 2)

# table
results <- tibble('term' = c('Likelihood Ratio Test', 
                             'Pseuso R²', 
                             'Share of Correct Prediction (%)'), 
                  'Model A' = c(NA, pse_a, share_md_a),
                  'Model B' = c(lr_a_to_b$Chisq[2], pse_b,
                                share_md_b),
                  'Model C' = c(lr_b_to_c$Chisq[2], pse_c,
                                share_md_c))
results %>% 
  gt() %>% 
  tab_header(
    title = md('**Table 3** Model Evaluation')
  ) %>% 
  fmt_number(
    columns = c('Model A', 'Model B', 'Model C'),
    decimal = 2
  ) %>% 
  cols_label(
    term = 'Metrics',
    `Model A` = 'Model A',
    `Model B` = 'Model B',
    `Model C` = 'Model C'
  ) %>% 
  cols_align(
    align = 'center',
    columns = everything()
  ) %>% 
  tab_footnote('The Likelihood ratio test for Model is not applicable as it is the most basic model in this analysis.\n*** P < 0.01') %>% 
  text_transform(
    locations = cells_body(
      columns = c(`Model B`, `Model C`),
      rows = term == "Likelihood Ratio Test"
    ),
    fn = function(x) paste0(x, '***')