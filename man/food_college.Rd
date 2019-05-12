\name{food_college}
\alias{food_college}
\docType{data}
\title{
Nutrition and Food Preferences Among College Students
}
\description{
This dataset includes information on food choices, nutrition, preferences, childhood favorites, and other information from college students. There are 126 responses from students. Data is raw and uncleaned. Cleaning is in the process and as soon as that is done, additional versions of the data will be posted. Acknowledgements

Thank you to all the students of Mercyhurst University who agreed to participate in this survey.

The survey questions are given towards the bottom of this documentation.
}
\usage{data("food_college")}
\format{
  A data frame with 125 observations on the following 61 variables.
  \describe{
    \item{\code{GPA}}{a numeric vector}
    \item{\code{Gender}}{a factor with levels \code{1} \code{2}}
    \item{\code{breakfast}}{a factor with levels \code{1} \code{2}}
    \item{\code{calories_chicken}}{a numeric vector}
    \item{\code{calories_day}}{a numeric vector}
    \item{\code{calories_scone}}{a numeric vector}
    \item{\code{coffee}}{a factor with levels \code{1} \code{2}}
    \item{\code{comfort_food}}{a character vector}
    \item{\code{comfort_food_reasons}}{a character vector}
    \item{\code{comfort_food_reasons_coded}}{a numeric vector}
    \item{\code{cook}}{a numeric vector}
    \item{\code{comfort_food_reasons_coded_1}}{a numeric vector}
    \item{\code{cuisine}}{a numeric vector}
    \item{\code{diet_current}}{a character vector}
    \item{\code{diet_current_coded}}{a numeric vector}
    \item{\code{drink}}{a numeric vector}
    \item{\code{eating_changes}}{a character vector}
    \item{\code{eating_changes_coded}}{a numeric vector}
    \item{\code{eating_changes_coded1}}{a numeric vector}
    \item{\code{eating_out}}{a numeric vector}
    \item{\code{employment}}{a numeric vector}
    \item{\code{ethnic_food}}{a numeric vector}
    \item{\code{exercise}}{a numeric vector}
    \item{\code{father_education}}{a numeric vector}
    \item{\code{father_profession}}{a character vector}
    \item{\code{fav_cuisine}}{a character vector}
    \item{\code{fav_cuisine_coded}}{a numeric vector}
    \item{\code{fav_food}}{a numeric vector}
    \item{\code{food_childhood}}{a character vector}
    \item{\code{fries}}{a numeric vector}
    \item{\code{fruit_day}}{a numeric vector}
    \item{\code{grade_level}}{a numeric vector}
    \item{\code{greek_food}}{a numeric vector}
    \item{\code{healthy_feeling}}{a numeric vector}
    \item{\code{healthy_meal}}{a character vector}
    \item{\code{ideal_diet}}{a character vector}
    \item{\code{ideal_diet_coded}}{a numeric vector}
    \item{\code{income}}{a numeric vector}
    \item{\code{indian_food}}{a numeric vector}
    \item{\code{italian_food}}{a numeric vector}
    \item{\code{life_rewarding}}{a numeric vector}
    \item{\code{marital_status}}{a numeric vector}
    \item{\code{meals_dinner_friend}}{a character vector}
    \item{\code{mother_education}}{a numeric vector}
    \item{\code{mother_profession}}{a character vector}
    \item{\code{nutritional_check}}{a numeric vector}
    \item{\code{on_off_campus}}{a numeric vector}
    \item{\code{parents_cook}}{a numeric vector}
    \item{\code{pay_meal_out}}{a numeric vector}
    \item{\code{persian_food}}{a numeric vector}
    \item{\code{self_perception_weight}}{a numeric vector}
    \item{\code{soup}}{a numeric vector}
    \item{\code{sports}}{a numeric vector}
    \item{\code{thai_food}}{a numeric vector}
    \item{\code{tortilla_calories}}{a numeric vector}
    \item{\code{turkey_calories}}{a numeric vector}
    \item{\code{type_sports}}{a character vector}
    \item{\code{veggies_day}}{a numeric vector}
    \item{\code{vitamins}}{a numeric vector}
    \item{\code{waffle_calories}}{a numeric vector}
    \item{\code{weight}}{a numeric vector}
  }
}
\details{
1) GPA – numerical, actual GPA

2) Gender categorical with two levels Female and Male

3) Breakfast (the participants are shown the following pictures and asked which one of these pictures they associate with the word “breakfast), 1 – cereal option and 2 – donut option

4) calories_chicken - guessing calories in chicken piadina:
1 - 265
2 - 430
3 - 610
4 – 720
(the variable shows the actual number of calories participants selected)


5) calories_day - Importance of consuming calories per day
1 - i dont know how many calories i should consume
2 - it is not at all important
3 - it is moderately important
4 - it is very important

6) calories_scone  - Guessing calories in a scone from starbucks
1 - 107 cal
2 - 315 cal
3 - 420 cal
4 - 980 cal
(the variable shows the actual number of calories participants selected)

7) coffee – which of the two pictures you associate with the word coffee?
1 – creamy frapuccino
2 – espresso shown




8) comfort_food - List 3-5 comfort foods that come to mind.
Open ended (perfect for NLP)

9) comfort_food_reasons - What are some of the reasons that make you eat comfort food? (i.e., anger, sadness, happiness, boredom, etc) - list up to three
Open ended (perfect for NLP)

10) comfort_food_reasons_coded
(the one mentioned first)

1 – stress
2 – boredom
3 – depression/sadness
4 – hunger
5 – laziness
6 – cold weather
7 – happiness
8- watching tv
9 – none



11) cook – how often do you cook?
1 - Every day
2 - A couple of times a week
3 - Whenever I can, but that is not very often
4 - I only help a little during holidays
5 - Never, I really do not know my way around a kitchen

12) cuisine – what type of cuisine did you eat growing up?
1 – American
2 – Mexican.Spanish
3 – Korean/Asian
4 – Indian
5 – American inspired international dishes
6 – other

(lots of cleaning needed for this variable)

13) diet_current – describe your current diet
open ended – ideal for NLP


14) diet_current_coded
(based on words used to describe the diet)

1 – healthy/balanced/moderated/
2 – unhealthy/cheap/too much/random/
3 – the same thing over and over
4 – unclear


15) which picture do you associate with the word “drink”?
1 – orange juice
2 – soda





16) eating_changes  - Describe your eating changes since the moment you got into college?
Open ended

17) eating_changes_coded

1 – worse
2 – better
3 – the same
4 – unclear


18) eating_changes_coded1
1 – eat faster
2 – bigger quantity
3 – worse quality
4 – same food
5 – healthier
6 – unclear
7 – drink coffee
8 – less food
9 – more sweets
10 – timing
11 – more carbs or snacking
12 – drink more water
13 – more variety


19) eating_out - frequency of eating out in a typical week
1 - Never
2 - 1-2 times
3 - 2-3 times
4 - 3-5 times
5 - every day


20) employment – do you work?
1 - yes full time
2 - yes part time
3 – no
4  - other


21) ethnic_food - How likely to eat ethnic food
1 - very unlikely
2 - unlikely
3 - neutral
4 - likely
5 - very likely

22) exercise – how often do you exercise in a regular week?
1 - Everyday
2 - Twice or three times per week
3 - Once a week
4 - Sometimes
5 – Never

23) father_education –
1 - less than high school
2 - high school degree
3 - some college degree
4 - college degree
5 - graduate degree


24) father_profession – what is your father profession?
Open ended

25) fav_cuisine - What is your favorite cuisine?
Open ended

26) fav_cuisine_coded

0-none
1 – Italian/French/greek
2 – Spanish/mexican
3 – Arabic/Turkish
4 – asian/chineses/thai/nepal
5 – American
6 – African
7 – Jamaican
8 – indian


27) fav_food - was your favorite food cooked at home or store bought?
1 - cooked at home
2 - store bought
3 - both bought at store and cooked at home

28) food_childhood – what was your favorite childhood food?
Open ended


29) which of these pictures you associate with word fries?
1 – Mcdonald’s fries
2 – home fries




30) fruit_day - How likely to eat fruit in a regular day
1 - very unlikely
2 - unlikely
3 - neutral
4 - likely
5 - very likely


31) grade_level –
1 - freshman
2 -Sophomore
3 - Junior
4 - Senior

32) greek_food - How likely to eat greek food when available?
1 - very unlikely
2 - unlikely
3 - neutral
4 - likely
5 - very likely

33) healthy_feel – how likely are you to agree with the following statement: “I feel very healthy!” ?
1 to 10 where 1 is strongly agree and 10 is strongly disagree - scale

34) healthy_meal – what is a healthy meal? Describe in 2-3 sentences.
Open ended

35) ideal_diet – describe your ideal diet in 2-3 sentences
Open ended

36) Ideal_diet_coded

1 – portion control
2 – adding veggies/eating healthier food/adding fruit
3 – balance
4 – less sugar
5 – home cooked/organic
6 – current diet
7 – more protein
8 – unclear

37) income
1 - less than $15,000
2 - $15,001 to $30,000
3 - $30,001 to $50,000
4 - $50,001 to $70,000
5 - $70,001 to $100,000
6 - higher than $100,000

38) indian_food – how likely are you to eat indian food when available
1 - very unlikely
2 - unlikely
3 - neutral
4 - likely
5 - very likely


39) Italian_food – how likely are you to eat Italian food when available?

1 - very unlikely
2 - unlikely
3 - neutral
4 - likely
5 - very likely

40) life_rewarding – how likely are you to agree with the following statement: “I feel life is very rewarding!” ?
1 to 10 where 1 is strongly agree and 10 is strongly disagree - scale

41) marital_status
1 -Single
2 - In a relationship
3 - Cohabiting
4 - Married
5 - Divorced
6 - Widowed


42) meals_dinner_friend – What would you serve to a friend for dinner?
Open ended

43) mothers_education
1 - less than high school
2 - high school degree
3 - some college degree
4 - college degree
5 - graduate degree

44) mothers_profession – what is your mother’s profession?

45) nutritional_check - checking nutritional values frequency
1 - never
2 - on certain products only
3 - very rarely
4 - on most products
5 - on everything

46) on_off_campus – living situation
1 - On campus
2 - Rent out of campus
3 - Live with my parents and commute
4 - Own my own house

47) parents_cook - Approximately how many days a week did your parents cook?
1 - Almost everyday
2 - 2-3 times a week
3 - 1-2 times a week
4 - on holidays only
5 - never

48) pay_meal_out - How much would you pay for meal out?
1 - up to $5.00
2 - $5.01 to $10.00
3 - $10.01 to $20.00
4 - $20.01 to $30.00
5 - $30.01 to $40.00
6 - more than $40.01

49) Persian_food - How likely to eat persian food when available?
1 - very unlikely
2 - unlikely
3 - neutral
4 - likely
5 - very likely

50) self_perception_weight - self perception of weight
6 - i dont think myself in these terms
5 - overweight
4 - slightly overweight
3 - just right
2 - very fit
1 - slim

51) Which of the two pictures you associate with the word soup?
1 – veggie soup
2 – creamy soup





52) sports - sports – do you do any sporting activity?

1 - Yes
2 - No
99 – no answer

53) thai_food - How likely to eat thai food when available?
1 - very unlikely
2 - unlikely
3 - neutral
4 - likely
5 - very likely

54) tortilla_calories - guessing calories in a burrito sandwhich from Chipolte?

1 - 580
2 - 725
3 - 940
4 - 1165

55) turkey_calories - Can you guess how many calories are in the foods shown below? (Panera Bread Roasted Turkey and Avocado BLT)

1 - 345
2 - 500
3 - 690
4 - 850


56) type_sports – what type of sports are you involved?
Open-ended

57) veggies_day - How likely to eat veggies in a day?
1 - very unlikely
2 - unlikely
3 - neutral
4- likely
5 - very likely

58) vitamins – do you take any supplements or vitamins?
1 – yes
2 – no

59) waffle_calories - guessing calories in waffle potato sandwhich
1 - 575
2 - 760
3 - 900
4 - 1315

60) weight – what is your weight in pounds?

}
\source{
https://www.kaggle.com/borapajo/food-choices/home
}
\references{
The full survey can be found here:

https://www.kaggle.com/borapajo/food-choices/downloads/codebook_food.docx/5
}
\examples{
data(food_college)
}
\keyword{datasets}
