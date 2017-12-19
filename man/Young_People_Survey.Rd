\name{Young_People_Survey}
\alias{Young_People_Survey}
\docType{data}
\title{
Young People Survey Data
}
\description{
This data file contains the results of a lengthly survey of young people.  All participants were of Slovakian nationality, aged between 15-30. The data consists of 1010 rows and 150 columns (139 integer and 11 categorical)
The variables can be split into the following groups:

    Music preferences (19 items)
    Movie preferences (12 items)
    Hobbies & interests (32 items)
    Phobias (10 items)
    Health habits (3 items)
    Personality traits, views on life, & opinions (57 items)
    Spending habits (7 items)
    Demographics (10 items)
}
\usage{data("Young_People_Survey")}
\format{
  A data frame with 1010 observations on the following 150 variables.
  \describe{
    \item{\code{Music}}{a numeric vector}
    \item{\samp{Slow songs or fast songs}}{a numeric vector}
    \item{\code{Dance}}{a numeric vector}
    \item{\code{Folk}}{a numeric vector}
    \item{\code{Country}}{a numeric vector}
    \item{\samp{Classical music}}{a numeric vector}
    \item{\code{Musical}}{a numeric vector}
    \item{\code{Pop}}{a numeric vector}
    \item{\code{Rock}}{a numeric vector}
    \item{\samp{Metal or Hardrock}}{a numeric vector}
    \item{\code{Punk}}{a numeric vector}
    \item{\samp{Hiphop, Rap}}{a numeric vector}
    \item{\samp{Reggae, Ska}}{a numeric vector}
    \item{\samp{Swing, Jazz}}{a numeric vector}
    \item{\samp{Rock n roll}}{a numeric vector}
    \item{\code{Alternative}}{a numeric vector}
    \item{\code{Latino}}{a numeric vector}
    \item{\samp{Techno, Trance}}{a numeric vector}
    \item{\code{Opera}}{a numeric vector}
    \item{\code{Movies}}{a numeric vector}
    \item{\code{Horror}}{a numeric vector}
    \item{\code{Thriller}}{a numeric vector}
    \item{\code{Comedy}}{a numeric vector}
    \item{\code{Romantic}}{a numeric vector}
    \item{\samp{Sci-fi}}{a numeric vector}
    \item{\code{War}}{a numeric vector}
    \item{\samp{Fantasy/Fairy tales}}{a numeric vector}
    \item{\code{Animated}}{a numeric vector}
    \item{\code{Documentary}}{a numeric vector}
    \item{\code{Western}}{a numeric vector}
    \item{\code{Action}}{a numeric vector}
    \item{\code{History}}{a numeric vector}
    \item{\code{Psychology}}{a numeric vector}
    \item{\code{Politics}}{a numeric vector}
    \item{\code{Mathematics}}{a numeric vector}
    \item{\code{Physics}}{a numeric vector}
    \item{\code{Internet}}{a numeric vector}
    \item{\code{PC}}{a numeric vector}
    \item{\samp{Economy Management}}{a numeric vector}
    \item{\code{Biology}}{a numeric vector}
    \item{\code{Chemistry}}{a numeric vector}
    \item{\code{Reading}}{a numeric vector}
    \item{\code{Geography}}{a numeric vector}
    \item{\samp{Foreign languages}}{a numeric vector}
    \item{\code{Medicine}}{a numeric vector}
    \item{\code{Law}}{a numeric vector}
    \item{\code{Cars}}{a numeric vector}
    \item{\samp{Art exhibitions}}{a numeric vector}
    \item{\code{Religion}}{a numeric vector}
    \item{\samp{Countryside, outdoors}}{a numeric vector}
    \item{\code{Dancing}}{a numeric vector}
    \item{\samp{Musical instruments}}{a numeric vector}
    \item{\code{Writing}}{a numeric vector}
    \item{\samp{Passive sport}}{a numeric vector}
    \item{\samp{Active sport}}{a numeric vector}
    \item{\code{Gardening}}{a numeric vector}
    \item{\code{Celebrities}}{a numeric vector}
    \item{\code{Shopping}}{a numeric vector}
    \item{\samp{Science and technology}}{a numeric vector}
    \item{\code{Theatre}}{a numeric vector}
    \item{\samp{Fun with friends}}{a numeric vector}
    \item{\samp{Adrenaline sports}}{a numeric vector}
    \item{\code{Pets}}{a numeric vector}
    \item{\code{Flying}}{a numeric vector}
    \item{\code{Storm}}{a numeric vector}
    \item{\code{Darkness}}{a numeric vector}
    \item{\code{Heights}}{a numeric vector}
    \item{\code{Spiders}}{a numeric vector}
    \item{\code{Snakes}}{a numeric vector}
    \item{\code{Rats}}{a numeric vector}
    \item{\code{Ageing}}{a numeric vector}
    \item{\samp{Dangerous dogs}}{a numeric vector}
    \item{\samp{Fear of public speaking}}{a numeric vector}
    \item{\code{Smoking}}{a factor with levels \code{current smoker} \code{former smoker} \code{never smoked} \code{tried smoking}}
    \item{\code{Alcohol}}{a factor with levels \code{drink a lot} \code{never} \code{social drinker}}
    \item{\samp{Healthy eating}}{a numeric vector}
    \item{\samp{Daily events}}{a numeric vector}
    \item{\samp{Prioritising workload}}{a numeric vector}
    \item{\samp{Writing notes}}{a numeric vector}
    \item{\code{Workaholism}}{a numeric vector}
    \item{\samp{Thinking ahead}}{a numeric vector}
    \item{\samp{Final judgement}}{a numeric vector}
    \item{\code{Reliability}}{a numeric vector}
    \item{\samp{Keeping promises}}{a numeric vector}
    \item{\samp{Loss of interest}}{a numeric vector}
    \item{\samp{Friends versus money}}{a numeric vector}
    \item{\code{Funniness}}{a numeric vector}
    \item{\code{Fake}}{a numeric vector}
    \item{\samp{Criminal damage}}{a numeric vector}
    \item{\samp{Decision making}}{a numeric vector}
    \item{\code{Elections}}{a numeric vector}
    \item{\samp{Self-criticism}}{a numeric vector}
    \item{\samp{Judgment calls}}{a numeric vector}
    \item{\code{Hypochondria}}{a numeric vector}
    \item{\code{Empathy}}{a numeric vector}
    \item{\samp{Eating to survive}}{a numeric vector}
    \item{\code{Giving}}{a numeric vector}
    \item{\samp{Compassion to animals}}{a numeric vector}
    \item{\samp{Borrowed stuff}}{a numeric vector}
    \item{\code{Loneliness}}{a numeric vector}
    \item{\samp{Cheating in school}}{a numeric vector}
    \item{\code{Health}}{a numeric vector}
    \item{\samp{Changing the past}}{a numeric vector}
    \item{\code{God}}{a numeric vector}
    \item{\code{Dreams}}{a numeric vector}
    \item{\code{Charity}}{a numeric vector}
    \item{\samp{Number of friends}}{a numeric vector}
    \item{\code{Punctuality}}{a factor with levels \code{i am always on time} \code{i am often early} \code{i am often running late}}
    \item{\code{Lying}}{a factor with levels \code{everytime it suits me} \code{never} \code{only to avoid hurting someone} \code{sometimes}}
    \item{\code{Waiting}}{a numeric vector}
    \item{\samp{New environment}}{a numeric vector}
    \item{\samp{Mood swings}}{a numeric vector}
    \item{\samp{Appearence and gestures}}{a numeric vector}
    \item{\code{Socializing}}{a numeric vector}
    \item{\code{Achievements}}{a numeric vector}
    \item{\samp{Responding to a serious letter}}{a numeric vector}
    \item{\code{Children}}{a numeric vector}
    \item{\code{Assertiveness}}{a numeric vector}
    \item{\samp{Getting angry}}{a numeric vector}
    \item{\samp{Knowing the right people}}{a numeric vector}
    \item{\samp{Public speaking}}{a numeric vector}
    \item{\code{Unpopularity}}{a numeric vector}
    \item{\samp{Life struggles}}{a numeric vector}
    \item{\samp{Happiness in life}}{a numeric vector}
    \item{\samp{Energy levels}}{a numeric vector}
    \item{\samp{Small - big dogs}}{a numeric vector}
    \item{\code{Personality}}{a numeric vector}
    \item{\samp{Finding lost valuables}}{a numeric vector}
    \item{\samp{Getting up}}{a numeric vector}
    \item{\samp{Interests or hobbies}}{a numeric vector}
    \item{\samp{Parents' advice}}{a numeric vector}
    \item{\samp{Questionnaires or polls}}{a numeric vector}
    \item{\samp{Internet usage}}{a factor with levels \code{few hours a day} \code{less than an hour a day} \code{most of the day} \code{no time at all}}
    \item{\code{Finances}}{a numeric vector}
    \item{\samp{Shopping centres}}{a numeric vector}
    \item{\samp{Branded clothing}}{a numeric vector}
    \item{\samp{Entertainment spending}}{a numeric vector}
    \item{\samp{Spending on looks}}{a numeric vector}
    \item{\samp{Spending on gadgets}}{a numeric vector}
    \item{\samp{Spending on healthy eating}}{a numeric vector}
    \item{\code{Age}}{a numeric vector}
    \item{\code{Height}}{a numeric vector}
    \item{\code{Weight}}{a numeric vector}
    \item{\samp{Number of siblings}}{a numeric vector}
    \item{\code{Gender}}{a factor with levels \code{female} \code{male}}
    \item{\samp{Left - right handed}}{a factor with levels \code{left handed} \code{right handed}}
    \item{\code{Education}}{a factor with levels \code{college/bachelor degree} \code{currently a primary school pupil} \code{doctorate degree} \code{masters degree} \code{primary school} \code{secondary school}}
    \item{\samp{Only child}}{a factor with levels \code{no} \code{yes}}
    \item{\samp{Village - town}}{a factor with levels \code{city} \code{village}}
    \item{\samp{House - block of flats}}{a factor with levels \code{block of flats} \code{house/bungalow}}
  }
}
\details{
Questionnaire
MUSIC PREFERENCES

    I enjoy listening to music.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I prefer.: Slow paced music 1-2-3-4-5 Fast paced music (integer)
    Dance, Disco, Funk: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Folk music: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Country: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Classical: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Musicals: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Pop: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Rock: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Metal, Hard rock: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Punk: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Hip hop, Rap: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Reggae, Ska: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Swing, Jazz: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Rock n Roll: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Alternative music: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Latin: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Techno, Trance: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Opera: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)

MOVIE PREFERENCES

    I really enjoy watching movies.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    Horror movies: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Thriller movies: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Comedies: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Romantic movies: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Sci-fi movies: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    War movies: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Tales: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Cartoons: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Documentaries: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Western movies: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)
    Action movies: Don't enjoy at all 1-2-3-4-5 Enjoy very much (integer)

HOBBIES & INTERESTS

    History: Not interested 1-2-3-4-5 Very interested (integer)
    Psychology: Not interested 1-2-3-4-5 Very interested (integer)
    Politics: Not interested 1-2-3-4-5 Very interested (integer)
    Mathematics: Not interested 1-2-3-4-5 Very interested (integer)
    Physics: Not interested 1-2-3-4-5 Very interested (integer)
    Internet: Not interested 1-2-3-4-5 Very interested (integer)
    PC Software, Hardware: Not interested 1-2-3-4-5 Very interested (integer)
    Economy, Management: Not interested 1-2-3-4-5 Very interested (integer)
    Biology: Not interested 1-2-3-4-5 Very interested (integer)
    Chemistry: Not interested 1-2-3-4-5 Very interested (integer)
    Poetry reading: Not interested 1-2-3-4-5 Very interested (integer)
    Geography: Not interested 1-2-3-4-5 Very interested (integer)
    Foreign languages: Not interested 1-2-3-4-5 Very interested (integer)
    Medicine: Not interested 1-2-3-4-5 Very interested (integer)
    Law: Not interested 1-2-3-4-5 Very interested (integer)
    Cars: Not interested 1-2-3-4-5 Very interested (integer)
    Art: Not interested 1-2-3-4-5 Very interested (integer)
    Religion: Not interested 1-2-3-4-5 Very interested (integer)
    Outdoor activities: Not interested 1-2-3-4-5 Very interested (integer)
    Dancing: Not interested 1-2-3-4-5 Very interested (integer)
    Playing musical instruments: Not interested 1-2-3-4-5 Very interested (integer)
    Poetry writing: Not interested 1-2-3-4-5 Very interested (integer)
    Sport and leisure activities: Not interested 1-2-3-4-5 Very interested (integer)
    Sport at competitive level: Not interested 1-2-3-4-5 Very interested (integer)
    Gardening: Not interested 1-2-3-4-5 Very interested (integer)
    Celebrity lifestyle: Not interested 1-2-3-4-5 Very interested (integer)
    Shopping: Not interested 1-2-3-4-5 Very interested (integer)
    Science and technology: Not interested 1-2-3-4-5 Very interested (integer)
    Theatre: Not interested 1-2-3-4-5 Very interested (integer)
    Socializing: Not interested 1-2-3-4-5 Very interested (integer)
    Adrenaline sports: Not interested 1-2-3-4-5 Very interested (integer)
    Pets: Not interested 1-2-3-4-5 Very interested (integer)

PHOBIAS

    Flying: Not afraid at all 1-2-3-4-5 Very afraid of (integer)
    Thunder, lightning: Not afraid at all 1-2-3-4-5 Very afraid of (integer)
    Darkness: Not afraid at all 1-2-3-4-5 Very afraid of (integer)
    Heights: Not afraid at all 1-2-3-4-5 Very afraid of (integer)
    Spiders: Not afraid at all 1-2-3-4-5 Very afraid of (integer)
    Snakes: Not afraid at all 1-2-3-4-5 Very afraid of (integer)
    Rats, mice: Not afraid at all 1-2-3-4-5 Very afraid of (integer)
    Ageing: Not afraid at all 1-2-3-4-5 Very afraid of (integer)
    Dangerous dogs: Not afraid at all 1-2-3-4-5 Very afraid of (integer)
    Public speaking: Not afraid at all 1-2-3-4-5 Very afraid of (integer)

HEALTH HABITS

    Smoking habits: Never smoked - Tried smoking - Former smoker - Current smoker (categorical)
    Drinking: Never - Social drinker - Drink a lot (categorical)
    I live a very healthy lifestyle.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)

PERSONALITY TRAITS, VIEWS ON LIFE & OPINIONS

    I take notice of what goes on around me.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I try to do tasks as soon as possible and not leave them until last minute.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I always make a list so I don't forget anything.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I often study or work even in my spare time.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I look at things from all different angles before I go ahead.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I believe that bad people will suffer one day and good people will be rewarded.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I am reliable at work and always complete all tasks given to me.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I always keep my promises.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I can fall for someone very quickly and then completely lose interest.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I would rather have lots of friends than lots of money.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I always try to be the funniest one.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I can be two faced sometimes.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I damaged things in the past when angry.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I take my time to make decisions.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I always try to vote in elections.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I often think about and regret the decisions I make.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I can tell if people listen to me or not when I talk to them.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I am a hypochondriac.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I am emphatetic person.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I eat because I have to. I don't enjoy food and eat as fast as I can.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I try to give as much as I can to other people at Christmas.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I don't like seeing animals suffering.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I look after things I have borrowed from others.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I feel lonely in life.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I used to cheat at school.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I worry about my health.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I wish I could change the past because of the things I have done.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I believe in God.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I always have good dreams.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I always give to charity.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I have lots of friends.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    Timekeeping.: I am often early. - I am always on time. - I am often running late. (categorical)
    Do you lie to others?: Never. - Only to avoid hurting someone. - Sometimes. - Everytime it suits me. (categorical)
    I am very patient.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I can quickly adapt to a new environment.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    My moods change quickly.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I am well mannered and I look after my appearance.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I enjoy meeting new people.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I always let other people know about my achievements.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I think carefully before answering any important letters.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I enjoy childrens' company.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I am not afraid to give my opinion if I feel strongly about something.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I can get angry very easily.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I always make sure I connect with the right people.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I have to be well prepared before public speaking.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I will find a fault in myself if people don't like me.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I cry when I feel down or things don't go the right way.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I am 100% happy with my life.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I am always full of life and energy.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I prefer big dangerous dogs to smaller, calmer dogs.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I believe all my personality traits are positive.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    If I find something the doesn't belong to me I will hand it in.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I find it very difficult to get up in the morning.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I have many different hobbies and interests.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I always listen to my parents' advice.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I enjoy taking part in surveys.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    How much time do you spend online?: No time at all - Less than an hour a day - Few hours a day - Most of the day (categorical)

SPENDING HABITS

    I save all the money I can.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I enjoy going to large shopping centres.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I prefer branded clothing to non branded.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I spend a lot of money on partying and socializing.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I spend a lot of money on my appearance.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I spend a lot of money on gadgets.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)
    I will hapilly pay more money for good, quality or healthy food.: Strongly disagree 1-2-3-4-5 Strongly agree (integer)

DEMOGRAPHICS

    Age: (integer)
    Height: (integer)
    Weight: (integer)
    How many siblings do you have?: (integer)
    Gender: Female - Male (categorical)
    I am: Left handed - Right handed (categorical)
    Highest education achieved: Currently a Primary school pupil - Primary school - Secondary school - College/Bachelor degree (categorical)
    I am the only child: No - Yes (categorical)
    I spent most of my childhood in a: City - village (categorical)
    I lived most of my childhood in a: house/bungalow - block of flats (categorical)
}
\source{
\link{https://www.kaggle.com/miroslavsabo/young-people-survey/data}
}
\references{
    Sleziak, P. - Sabo, M.: Gender differences in the prevalence of specific phobias. Forum Statisticum Slovacum. 2014, Vol. 10, No. 6. [Differences (gender + whether people lived in village/town) in the prevalence of phobias.]

    Sabo, Miroslav. Multivariate Statistical Methods with Applications. Diss. Slovak University of Technology in Bratislava, 2014. [Clustering of variables (music preferences, movie preferences, phobias) + Clustering of people w.r.t. their interests.]


}
\examples{
data(Young_People_Survey)
}
\keyword{datasets}
