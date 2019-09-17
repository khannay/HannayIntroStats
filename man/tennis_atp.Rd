\name{tennis_atp}
\alias{tennis_atp}
\docType{data}
\title{
Association of Tennis Professionals Matches
}
\description{
ATP tennis matches from 2000-2017 downloaded from kaggle.com. w_ means for the winning player l_ means for the loser.
}
\usage{data("tennis_atp")}
\format{
  A data frame with 53571 observations on the following 50 variables.
  \describe{
    \item{\code{tourney_id}}{a character vector}
    \item{\code{year}}{a numeric vector}
    \item{\code{tourney_name}}{a character vector}
    \item{\code{surface}}{a character vector}
    \item{\code{draw_size}}{a numeric vector}
    \item{\code{tourney_level}}{a character vector}
    \item{\code{tourney_date}}{a numeric vector}
    \item{\code{match_num}}{a character vector}
    \item{\code{winner_id}}{a numeric vector}
    \item{\code{winner_seed}}{a numeric vector}
    \item{\code{winner_entry}}{a character vector}
    \item{\code{winner_name}}{a character vector}
    \item{\code{winner_hand}}{a character vector}
    \item{\code{winner_ht}}{a numeric vector}
    \item{\code{winner_ioc}}{a character vector}
    \item{\code{winner_age}}{a numeric vector}
    \item{\code{winner_rank}}{a numeric vector}
    \item{\code{winner_rank_points}}{a numeric vector}
    \item{\code{loser_id}}{a numeric vector}
    \item{\code{loser_seed}}{a numeric vector}
    \item{\code{loser_entry}}{a character vector}
    \item{\code{loser_name}}{a character vector}
    \item{\code{loser_hand}}{a character vector}
    \item{\code{loser_ht}}{a numeric vector}
    \item{\code{loser_ioc}}{a character vector}
    \item{\code{loser_age}}{a numeric vector}
    \item{\code{loser_rank}}{a numeric vector}
    \item{\code{loser_rank_points}}{a numeric vector}
    \item{\code{score}}{a character vector}
    \item{\code{best_of}}{a numeric vector}
    \item{\code{round}}{a character vector}
    \item{\code{minutes}}{a numeric vector}
    \item{\code{w_ace}}{a numeric vector absolute number of aces}
    \item{\code{w_df}}{a numeric vector number of double faults}
    \item{\code{w_svpt}}{a numeric vector total serve points}
    \item{\code{w_1stIn}}{a numeric vector 1st serve in}
    \item{\code{w_1stWon}}{a numeric vector points won on first serve}
    \item{\code{w_2ndWon}}{a numeric vector points won on second serve}
    \item{\code{w_SvGms}}{a numeric vector giving the serve games}
    \item{\code{w_bpSaved}}{a numeric vector break point saved}
    \item{\code{w_bpFaced}}{a numeric vector break point faced}
    \item{\code{l_ace}}{a numeric vector}
    \item{\code{l_df}}{a numeric vector}
    \item{\code{l_svpt}}{a numeric vector}
    \item{\code{l_1stIn}}{a numeric vector}
    \item{\code{l_1stWon}}{a numeric vector}
    \item{\code{l_2ndWon}}{a numeric vector}
    \item{\code{l_SvGms}}{a numeric vector}
    \item{\code{l_bpSaved}}{a numeric vector}
    \item{\code{l_bpFaced}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
https://www.kaggle.com/gmadevs/atp-matches-dataset
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(tennis_atp)
}
\keyword{datasets}
