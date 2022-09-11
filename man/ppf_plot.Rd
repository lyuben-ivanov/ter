\name{ppf_plot}
\alias{ppf_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Production Possibilities Frontier Plot
}
\description{
This function takes as an input the maximum quantities of two products that
a country can produce and produces a production possibilities frontier plot
which shows the various combinations of output that the country can produce
given its resources and available technology.
}
\usage{
ppf_plot(
  maximum_quantity_of_product_1 = 50,
  maximum_quantity_of_product_2 = 30,
  plot_new_frontier = FALSE,
  new_maximum_quantity_of_product_1 = 50,
  new_maximum_quantity_of_product_2 = 30,
  plot_point = FALSE,
  point_label = "A",
  quantity_of_product_1 = 10,
  quantity_of_product_2 = 28.75,
  plot_new_point = FALSE,
  new_point_label = "B",
  new_quantity_of_product_1 = 15,
  new_quantity_of_product_2 = 15,
  x_axis_label = "Production of pizzas (mlns per month)",
  y_axis_label = "Production of robots (thousands per month)",
  linear = FALSE
  )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{maximum_quantity_of_product_1}{
integer: the amount of the first good or service that can be produced
if all resources in the economy are allocated to the production of this good or
service
}
  \item{maximum_quantity_of_product_2}{
integer: the amount of the second good or service that can be produced
if all resources in the economy are allocated to the production of this good or
service
}
  \item{plot_new_frontier}{
logical: indicates whether a new frontier should be plotted alongside the
existing one
  }
  \item{new_maximum_quantity_of_product_1}{
integer: the amount of the first good or service that can be produced
if the productivity of the economy with regard to this good or service changes
or more resources become available
}
  \item{new_maximum_quantity_of_product_2}{
integer: the amount of the second good or service that can be produced
if the productivity of the economy with regard to this good or service changes
or more resources become available
}
 \item{plot_point}{
logical: indicates whether a point showing possible combination of the two
products should be plotted on the graph or not
}
  \item{point_label}{
string: a capital letter (or any other string) to replace the default label of
the point
}
 \item{quantity_of_product_1}{
integer: the x-axis location of the point showing possible combination of the two
products
}
  \item{quantity_of_product_2}{
integer: the y-axis location of the new point showing possible combination of
the two products
}
 \item{plot_new_point}{
logical: indicates whether a new point showing another possible combination
of the two products should be plotted on the graph or not
}
 \item{new_point_label}{
string: a capital letter (or any other string) to replace the default label of
the new point
}
 \item{new_quantity_of_product_1}{
integer: the x-axis location of the new point showing possible combination of
the two products
}
  \item{new_quantity_of_product_2}{
integer: the y-axis location of the new point showing possible combination of
the two products
}
 \item{x_axis_label}{
string: a word or a few words to replace the default label of the x-axis
}
 \item{y_axis_label}{
string: a word or a few words to replace the default label of the y-axis
}
 \item{linear}{
logical: indicates whether the production possibility frontier should be shown
as a straight line (indicating constant opportunity cost) or not (indicating
increasing opportunity cost)
}
}
\details{
The graph can be made dynamic by specifying new maximum quantities of the
products that are different from the initial maximum quantities.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Lyuben Ivanov
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
