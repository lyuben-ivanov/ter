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
ppf_plot(maximum_quantity_of_product_1 = 50,
    maximum_quantity_of_product_2 = 30,
    new_maximum_quantity_of_product_1 = 50,
    new_maximum_quantity_of_product_2 = 30,
    linear = FALSE,
    plot_points = TRUE,
    point_label = "A",
    quantity_product_1 = 10,
    quantity_product_2 = 28.75,
    x_axis_label = "Production of pizzas (mlns per month)",
    y_axis_label = "Production of robots (hundreds per month)")
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
 \item{linear}{
logical: indicates whether the production possibility frontier should be shown
as a straight line (indicating constant opportunity cost) or not (indicating
increasing opportunity cost)
}
 \item{plot_points}{
logical: indicates whether points should be plotted on the graph or not
}
 \item{point_label}{
string: a capital letter (or any other string) to replace the default label of
the point
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
