% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selenium_funs.R
\name{sel_get_driver}
\alias{sel_get_driver}
\title{Make Selenium Driver}
\usage{
sel_get_driver(port_sel, ecaps = list())
}
\arguments{
\item{port_sel}{Selenium Port (see sel_docker_start())}

\item{ecaps}{A list of Extra-Capabilites (Default for Firefox Direct DOwnload)}
}
\value{
An object of class remote driver
}
\description{
Make Selenium Driver
}
