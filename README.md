# Investigating deflationary effect of Bitcoin on US economy

The goal of this project was to investigate the deflationary effect of Bitcoin on US economy. Some economists, such as Paul Krugman believed in the possibility of deflationary pressure in crypto-currency networks. Bitcoin’s capped total money supply could be viewed as a variation on Milton Friedmans “k-percent rule”. This theory states that an optimal way to control inflation over the long term is for the central bank to grow the money supply by a fixed amount of k% each year, irrespective of the cyclical state of the economy. In particular, one should set the growth variable of k% at a rate equal to the growth of real GDP each year. This connection between Milton Freidman’s Nobel prize winning theory and Bitcoin practice was highlighted recently in a paper by B¨ohme who argues that one can consider Bitcoin as a type of “... proposal to fix the annual growth rate of the money supply to a fixed rate of growth.” B¨ohme reiterates the views of Paul Krugman that “... the fixed slow growth rate of Bitcoin creates the possibility of deflation if Bitcoin was to be used widely...”. He also notes that there have been other crypto-currency extensions of Bitcoin proposed to overcome such potential problems, see discussion by for instance King which introduces Primecoin with infinite money supply or the introduction of Peercoin which keeps k% around 1-2.

## Installation
First, install shiny  packages as follow:
```
require(devtools)
install_github('shiny', 'rstudio')
require(shiny)
```
## Running the app
Second, run the following command in your R Studio:
```
shiny::runGitHub('bitcoin-deflation', 'moghbaie')
```
## Built with
[shiny](http://shiny.rstudio.com/) -web applictaion framework for R

## Contributers
Mehrnoosh Oghbaie
Hongjing Zhang   
