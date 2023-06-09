# Innovation-Geothermal-Dashboard 

Link Dashboard RShiny : https://nidals17.shinyapps.io/Dash/?_ga=2.25631997.1550294498.1675550866-1755815181.1674844831
 
## Description :

The dashboard aims to clearly visualize data related to the patent and scientific paper in geothermal energy and understand trends and relationships between technical terms. 
This work is based on a research paper in production named "Detecting knowledge cycles∗" by Moritz Müller, Ingrid, Ulrich Schmoch, and Jonathan Völkle. 
Here,we will present the database structure, descriptive statistics, and graphs for a better overall understanding of the knowledge space dynamics. 

## Information about the Data :

In our df data every observation is a (document, term) combination, a document is a paper or patent, a term is a token or an n-gram (n=2,3), n is the number of times the term appears in the document title or abstract, year the patent is applied for or paper is published, country is where patent is applied, type is patent(technology) or paper(scientific), topic - is the topic with which the term is most likely associated - taking into account science and technology layer, beta is the probability of the term being associated with that topic.

In our comp_hist data every observation is (type,term,year) yielding some information on a term in a layer (patent/paper) in a year the topic is strictly associated with the topic (as in df above), n - number of uses in that (type,year), n_cum - cummulated number of uses in that (type,year), comp_inno - whether it is an innovation (1) or not (0), n_any - number of uses in that (year) but any layer, n_cum_any - number of cummulated uses up to that (year) but any layer, first_year_mentioned - when the term is first mentioned in any layer



## How to Use the Dashboard :

The dashboard is designed to be user-friendly and interactive. 
You can choose the technical terms you are interested in and track their appearance and spread in the fields of science and technology. 
Additionally, you can view descriptive statistics, such as the ratio of usage, top countries in scientific paper production, and even visualize a network of relationships between themes. 
These features provide a comprehensive understanding of the data related to geothermal energy, making it easier to identify trends and patterns.

## Requirements : 

* Shiny , main package to create interactive web applications in R. 
* Shinydashboard , used to design a dashboard interface for the web application.
* Dplyr , The dplyr library is included for data manipulation and cleaning purposes.
* ggplot2 , utilized for data visualization and generating plots.
* Plotly , provides interactive and animated data visualizations.
* Igraph , used for creating and analyzing network graphs.

