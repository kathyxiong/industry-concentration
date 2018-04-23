The United States is a vast place with a diverse economic landscape. One way to characterize an economy is by looking at the distribution of employment across industries. To get a sense of how the mix of industries differs across states, I decided to look at data from the Bureau of Labor Statistic's Quarterly Census of Employment and Wages.

In the interactive plot above, each row represents an industry, and each bar represents a state. The position of the bars on the x-axis represents the industry's share of that state's total workforce. Click on the buttons above the charts to highlight regions and to see data for different years. Hover over the bars to see state names.

## Patterns by region

Focusing on one year's data at a time, we can spot some interesting regional patterns by examining where bars of the same color tend to concentrate in the distribution. For example, looking at 2016's data, we can clearly see how much more predominant **Manufacturing** is in the Midwest than in the West, since there are more blue bars to the right and more green bars to the left, with little overlap in between. 

This is perhaps not too surprising. What is more surprising, to me at least, is the relative predominance of **Construction** and **Real estate** in the West -- perhaps a sign of the region's robust population growth and economic development -- and of **Finance and insurance** and **Management of Companies and enterprises** in the Midwest -- perhaps a result of the regions lower cost of operations compared to large coastal cities.

## Patterns over time

Switching between the years, we can see how employment distribution has changed over time. The most dramatic shift is probably the rise of employment in **Health care and social assistance** along with the decline in **Manufacturing**. 

## Patterns by region, over time

The combination of regional patterns and changes over time reveals some interesting insights. For example, looking at 2016's data, I noticed that **Health care and social assistance**, and **Educational services**, appear to be more predominant in the Northeast than elsewhere in the country. However, back in 1990, this wasn't the case. The Midwest was almost on par with the Northeast in terms of percent of people employed in Health care and social assistance, and the West was ahead of the Northeast in terms of Educational services. The rise of Healthcare and Educational services in the Northeast appears to a trend that has developed in the last thirty years.

Another interesting pattern I noticed is the increased predominance of **Accomodation and food services** in many states. Back in 1990, a few Western states stand out as having more employement in Accomodation and food services. In the last three decades, however, employment in Accomondation and food services have increased in other states, so that in 2016, there is no longer a clear division between the regions.

---

### Data notes

The data comes from the Bureau of Labor Statistic's [Quarterly Census of Employment and Wages](https://www.bls.gov/cew/datatoc.htm). Percentages are calculated from annual averages of monthly employment levels, by dividing employment for a given industry by total employment for that state. Employment includes all ownership types (public and private sectors). Industries represent NAICS Supersectors and the "Unclassified" category is not shown. Raw data and R script used to process the data can be found on my [github](https://github.com/kathyxiong/industry-concentration).
