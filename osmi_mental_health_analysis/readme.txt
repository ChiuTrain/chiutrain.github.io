OSMI Mental Health in Tech Dataset Case Study


1. Scenario

An American mental healthcare company (hereafter referred to as "The Company") conducts research on mental health in the workplace and consults with corporations and governmental organizations on improving the mental health of the American workforce through efforts such as recommending the implementation of workplace policies that aim to mitigate the effects of mental health disorders on workers. The Company issues a survey every year to companies nationwide in an attempt to gather data related to mental health issues in the workplace, as well as how frequent mental health disorders occur among working persons. 

This year (2021), The Company decided to issue out a survey specifically targeting tech companies in the United States (US), as they are looking to conduct research targeting employees working specifically in this field, and aim to provide effective mental health policy recommendations to tech companies. The Company believes that the technological field is expanding rapidly, and that as more workers join this sector, more data is needed on how to best mitigate mental health issues in these companies in order to maximize workforce efficiency, and thus quickening technological advancements.

You are a data analyst working for The Company, and have been tasked with analyzing the data collected from the survey to provide recommendations for how to best proceed given the findings from the data. The recommendations should align with the company's goals and objectives (i.e., condcuting research on understanding mental health, and providing policy recommendations to corporate and governmental entities). You have figured out that, due to the context of this project, this is more so an exploratory analysis; in other words, there are no fixed/pre-determined hypotheses and research questions that need to be answered. Rather, you are attempting to make sense of trends or observations in the data collected to develop a research question/hypothesis instead.


2. Sources

The dataset used is the 2021 dataset from Open Sourcing Mental Health (OSMI), which can be found at (https://osmhhelp.org/research.html). OSMI is a non-profit corporation aiming to raise mental health awarneess in tech communities, and has previously partnered with GitHub to develop their survey and guideline recommendations (https://osmhhelp.org/resources/index.html). The corporation's board members include a licensed clincal psychologist, industrial-organizational psychologist, and two software developers. They have also been featured on Business Insider (https://www.businessinsider.com/open-sourcing-mental-illness-stigma-tech-2019-12), where founder Ed Finkler provided information on burnout and mental health issues in the workplace. All this suggests that the dataset (and OSMI) are credible sources of data.


3. Overall Data Process

The data analysis in this project follows the 6-stage process for data analyses as highlighted by Google:

1) Ask: Defining the objective of the data analysis, and the problem to be solved
2) Prepare: Collecting/choosing the correct data, understanding the metrics needed for analysis
3) Process: Data cleaning
4) Analyze: Data analysis
5) Share: Data visualization and presentation to stakeholders
6) Act: Providing recommendations on how to solve the problem


4. File Directory/Summary

This folder contains several files; below is a quick description of what is contained within each file:

OSMI_2022.csv -> The raw data file provided by OSMI. Note that this is data gathered in 2021, despite the filename indicating otherwise.
OSMI_Analysis_XXX.r -> R files for each section of the data analysis in the final report (i.e., the rmd/html files); these are in seprate files for better readability.
OSMI_Demo_Markdown.rmd -> The R Markdown file for the report guiding stakeholders through the data analysis process
OSMI_Demo_Markdown.html -> The HTML version of the above file