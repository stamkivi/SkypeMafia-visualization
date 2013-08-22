SkypeMafia-visualization
========================

The aim of this visualization script is to celebrate the 10th birthday of Skype
by exposing the social graph around it.

Included are the founders, staff and investors who played a huge part
in making Skype possible in the first place, but also a network of new
startups founded and funded by these players, plus a number of more established
companies feeding on Skype-related talent.

Built on igraph package for R, the script takes two simple CSV input files:
* edge list of Person -> Company relationships & identifier for their role (founder, employee, investor/advisor)
* properties list for type, country of origin and formatting metadata of all entities mentioned in edge list

The data in Skype-specific CSV files has been acquired by (mostly manually) scanning public profiles
of mentioned players at sources such as Crunchbase, AngelList, LinkedIn, etc. Should be relatively easy to
replicate for whatever group of people and companies you're interested in if you were to branch this script.
