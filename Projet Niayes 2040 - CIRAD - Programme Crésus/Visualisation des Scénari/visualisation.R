


# FONCTIONS PERSONNALISEES R-NIAYES 2040
source(file.path(getwd(),'functions_visualisation.R'))


#Extraction des scénarios
get_My_scenario(scenario="piezo")
get_My_scenario(scenario="exploitations")

#graphique des scnénarios
visuel <-visualisation(simul=FALSE,ref.patt="industries",y.lim="f")
visuel <-visualisation(simul=FALSE,ref.patt="industries",end="2013-12-31")

