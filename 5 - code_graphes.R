library(tidyverse)
library(tidyquant)
library(lubridate)
library(haven)
library(readr)
library(ggplot2)

# Stats échantillon étude -- stagiaires 2017-2019 ----

stats_age_entreprise_tbl <- read.csv("Stats_echantillon_etude/statistiques_age_entreprise.csv")
stats_age_entreprise_tbl <- stats_age_entreprise_tbl %>% mutate(proportion = count/sum(count))
stats_age_entreprise_tbl %>% 
  mutate(AGE_ENTREPRISE = as.factor(AGE_ENTREPRISE)%>% fct_reorder(proportion))  %>% 
  ggplot(aes(x=AGE_ENTREPRISE, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Company age among 2017-2019 interns",
       x = "Company age",
       y= "")

stats_age_tbl <- read.csv("Stats_echantillon_etude/statistiques_age.csv")
stats_age_tbl <- stats_age_tbl %>% mutate(proportion = count/sum(count)) %>% filter(proportion>0.001)
stats_age_tbl  %>% 
  ggplot(aes(x=categorie_age, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Age category among 2017-2019 interns",
       x = "Age category",
       y= "")

stats_commanditaire_tbl <- read.csv("Stats_echantillon_etude/statistiques_commanditaire.csv")
stats_commanditaire_tbl <- stats_commanditaire_tbl %>% mutate(proportion = count/sum(count))
stats_commanditaire_tbl %>% 
  ggplot(aes(x=COMMANDITAIRE, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Sponsor among 2017-2019 interns",
       x = "Sponsor",
       y= "")

stats_diplome_tbl <- read.csv("Stats_echantillon_etude/statistiques_diplome.csv")
stats_diplome_tbl <- stats_diplome_tbl %>% mutate(proportion = count/sum(count))
stats_diplome_tbl %>%  mutate(NIV_DIPLOME = c("1", "2", "3", "4", "5", "6", "7")) %>% 
  ggplot(aes(x=NIV_DIPLOME, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Education level among 2017-2019 interns",
       x = "Education level",
       y= "")
  

stats_industrie_tbl <- read.csv("Stats_echantillon_etude/statistiques_industrie.csv")
stats_industrie_tbl <- stats_industrie_tbl %>% mutate(proportion = count/sum(count))
stats_industrie_tbl %>% 
  ggplot(aes(x=A21_ET, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Industry among 2017-2019 interns",
       x = "Industry",
       y= "")

stats_sexe_tbl <- read.csv("Stats_echantillon_etude/statistiques_sexe.csv")
stats_sexe_tbl <- stats_sexe_tbl %>% mutate(proportion = count/sum(count))
stats_sexe_tbl %>% 
  ggplot(aes(x=SEXE.x, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Sex of 2017-2019 interns",
       x = "Sex",
       y= "")

stats_region_tbl <- read.csv("Stats_echantillon_etude/statistiques_region.csv")
stats_region_tbl <- stats_region_tbl %>% mutate(proportion = count/sum(count))
stats_region_tbl %>% mutate(REGION_HABITATION = as.character(c(1:19))) %>% 
  ggplot(aes(x=REGION_HABITATION, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Region of 2017-2019 interns",
       x = "Region",
       y= "")

stats_remuneration_tbl <- read.csv("Stats_echantillon_etude/statistiques_remuneration.csv")
stats_remuneration_tbl <- stats_remuneration_tbl %>% mutate(proportion = count/sum(count))
stats_remuneration_tbl %>% mutate(TYPE_REMUNERATION = c("1", "2", "3", "4", "5", "6", "7")) %>% 
  ggplot(aes(x=TYPE_REMUNERATION, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Origin of earnings among 2017-2019 interns",
       x = "Origin of earnings",
       y= "")

stats_duree_formation_tbl <- read.csv("Stats_echantillon_etude/statistiques_duree_formation.csv")
stats_duree_formation_tbl <- stats_duree_formation_tbl %>% mutate(proportion = count/sum(count))
stats_duree_formation_tbl %>% mutate(DUREE_FORMATION_DECILE = as.factor(DUREE_FORMATION_DECILE)) %>% 
  ggplot(aes(x=DUREE_FORMATION_DECILE, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Duration of internship among 2017-2019 interns",
       x = "Duration of internship",
       y = "")

stats_domaine_formation_tbl <- read.csv("Stats_echantillon_etude/statistiques_domaine_formation.csv")
stats_domaine_formation_tbl <- stats_domaine_formation_tbl %>% mutate(proportion = count/sum(count))
stats_domaine_formation_tbl %>% mutate(DOMAINE_FORMATION = c("A", "B", "C", "D", "E", "F", "G", "H", "J", "L", "M", "O")) %>% 
  ggplot(aes(x=DOMAINE_FORMATION, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Internship thema among 2017-2019 interns",
       x = "Internship thema",
       y = "")

# stats de_tempo_2019_in_2022
stats_age_entreprise_tbl <- read.csv("stats_de_tempo_2019_in_2022/.csv")
stats_age_entreprise_tbl <- stats_age_entreprise_tbl %>% mutate(proportion = count/sum(count))
stats_age_entreprise_tbl %>% 
  mutate(AGE_ENTREPRISE = as.factor(AGE_ENTREPRISE)%>% fct_reorder(proportion))  %>% 
  ggplot(aes(x=AGE_ENTREPRISE, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Company age among 2019 interns, working in 2022,\n present in the fh database in 2019",
       x = "Company age",
       y= "")

stats_age_tbl <- read.csv("stats_de_tempo_2019_in_2022/categorie_age.csv")
stats_age_tbl <- stats_age_tbl %>% mutate(proportion = count/sum(count)) %>% filter(proportion>0.001)
stats_age_tbl  %>% 
  ggplot(aes(x=categorie_age, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Age category among 2019 interns, working in 2022,\n present in the fh database in 2019",
       x = "Age category",
       y= "")

stats_commanditaire_tbl <- read.csv("stats_de_tempo_2019_in_2022/commanditaire.csv")
stats_commanditaire_tbl <- stats_commanditaire_tbl %>% mutate(proportion = count/sum(count))
stats_commanditaire_tbl %>% 
  ggplot(aes(x=COMMANDITAIRE, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Sponsor among 2019 interns, working in 2022,\n present in the fh database in 2019",
       x = "Sponsor",
       y= "")

stats_diplome_tbl <- read.csv("stats_de_tempo_2019_in_2022/niv_diplome.csv")
stats_diplome_tbl <- stats_diplome_tbl %>% mutate(proportion = count/sum(count))
stats_diplome_tbl %>%  mutate(NIV_DIPLOME = c("1", "2", "3", "4", "5", "6", "7")) %>% 
  ggplot(aes(x=NIV_DIPLOME, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Education level among 2019 interns, working in 2022\npresent in the fh database in 2019",
       x = "Education level",
       y= "")


stats_industrie_tbl <- read.csv("stats_de_tempo_2019_in_2022/salaire_A21_ET.csv")
stats_industrie_tbl <- stats_industrie_tbl %>% mutate(proportion = count/sum(count))
stats_industrie_tbl %>% 
  ggplot(aes(x=A21_ET, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Industry among 2019 interns, working in 2022,\n present in the fh database in 2019",
       x = "Industry",
       y= "")

stats_sexe_tbl <- read.csv("stats_de_tempo_2019_in_2022/salaires_sexe.csv")
stats_sexe_tbl <- stats_sexe_tbl %>% mutate(proportion = count/sum(count))
stats_sexe_tbl %>% 
  ggplot(aes(x=SEXE.x, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Sex of 2019 interns, working in 2022,\n present in the fh database in 2019",
       x = "Sex",
       y= "")

stats_region_tbl <- read.csv("stats_de_tempo_2019_in_2022/region_habitation.csv")
stats_region_tbl <- stats_region_tbl %>% mutate(proportion = count/sum(count))
stats_region_tbl %>% mutate(REGION_HABITATION = as.character(c(1:15))) %>% 
  ggplot(aes(x=REGION_HABITATION, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Region of 2019 interns, working in 2022, present in the fh database in 2019",
       x = "Region",
       y= "")

stats_remuneration_tbl <- read.csv("stats_de_tempo_2019_in_2022/type_remuneration.csv")
stats_remuneration_tbl <- stats_remuneration_tbl %>% mutate(proportion = count/sum(count))
stats_remuneration_tbl %>% mutate(TYPE_REMUNERATION = c("1", "2", "3", "4", "5", "6")) %>% 
  ggplot(aes(x=TYPE_REMUNERATION, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Origin of earnings among 2019 interns, working in 2022, present in the fh database in 2019",
       x = "Origin of earnings",
       y= "")

stats_duree_formation_tbl <- read.csv("stats_de_tempo_2019_in_2022/decile_heures_formation.csv")
stats_duree_formation_tbl <- stats_duree_formation_tbl %>% mutate(proportion = count/sum(count))
stats_duree_formation_tbl %>% mutate(DUREE_FORMATION_DECILE=round(DUREE_FORMATION_DECILE, digits=3)) %>% 
  mutate(DUREE_FORMATION_DECILE = as.factor(DUREE_FORMATION_DECILE)) %>% 
  ggplot(aes(x=DUREE_FORMATION_DECILE, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Duration of internship among 2019 interns,\n working in 2022, present in the fh database in 2019",
       x = "Duration of internship",
       y = "")

stats_domaine_formation_tbl <- read.csv("stats_de_tempo_2019_in_2022/statistiques_domaine_formation.csv")
stats_domaine_formation_tbl <- stats_domaine_formation_tbl %>% mutate(proportion = count/sum(count))
stats_domaine_formation_tbl %>% mutate(DOMAINE_FORMATION = c("A", "B", "C", "D", "E", "F", "G", "H", "J", "L", "M", "O")) %>% 
  ggplot(aes(x=DOMAINE_FORMATION, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Internship thema among 2017-2019 interns",
       x = "Internship thema",
       y = "")

# stats de_tempo_2019_not_in_2022

stats_age_entreprise_tbl <- read.csv("stats_de_tempo_2019_not_in_2022/")
stats_age_entreprise_tbl <- stats_age_entreprise_tbl %>% mutate(proportion = count/sum(count))
stats_age_entreprise_tbl %>% 
  mutate(AGE_ENTREPRISE = as.factor(AGE_ENTREPRISE)%>% fct_reorder(proportion))  %>% 
  ggplot(aes(x=AGE_ENTREPRISE, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Company age among 2017-2019 interns",
       x = "Company age",
       y= "")

stats_age_tbl <- read.csv("stats_de_tempo_2019_not_in_2022/categorie_age.csv")
stats_age_tbl <- stats_age_tbl %>% mutate(proportion = count/sum(count)) %>% filter(proportion>0.001)
stats_age_tbl  %>% 
  ggplot(aes(x=categorie_age, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Age category among 2019 interns,\n not working in 2022, present in the fh database in 2019",
       x = "Age category",
       y= "")

stats_commanditaire_tbl <- read.csv("stats_de_tempo_2019_not_in_2022/commanditaire.csv")
stats_commanditaire_tbl <- stats_commanditaire_tbl %>% mutate(proportion = count/sum(count))
stats_commanditaire_tbl %>% 
  ggplot(aes(x=COMMANDITAIRE, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Sponsor among 2019 interns,\n not working in 2022, present in the fh database in 2019",
       x = "Sponsor",
       y= "")

stats_diplome_tbl <- read.csv("stats_de_tempo_2019_not_in_2022/niv_diplome.csv")
stats_diplome_tbl <- stats_diplome_tbl %>% mutate(proportion = count/sum(count))
stats_diplome_tbl %>%  mutate(NIV_DIPLOME = c("1", "2", "3", "4", "5", "6", "7")) %>% 
  ggplot(aes(x=NIV_DIPLOME, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Education level among 2019 interns,\n not working in 2022, present in the fh database in 2019",
       x = "Education level",
       y= "")


stats_industrie_tbl <- read.csv("stats_de_tempo_2019_not_in_2022/salaire_A21_ET.csv")
stats_industrie_tbl <- stats_industrie_tbl %>% mutate(proportion = count/sum(count))
stats_industrie_tbl %>% 
  ggplot(aes(x=A21_ET, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Industry among 2019 interns,\n not working in 2022, present in the fh database in 2019",
       x = "Industry",
       y= "")

stats_sexe_tbl_2019_de_tempo_not_2022 <- read.csv("stats_de_tempo_2019_not_in_2022/salaires_sexe.csv")
stats_sexe_tbl <- stats_sexe_tbl %>% mutate(proportion = count/sum(count))
stats_sexe_tbl %>% 
  ggplot(aes(x=SEXE.x, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Sex among 2019 interns,\n not working in 2022, present in the fh database in 2019",
       x = "Sex",
       y= "")

stats_region_tbl <- read.csv("stats_de_tempo_2019_not_in_2022/region_habitation.csv")
stats_region_tbl <- stats_region_tbl %>% mutate(proportion = count/sum(count))
stats_region_tbl %>% mutate(REGION_HABITATION = as.character(c(1:15))) %>% 
  ggplot(aes(x=REGION_HABITATION, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Region of 2017-2019 interns",
       x = "Region",
       y= "")

stats_remuneration_tbl <- read.csv("stats_de_tempo_2019_not_in_2022/type_remuneration.csv")
stats_remuneration_tbl <- stats_remuneration_tbl %>% mutate(proportion = count/sum(count))
stats_remuneration_tbl %>% mutate(TYPE_REMUNERATION = c("1", "2", "3", "4", "5", "6")) %>% 
  ggplot(aes(x=TYPE_REMUNERATION, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Origin of earnings among 2017-2019 interns",
       x = "Origin of earnings",
       y= "")

stats_duree_formation_tbl <- read.csv("stats_de_tempo_2019_not_in_2022/decile_heures_formation.csv")
stats_duree_formation_tbl <- stats_duree_formation_tbl %>% mutate(proportion = count/sum(count))
stats_duree_formation_tbl %>%mutate(DUREE_FORMATION_DECILE=round(DUREE_FORMATION_DECILE, digits=3)) %>% 
  mutate(DUREE_FORMATION_DECILE = as.factor(DUREE_FORMATION_DECILE)) %>% 
  ggplot(aes(x=DUREE_FORMATION_DECILE, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Duration of internship among 2019 interns,\n not working in 2022, present in the fh database in 2019",
       x = "Duration of internship",
       y = "")

stats_domaine_formation_tbl <- read.csv("stats_de_tempo_2019_not_in_2022/statistiques_domaine_formation.csv")
stats_domaine_formation_tbl <- stats_domaine_formation_tbl %>% mutate(proportion = count/sum(count))
stats_domaine_formation_tbl %>% mutate(DOMAINE_FORMATION = c("A", "B", "C", "D", "E", "F", "G", "H", "J", "L", "M", "O")) %>% 
  ggplot(aes(x=DOMAINE_FORMATION, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Internship thema among 2017-2019 interns",
       x = "Internship thema",
       y = "")

# stats stagiaires 2019_2022_pas de tempo

stats_age_entreprise_tbl <- read.csv("stats_stagiaires_2019_2022_pas_de_tempo/")
stats_age_entreprise_tbl <- stats_age_entreprise_tbl %>% mutate(proportion = count/sum(count))
stats_age_entreprise_tbl %>% 
  mutate(AGE_ENTREPRISE = as.factor(AGE_ENTREPRISE)%>% fct_reorder(proportion))  %>% 
  ggplot(aes(x=AGE_ENTREPRISE, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Company age among 2017-2019 interns",
       x = "Company age",
       y= "")

stats_age_tbl <- read.csv("stats_stagiaires_2019_2022_pas_de_tempo/categorie_age.csv")
stats_age_tbl <- stats_age_tbl %>% mutate(proportion = count/sum(count)) %>% filter(proportion>0.001)
stats_age_tbl  %>% 
  ggplot(aes(x=categorie_age, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Age category among 2019 interns working in 2022, not in the fh database",
       x = "Age category",
       y= "")

stats_commanditaire_tbl <- read.csv("stats_stagiaires_2019_2022_pas_de_tempo/commanditaire.csv")
stats_commanditaire_tbl <- stats_commanditaire_tbl %>% mutate(proportion = count/sum(count))
stats_commanditaire_tbl %>% 
  ggplot(aes(x=COMMANDITAIRE, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Sponsor among 2019 interns working in 2022, not in the fh database",
       x = "Sponsor",
       y= "")

stats_diplome_tbl <- read.csv("stats_stagiaires_2019_2022_pas_de_tempo/niv_diplome.csv")
stats_diplome_tbl <- stats_diplome_tbl %>% mutate(proportion = count/sum(count))
stats_diplome_tbl %>%  mutate(NIV_DIPLOME = c("1", "2", "3", "4", "5", "6", "7")) %>% 
  ggplot(aes(x=NIV_DIPLOME, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Education level among 2019 interns working in 2022, not in the fh database",
       x = "Education level",
       y= "")


stats_industrie_tbl <- read.csv("stats_stagiaires_2019_2022_pas_de_tempo/salaire_A21_ET.csv")
stats_industrie_tbl <- stats_industrie_tbl %>% mutate(proportion = count/sum(count))
stats_industrie_tbl %>% 
  ggplot(aes(x=A21_ET, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Industry among 2019 interns working in 2022, not in the fh database",
       x = "Industry",
       y= "")

stats_sexe_tbl <- read.csv("stats_stagiaires_2019_2022_pas_de_tempo/salaires_sexe.csv")
stats_sexe_tbl <- stats_sexe_tbl %>% mutate(proportion = count/sum(count))
stats_sexe_tbl %>% 
  ggplot(aes(x=SEXE.x, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Sex among 2019 interns working in 2022, not in the fh database",
       x = "Sex",
       y= "")

stats_region_tbl <- read.csv("stats_stagiaires_2019_2022_pas_de_tempo/region_habitation.csv")
stats_region_tbl <- stats_region_tbl %>% mutate(proportion = count/sum(count))
stats_region_tbl %>% mutate(REGION_HABITATION = as.character(c(1:17))) %>% 
  ggplot(aes(x=REGION_HABITATION, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Region of 2017-2019 interns",
       x = "Region",
       y= "")

stats_remuneration_tbl <- read.csv("stats_stagiaires_2019_2022_pas_de_tempo/type_remuneration.csv")
stats_remuneration_tbl <- stats_remuneration_tbl %>% mutate(proportion = count/sum(count))
stats_remuneration_tbl %>% mutate(TYPE_REMUNERATION = c("1", "2", "3", "4", "5", "6", "7")) %>% 
  ggplot(aes(x=TYPE_REMUNERATION, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Origin of earnings among 2017-2019 interns",
       x = "Origin of earnings",
       y= "")

stats_duree_formation_tbl <- read.csv("stats_stagiaires_2019_2022_pas_de_tempo/decile_heures_formation.csv")
stats_duree_formation_tbl <- stats_duree_formation_tbl %>% mutate(proportion = count/sum(count))
stats_duree_formation_tbl %>% mutate(DUREE_FORMATION_DECILE=round(DUREE_FORMATION_DECILE, digits=3)) %>%
  mutate(DUREE_FORMATION_DECILE = as.factor(DUREE_FORMATION_DECILE)) %>% 
  ggplot(aes(x=DUREE_FORMATION_DECILE, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Duration of internship among 2019 interns working in 2022, not in the fh database",
       x = "Duration of internship",
       y = "")

stats_domaine_formation_tbl <- read.csv("stats_stagiaires_2019_2022_pas_de_tempo/statistiques_domaine_formation.csv")
stats_domaine_formation_tbl <- stats_domaine_formation_tbl %>% mutate(proportion = count/sum(count))
stats_domaine_formation_tbl %>% mutate(DOMAINE_FORMATION = c("A", "B", "C", "D", "E", "F", "G", "H", "J", "L", "M", "O")) %>% 
  ggplot(aes(x=DOMAINE_FORMATION, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Internship thema among 2017-2019 interns",
       x = "Internship thema",
       y = "")*
  
  #Stats 2019 interns - not 2022
  
  stats_age_entreprise_tbl <- read.csv("Stats_echantillon_etude/statistiques_age_entreprise.csv")
stats_age_entreprise_tbl <- stats_age_entreprise_tbl %>% mutate(proportion = count/sum(count))
stats_age_entreprise_tbl %>% 
  mutate(AGE_ENTREPRISE = as.factor(AGE_ENTREPRISE)%>% fct_reorder(proportion))  %>% 
  ggplot(aes(x=AGE_ENTREPRISE, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Company age among 2017-2019 interns",
       x = "Company age",
       y= "")

stats_age_tbl <- read.csv("evol_stagiaires_2019_non_2022/categorie_age.csv")
stats_age_tbl <- stats_age_tbl %>% mutate(proportion = count/sum(count)) %>% filter(proportion>0.001)
stats_age_tbl  %>% 
  ggplot(aes(x=categorie_age, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Age category among 2019 interns - not there in 2022",
       x = "Age category",
       y= "")

stats_commanditaire_tbl <- read.csv("Stats_echantillon_etude/statistiques_commanditaire.csv")
stats_commanditaire_tbl <- stats_commanditaire_tbl %>% mutate(proportion = count/sum(count))
stats_commanditaire_tbl %>% 
  ggplot(aes(x=COMMANDITAIRE, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Sponsor among 2017-2019 interns",
       x = "Sponsor",
       y= "")

  stats_diplome_tbl <- read.csv("evol_stagiaires_2019_non_2022/niv_diplome.csv")
stats_diplome_tbl <- stats_diplome_tbl %>% mutate(proportion = count/sum(count))
stats_diplome_tbl %>%  mutate(NIV_DIPLOME = c("1", "2", "3", "4", "5", "6", "7")) %>% 
  ggplot(aes(x=NIV_DIPLOME, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Education level among 2019 interns - not there in 2022",
       x = "Education level",
       y= "")


stats_industrie_tbl <- read.csv("evol_stagiaires_2019_non_2022/salaire_A21_ET.csv")
stats_industrie_tbl <- stats_industrie_tbl %>% mutate(proportion = count/sum(count))
stats_industrie_tbl %>% 
  ggplot(aes(x=A21_ET, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Industry among 2019 interns - not there in 2022",
       x = "Industry",
       y= "")

stats_sexe_tbl <- read.csv("evol_stagiaires_2019_non_2022/salaires_sexe.csv")
stats_sexe_tbl <- stats_sexe_tbl %>% mutate(proportion = count/sum(count))
stats_sexe_tbl %>% 
  ggplot(aes(x=SEXE.x, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Sex among 2019 interns - not there in 2022",
       x = "Sex",
       y= "")

stats_region_tbl <- read.csv("Stats_echantillon_etude/statistiques_region.csv")
stats_region_tbl <- stats_region_tbl %>% mutate(proportion = count/sum(count))
stats_region_tbl %>% mutate(REGION_HABITATION = as.character(c(1:19))) %>% 
  ggplot(aes(x=REGION_HABITATION, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Region of 2017-2019 interns",
       x = "Region",
       y= "")

stats_remuneration_tbl <- read.csv("Stats_echantillon_etude/statistiques_remuneration.csv")
stats_remuneration_tbl <- stats_remuneration_tbl %>% mutate(proportion = count/sum(count))
stats_remuneration_tbl %>% mutate(TYPE_REMUNERATION = c("1", "2", "3", "4", "5", "6", "7")) %>% 
  ggplot(aes(x=TYPE_REMUNERATION, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Origin of earnings among 2017-2019 interns",
       x = "Origin of earnings",
       y= "")

stats_duree_formation_tbl <- read.csv("evol_stagiaires_2019_non_2022/decile_heures_formation.csv")
stats_duree_formation_tbl <- stats_duree_formation_tbl %>% mutate(proportion = count/sum(count))
stats_duree_formation_tbl %>% mutate(DUREE_FORMATION_DECILE = as.factor(DUREE_FORMATION_DECILE)) %>% 
  ggplot(aes(x=DUREE_FORMATION_DECILE, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Duration of internship among 2019 interns - not there in 2022",
       x = "Duration of internship",
       y = "")

stats_domaine_formation_tbl <- read.csv("evol_stagiaires_2019_non_2022//")
stats_domaine_formation_tbl <- stats_domaine_formation_tbl %>% mutate(proportion = count/sum(count))
stats_domaine_formation_tbl %>% mutate(DOMAINE_FORMATION = c("A", "B", "C", "D", "E", "F", "G", "H", "J", "L", "M", "O")) %>% 
  ggplot(aes(x=DOMAINE_FORMATION, y=count))+
  geom_col(fill="blue")+
  geom_text(aes(label= scales::percent(proportion)), vjust=-0.5)+
  labs(title = "Internship thema among 2017-2019 interns",
       x = "Internship thema",
       y = "")  

# Stats evol salaires 2019- 2022 - stagiaires

evol_salaires_2019_2022_age <- read.csv("evol_stagiaires_2019_2022/salaires_age.csv")
evol_salaires_2019_2022_age%>% mutate(mean_augmentation_2022=round(mean_augmentation_2022, digits=3)) %>% 
  ggplot(aes(categorie_age, mean_salary_2022))+
  geom_col(fill="blue")+
  geom_text(aes(label=scales::percent(mean_augmentation_2022)), vjust=-0.5)+
  labs(title= "Mean salary in 2022 among 2019 workers by age catagory\n and percent of increase compared to 2019",
       x = "Age Category",
       y = "Mean salary in 2022")

evol_salaires_2019_2022_industrie <- read.csv("evol_stagiaires_2019_2022/salaire_A21_ET.csv")
evol_salaires_2019_2022_industrie %>% mutate(mean_augmentation_2022=round(mean_augmentation_2022, digits=3)) %>% 
  ggplot(aes(A21_ET, mean_salary_2022))+
  geom_col(fill="blue")+
  geom_text(aes(label=(scales::percent(mean_augmentation_2022))), vjust=-0.5)+
  labs(title= "Mean salary in 2022 among 2019 workers by industry\n and percent of increase compared to 2019",
       x = "industry",
       y = "Mean salary in 2022")

evol_salaires_2019_2022_diplome <- read.csv("evol_stagiaires_2019_2022/salaires_diplome.csv")
evol_salaires_2019_2022_diplome %>% mutate(mean_augmentation_2022=round(mean_augmentation_2022, digits=3)) %>%
  mutate(NIV_DIPLOME = c("1", "2", "3", "4", "5", "6", "7")) %>% 
  ggplot(aes(NIV_DIPLOME, mean_salary_2022))+
  geom_col(fill="blue")+
  geom_text(aes(label=(scales::percent(mean_augmentation_2022))), vjust=-0.5)+
  labs(title= "Mean salary in 2022 among 2019 workers by education level\n and percent of increase compared to 2019",
       x = "Education level",
       y = "Mean salary in 2022")

evol_salaires_2019_2022_duree_formation <- read.csv("evol_stagiaires_2019_2022/salaires_duree_formation.csv")
evol_salaires_2019_2022_duree_formation %>% mutate(mean_augmentation_2022=round(mean_augmentation_2022, digits=3)) %>%
  mutate(DUREE_FORMATION_DECILE = as.factor(DUREE_FORMATION_DECILE)) %>% 
  ggplot(aes(DUREE_FORMATION_DECILE, mean_salary_2022))+
  geom_col(fill="blue")+
  geom_text(aes(label=(scales::percent(mean_augmentation_2022))), vjust=-0.5)+
  labs(title= "Mean salary in 2022 among 2019 workers by traineeship length\n and percent of increase compared to 2019",
       x = "Traineeship length",
       y = "Mean salary in 2022")

evol_salaires_2019_2022_sexe <- read.csv("evol_stagiaires_2019_2022/salaires_sexe.csv")
evol_salaires_2019_2022_sexe %>% mutate(mean_augmentation_2022=round(mean_augmentation_2022, digits=3)) %>%
  ggplot(aes(SEXE.x, mean_salary_2022))+
  geom_col(fill="blue")+
  geom_text(aes(label=(scales::percent(mean_augmentation_2022))), vjust=-0.5)+
  labs(title= "Mean salary in 2022 among 2019 workers by sex\n and percent of increase compared to 2019",
       x = "SEX",
       y = "Mean salary in 2022")

evol_salaires_2019_2022_region <- read.csv("evol_stagiaires_2019_2022/salaires_region.csv")
evol_salaires_2019_2022_region %>% mutate(mean_augmentation_2022=round(mean_augmentation_2022, digits=3)) %>%
  mutate(REGION_HABITATION = as.character(c(1:17))) %>% 
  ggplot(aes(REGION_HABITATION, mean_salary_2022))+
  geom_col(fill="blue")+
  geom_text(aes(label=(scales::percent(mean_augmentation_2022))), vjust=-0.5)+
  labs(title= "Mean salary in 2022 among 2019 workers by region\n and percent of increase compared to 2019",
       x = "Region",
       y = "Mean salary in 2022")

evol_salaires_2019_2022_type_remuneration <- read.csv("evol_stagiaires_2019_2022/salaires_type_remuneration.csv")
evol_salaires_2019_2022_type_remuneration %>% mutate(mean_augmentation_2022=round(mean_augmentation_2022, digits=3)) %>%
  mutate(TYPE_REMUNERATION = c("1", "2", "3", "4", "5", "6", "7")) %>% 
  ggplot(aes(TYPE_REMUNERATION, mean_salary_2022))+
  geom_col(fill="blue")+
  geom_text(aes(label=(scales::percent(mean_augmentation_2022))), vjust=-0.5)+
  labs(title= "Mean salary in 2022 among 2019 workers by origin of earnings\n and percent of increase compared to 2019",
       x = "Origin of earnings",
       y = "Mean salary in 2022")


# Stats evol salaires 2019- 2022 - non stagiaires

evol_salaires_2019_2022_region <- read.csv("evol_non_stagiaires_2019_2022/REGION_ET.csv")
evol_salaires_2019_2022_region %>% mutate(mean_augmentation_202=round(mean_augmentation_202, digits=2)) %>%
  mutate(REGION_HABITATION = as.character(c(1:19))) %>% 
  ggplot(aes(REGION_HABITATION, mean_salary_2022))+
  geom_col(fill="blue")+
  geom_text(aes(label=(scales::percent(mean_augmentation_202))), vjust=-0.5)+
  labs(title= "Mean salary in 2022 among 2019 non trainees by region\n and percent of increase compared to 2019",
       x = "Region",
       y = "Mean salary in 2022")

evol_salaires_2019_2022_age <- read.csv("evol_non_stagiaires_2019_2022/salaires_age.csv")
evol_salaires_2019_2022_age %>% mutate(mean_augmentation_202=round(mean_augmentation_202, digits=2)) %>%
  ggplot(aes(categorie_age, mean_salary_2022))+
  geom_col(fill="blue")+
  geom_text(aes(label=(scales::percent(mean_augmentation_202))), vjust=-0.5)+
  labs(title= "Mean salary in 2022 among 2019 non trainees by age\n and percent of increase compared to 2019",
       x = "Age category",
       y = "Mean salary in 2022")

evol_salaires_2019_2022_industrie <- read.csv("evol_non_stagiaires_2019_2022/salaires_A21_ET.csv")
evol_salaires_2019_2022_industrie %>% mutate(mean_augmentation_202=round(mean_augmentation_202, digits=3)) %>%
  ggplot(aes(A21_ET, mean_salary_2022))+
  geom_col(fill="blue")+
  geom_text(aes(label=(scales::percent(mean_augmentation_202))), vjust=-0.5)+
  labs(title= "Mean salary in 2022 among 2019 non trainees by industry\n and percent of increase compared to 2019",
       x = "Industry",
       y = "Mean salary in 2022")

# Mois travailles 2019 - 2022, stagiaires et non stagiaires:


mois_travailles_stagiaires_2019 <- read.csv("evol_stagiaires_2019_2022/mois_travailles_2019.csv")
mois_travailles_stagiaires_2019 %>% 
  ggplot(aes(mois_travailles_2019, proportion))+
  geom_col(fill="blue")+
  geom_text(aes(label=(scales::percent(proportion))), vjust=-0.5)+
  labs(title= "Proportion of workers by number of months worked in 2019\n among 2019 trainees",
       x = "Number of months worked",
       y = "Porportion")

mois_travailles_stagiaires_2022 <- read.csv("evol_stagiaires_2019_2022/mois_travailles_2022.csv")
mois_travailles_stagiaires_2022 %>% 
  ggplot(aes(mois_travailles_2022, proportion))+
  geom_col(fill="blue")+
  geom_text(aes(label=(scales::percent(proportion))), vjust=-0.5)+
  labs(title= "Proportion of workers by number of months worked in 2022\n among 2019 trainees",
       x = "Number of months worked",
       y = "Porportion")

mois_travailles_non_stagiaires_2022 <- read.csv("evol_non_stagiaires_2019_2022/mois_travailles_2022.csv")
mois_travailles_non_stagiaires_2022 %>% 
  ggplot(aes(mois_travailles_2022, proportion))+
  geom_col(fill="blue")+
  geom_text(aes(label=(scales::percent(proportion))), vjust=-0.5)+
  labs(title= "Proportion of workers by number of months worked in 2022\n among 2019 non trainees",
       x = "Number of months worked",
       y = "Porportion")
