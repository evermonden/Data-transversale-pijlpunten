# Importeer het excel bestand met de data
# Zorg ervoor dat de waarde voor de lengte-breedte index numeriek is
# Om dit te controleren, gebruik glimpse(Transversaal_pijlpunten)
# Als er achter lengte-breedte index <dbl> staat, is het goed

# Benodigde packages
library(tidyverse)
library(patchwork)

# Code gebruikt voor het maken van de grafieken in de tekst

  # Filter datasets

    # Op context (Hunebed en oppervlaktevondst)
TPhunebed <- Transversaal_pijlpunten %>% filter(Context=="Hunebed")
TPoppervlakte <- Transversaal_pijlpunten %>% filter(Context=="Oppervlaktevondst")

    # Op conditie (beschadigd vs onbeschadigd)
TP_hun_stuk <- TPhunebed %>% 
  filter(Conditie != "Compleet") %>% 
  filter(Conditie != "Archeologisch compleet")
TP_hun_com <- TPhunebed %>% 
  filter(Conditie != "Gebroken") %>% 
  filter(Conditie != "Onbekend")

TP_opp_stuk <- TPoppervlakte %>% 
  filter(Conditie != "Compleet") %>% 
  filter(Conditie != "Archeologisch compleet")
TP_opp_com <- TPoppervlakte %>% 
  filter(Conditie != "Gebroken") %>% 
  filter(Conditie != "Onbekend")

  # Scatter plots

# Lengte/breedte
  # Conditie hunebed context
sc_lbhun_com <- ggplot(TP_hun_com, aes(`Max. lengte`, `Max. breedte`))

p1 <- sc_lbhun_com + geom_point(aes(colour=Vindplaats), size=4, alpha=0.9) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), size=1) +
  scale_color_manual(values=c("#CC6677","#332288","#661100")) +
  xlim(10,32) +
  ylim(8,40) +
  guides(color = guide_legend(keywidth = 2.5)) +
  ylab("Breedte (mm)") +
  ggtitle("Onbeschadigde spitsen") +
  xlab("Lengte (mm)")

sc_lbhun_stuk <- ggplot(TP_hun_stuk, aes(`Max. lengte`, `Max. breedte`))

p2 <- sc_lbhun_stuk + geom_point(aes(colour=Vindplaats), size=4, alpha=0.9,show.legend = FALSE) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), size=1) +
  scale_color_manual(values=c("#CC6677","#661100")) +
  xlim(10,32) +
  ylim(8,40) +
  guides(color = guide_legend(keywidth = 2.5)) +
  ggtitle("Beschadigde spitsen") +
  ylab("Breedte (mm)") +
  xlab("Lengte (mm)")

  # Conditie oppervlakvondsten
sc_lbopp_com <- ggplot(TP_opp_com, aes(`Max. lengte`, `Max. breedte`))

p3 <- sc_lbopp_com + geom_point(aes(colour=Vindplaats), size=4, alpha=0.9, shape=17) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), size=1) +
  scale_color_manual(values=c("#88CCEE","#4cc8a3","#f6a824","#2973b8", "#F46D43",
                              "#882255","#117733")) +
  xlim(10,32) +
  ylim(8,40) +
  guides(color = guide_legend(keywidth = 3.36)) +
  ylab("Breedte (mm)") +
  xlab("Lengte (mm)") 

sc_lbopp_stuk <- ggplot(TP_opp_stuk, aes(`Max. lengte`, `Max. breedte`))

p4 <- sc_lbopp_stuk + geom_point(aes(colour=Vindplaats), size=4, alpha=0.9, shape=17,show.legend = FALSE) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), size=1) +
  scale_color_manual(values=c("#4cc8a3","#f6a824","#2973b8","#F46D43",
                              "#882255","#117733")) +
  xlim(10,32) +
  ylim(8,40) +
  guides(color = guide_legend(keywidth = 3.36)) +
  ylab("Breedte (mm)") +
  xlab("Lengte (mm)")

  # Multipanel grafiek
# Fig. 2
p1 + p2 + p3 + p4 + plot_layout(guides='collect') 

# Proximaal/distaal 
  # Conditie hunebed context
sc_pdhun_com <- ggplot(TP_hun_com, aes(`Proximale zijde`, `Distale zijde`))

p5 <- sc_pdhun_com + geom_point(aes(colour=Vindplaats), size=4, alpha=0.9) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), size=1) +
  scale_color_manual(values=c("#CC6677","#332288","#661100")) +
  xlim(9,34) +
  ylim(8,32) +
  guides(color = guide_legend(keywidth = 2.5)) +
  ggtitle("Onbeschadigde spitsen") + 
  xlab("Proximale zijde (mm)") +
  ylab("Distale zijde (mm)")

sc_pdhun_stuk <- ggplot(TP_hun_stuk, aes(`Proximale zijde`, `Distale zijde`))

p6 <- sc_pdhun_stuk + geom_point(aes(colour=Vindplaats), size=4, alpha=0.9, show.legend = FALSE) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), size=1) +
  scale_color_manual(values=c("#CC6677","#661100")) +
  xlim(9,34) +
  ylim(8,32) +
  guides(color = guide_legend(keywidth = 2.5)) +
  ggtitle("Beschadigde spitsen") + 
  xlab("Proximale zijde (mm)") +
  ylab("Distale zijde (mm)")

  # Conditie nederzetting context
sc_pdopp_com <- ggplot(TP_opp_com, aes(`Proximale zijde`, `Distale zijde`))

p7 <- sc_pdopp_com + geom_point(aes(colour=Vindplaats), size=4, alpha=0.9, shape=17) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), size=1) +
  scale_color_manual(values=c("#88CCEE","#4cc8a3","#f6a824","#2973b8", "#F46D43",
                              "#882255","#117733")) +
  xlim(9,34) +
  ylim(8,32) +
  guides(color = guide_legend(keywidth = 3.36)) + 
  xlab("Proximale zijde (mm)") +
  ylab("Distale zijde (mm)")

sc_pdopp_stuk <- ggplot(TP_opp_stuk,aes(`Proximale zijde`, `Distale zijde`))

p8 <- sc_pdopp_stuk + geom_point(aes(colour=Vindplaats), size=4, alpha=0.9, shape=17,show.legend = FALSE) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), size=1) +
  scale_color_manual(values=c("#4cc8a3","#f6a824","#2973b8","#F46D43",
                              "#882255","#117733")) +
  xlim(9,34) +
  ylim(8,32) +
  guides(color = guide_legend(keywidth = 3.36)) + 
  xlab("Proximale zijde (mm)") +
  ylab("Distale zijde (mm)")

  # Multipanel grafiek
# Fig. 6
p5 + p6 + p7 + p8 + plot_layout(guides='collect')

# Histogram

  # LB index hunebed context
lb1 <- ggplot(TPhunebed, aes(x=`Lengte-breedte index`,
                             fill=Context)) +
  scale_fill_manual(values="#CC6677") +
  geom_histogram(colour="black", bins=20,show.legend = FALSE) +
  geom_vline(xintercept=1,colour="black",linetype="longdash",size=1.1) +
  theme_minimal() +
  ylab("Aantal") +
  xlim(0.0,1.4) +
  ylim(0,30)

  # LB index oppervlakte vondsten
lb2 <- ggplot(TPoppervlakte, aes(x=`Lengte-breedte index`,
                                 fill=Context)) +
  scale_fill_manual(values="#88CCEE") +
  geom_histogram(colour="black", bins=20,show.legend = FALSE) +
  geom_vline(xintercept=1,colour="black",linetype="longdash",size=1.1) +
  theme_minimal() +
  ylab("Aantal") +
  xlim(0.0,1.4) +
  ylim(0,30)

  # Multipanel grafiek
# Fig. 3
lb1 + lb2 + plot_layout(guides='collect')

# Boxplots

  # Dikte per vindplaats
# Fig. 4
ggplot(Transversaal_pijlpunten,
       aes(`Max. dikte`, Context, fill=Vindplaats)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Set3") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), size=1)

  # Dikte per context
# Fig. 5
ggplot(Transversaal_pijlpunten,
       aes(`Max. dikte`, Context, fill=Context)) +
  geom_boxplot() +
  guides(color = guide_legend(reverse=TRUE)) +
  scale_color_manual(values=c("#88CCEE","#CC6677")) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), size=1) 

# Staafdiagrammen

  # Regelmatigheid per context
# Fig. 7
ggplot(Transversaal_pijlpunten, aes(fill=Context,x=Regelmatigheid)) +
  geom_bar(position="stack",show.legend = FALSE) +
  theme_minimal() +
  scale_fill_manual(values=c("#CC6677","#88CCEE")) +
  ylab("Aantal") +
  ylim(0,100)

# Extra figuren 

 # Zijde 1/zijde 2
  # Conditie hunebed context
sc_zhun_com <- ggplot(TP_hun_com, aes(`Zijde 1`,`Zijde 2`))

p9 <- sc_zhun_com + geom_point(aes(colour=Vindplaats), size=4, alpha=0.9) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), size=1) +
  scale_color_manual(values=c("#CC6677","#332288","#661100")) +
  xlim(9,34) +
  ylim(8,32) +
  guides(color = guide_legend(keywidth = 3.36)) + 
  ggtitle("Onbeschadigde spitsen") + 
  xlab("Zijde 1 (mm)") +
  ylab("Zijde 2 (mm)")

sc_zhun_stuk <- ggplot(TP_hun_stuk, aes(`Zijde 1`,`Zijde 2`))

p10 <- sc_zhun_stuk + geom_point(aes(colour=Vindplaats), size=4, alpha=0.9,show.legend = FALSE) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), size=1) +
  scale_color_manual(values=c("#CC6677","#661100")) +
  xlim(9,34) +
  ylim(8,32) +
  guides(color = guide_legend(keywidth = 3.36)) + 
  ggtitle("Beschadigde spitsen") + 
  xlab("Zijde 1 (mm)") +
  ylab("Zijde 2 (mm)")

  # Conditie nederzetting context
sc_zopp_com <- ggplot(TP_opp_com, aes(`Zijde 1`,`Zijde 2`))

p11 <- sc_zopp_com + geom_point(aes(colour=Vindplaats), size=4, alpha=0.9, shape=17) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), size=1) +
  scale_color_manual(values=c("#88CCEE","#4cc8a3","#f6a824","#2973b8", "#F46D43",
                              "#882255","#117733")) +
  xlim(9,34) +
  ylim(8,32) +
  guides(color = guide_legend(keywidth = 3.36)) + 
  xlab("Zijde 1 (mm)") +
  ylab("Zijde 2 (mm)")

sc_zopp_stuk <- ggplot(TP_opp_stuk, aes(`Zijde 1`,`Zijde 2`))

p12 <- sc_zopp_stuk + geom_point(aes(colour=Vindplaats), size=4, alpha=0.9, shape=17,show.legend = FALSE) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), size=1) +
  scale_color_manual(values=c("#4cc8a3","#f6a824","#2973b8","#F46D43",
                              "#882255","#117733")) +
  xlim(9,34) +
  ylim(8,32) +
  guides(color = guide_legend(keywidth = 3.36)) + 
  xlab("Zijde 1 (mm)") +
  ylab("Zijde 2 (mm)")

  # Multipanel grafiek
# Zijdes
p9 + p10 + p11 + p12 + plot_layout(guides='collect')

# Regelmatigheid per locatie

ggplot(Transversaal_pijlpunten, aes(fill=Vindplaats,x=Regelmatigheid)) +
  geom_bar(position="stack") +
  theme_minimal() +
  scale_fill_manual(values=c("#88CCEE","#CC6677","#332288","#4cc8a3",
                             "#f6a824","#661100","#2973b8","#F46D43",
                             "#882255","#117733")) +
  ylim(0,100)
