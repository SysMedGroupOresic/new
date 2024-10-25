#install.packages("remotes")
#remotes::install_github("davidsjoberg/ggsankey")

library(ggsankey)
library(ggplot2)
library(dplyr)
library(ggsankeyfier)
library(ggplot2)
theme_set(theme_light())
data("ecosystem_services")

#Create data which can be used for Sankey
set.seed(111)

#Trying to imitate the approach below:
u3=all_all1; d='t'
c1=c()
ACMEMedian=c();ACMEpval=c();ACMEVar=c()
ADEMedian=c();ADEpval=c();ADEVar=c()
c1= u3 #[u3[,'ADE'] < ADEMedian  & DV<ADEVar,] #& u3[,'z0.p']<ADEpval
# ACMEMedian=0#median(c1[,'ACME'][c1[,'ACME']>0])
c1=c1[rev(order(c1[,'ACME'])),];  
c1=c1[((c1[,'ACME']-c1[,'ADE']) > 0), ] #c1[,'ACME']>ACMEMedian & 
c1=c1[c1[,'d0.p']<0.05, ] # c1=tryCatch({c1[1:lkm,]}, error = function(msg){return(c1)})
uh7ma = na.omit(c1)
rt2=uh7ma #[,1:17]# rtot=rtot[,1:17]# rtot=data.frame(rtot) # name=paste(simss,'basic hypothesis',take)# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/p.adjust.html# https://www.middleprofessor.com/files/applied-biostatistics_bookdown/_book/adding-covariates-to-a-linear-model# https://github.com/MarioniLab/miloR# https://www.nature.com/articles/s41467-023-40458-9/figures/4
name=paste('Contaminants_Steroids_BAs_or_Lipids_sims',date) # rtot=rtot_2000_mrct # rtot=uh5

hoi=c(); 
if (d=='t') {hoi=scan(text=rt2[,1] , what=" ")} else {hoi=scan(text=rownames(rt2) , what=" ")} #rownames(rt2)# names(rt2[,1]) rownames(rt2)
# hoi=scan(text=rownames(rt2) , what=" ")#rownames(rt2)# names(rt2[,1]) rownames(rt2)
hoi=as.data.frame(matrix(hoi, ncol = 3,  byrow = TRUE), stringsAsFactors = FALSE) #Check this number (ncol) 3/4
hoi=cbind(hoi[,1:3],rt2[,2])
colnames(hoi)=c('Contaminants','Steroids','Bile Acids or Lipids','Weight')#,'Gender') ##https://stats.stackexchange.com/questions/282155/causal-mediation-analysis-negative-indirect-and-total-effect-positive-direct# https://www.researchgate.net/post/How_can_I_interpret_a_negative_indirect_effect_for_significant_mediation# https://stackoverflow.com/questions/31518150/gsub-in-r-is-not-replacing-dot replacing dot

hoi[,'Steroids' ][hoi[,'Steroids' ]=='17aOH.P4']='17a-OHP4'
hoi[,'Steroids' ][hoi[,'Steroids' ]=='17aOH-P4']='17a-OHP4'
hoi[,'Steroids' ]  <- gsub("\\.", "-",  hoi[,'Steroids' ] ) #:)
hoi[,'Steroids' ][ hoi[,'Steroids' ]=='T-Epi-T']='T/Epi-T'
  
df22 <-
  hoi |>
  subset(Weight > -1.05) |>
  pivot_stages_longer(c("Contaminants", "Steroids", "Bile Acids or Lipids"), "Weight", "Bile Acids or Lipids")
pos <- position_sankey(v_space = "auto", order = "ascending", align = "justify")
p <-
  ggplot(
    data    = df22,
    mapping = aes(x = stage, y = Weight, group = node,
                  edge_id = edge_id, connector = connector))+ theme_sankey(base_size = 28) + #theme(legend.position = "none") +
  # scale_fill_grey(start = 0.5, end = 0.5)+
  theme(axis.text.x = element_text(hjust = 0.5, vjust=7,colour = 'black') )+ #https://stackoverflow.com/questions/38862303/customize-ggplot2-axis-labels-with-different-colors
  theme(axis.title.x = element_blank())


pos <- position_sankey(v_space = "auto", order = "descending")
p + geom_sankeyedge(aes(fill = Weight), position = pos) +
  geom_sankeynode(position = pos,fill = "#dfe0e6") +
  geom_text(aes(label = node), stat = "sankeynode", position = pos, cex = 2)+scale_fill_viridis_c(option = "turbo")
  # geom_sankey_label(size = 8.0, color = 1, fill = "white")


## Let's subset the example data to create a less cluttered
## Sankey diagram
########this works!!!!!###########:
# es_sub <-
#   ecosystem_services |>
#   subset(RCSES > 0.005) |>
#   pivot_stages_longer(c("activity_realm", "biotic_realm", "service_section"), "RCSES", "service_section")
# 
# pos <- position_sankey(v_space = "auto", order = "ascending", align = "justify")
# 
# p <-
#   ggplot(
#     data    = es_sub,
#     mapping = aes(x = stage, y = RCSES, group = node,
#                   edge_id = edge_id, connector = connector))
# p +
#   geom_sankeyedge(aes(fill = RCSES), position = pos) +
#   geom_sankeynode(position = pos) +
#   scale_fill_viridis_c(option = "turbo")
########!!!!!###########


# Other
# install.packages("ggalluvial")
# library(ggalluvial)

# to do it for some names in a vector named 'col_names'
# col_names <- colnames(hoi[,1:3])
# hoi[col_names] <- lapply(hoi[col_names] , factor)
# hoie=tibble(hoi) #https://stackoverflow.com/questions/9251326/convert-data-frame-column-format-from-character-to-factor


# ggplot(data = vaccinations,
#        aes(axis1 = survey,   # First variable on the X-axis
#            axis2 = response, # Second variable on the X-axis
#            axis3 = survey,   # Third variable on the X-axis
#            y = freq)) +
#   geom_alluvium(aes(fill = response)) +
#   geom_stratum() +
#   geom_text(stat = "stratum",
#             aes(label = after_stat(stratum))) +
#   scale_x_discrete(limits = c("Survey", "Response"),
#                    expand = c(0.15, 0.05)) +
#   theme_void()

# install.packages('networkD3') #https://r-graph-gallery.com/322-custom-colours-in-sankey-diagram.html
# 
# 
# # Library
# library(networkD3)
# library(dplyr)
# 
# # Make a connection data frame
# links <- data.frame(
#   source=c("group_A","group_A", "group_B", "group_C", "group_C", "group_E"), 
#   target=c("group_C","group_D", "group_E", "group_F", "group_G", "group_H"), 
#   value=c(2,3, 2, 3, 1, 3)
# )

# # From these flows we need to create a node data frame: it lists every entities involved in the flow
# nodes <- data.frame(
#   name=c(as.character(links$source), as.character(links$target)) %>% 
#     unique()
# )
# 
# # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
# links$IDsource <- match(links$source, nodes$name)-1 
# links$IDtarget <- match(links$target, nodes$name)-1
#  
# # prepare color scale: I give one specific color for each node.
# my_color <- 'd3.scaleOrdinal() .domain(["group_A", "group_B","group_C", "group_D", "group_E", "group_F", "group_G", "group_H"]) .range(["blue", "blue" , "blue", "red", "red", "yellow", "purple", "purple"])'
#  
# # Make the Network. I call my colour scale with the colourScale argument
# p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
#               Value = "value", NodeID = "name", colourScale=my_color)
# p
# 
# 
# # Add a 'group' column to the nodes data frame:
# nodes$group <- as.factor(c("a","a","a","a","a","b","b","b"))
#  
# # Give a color for each group:
# my_color <- 'd3.scaleOrdinal() .domain(["a", "b"]) .range(["#69b3a2", "steelblue"])'
#  
# # Make the Network
# p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
#               Value = "value", NodeID = "name", 
#               colourScale=my_color, NodeGroup="group")
# p
# 
# # save the widget
# # library(htmlwidgets)
# # saveWidget(p, file=paste0( getwd(), "/HtmlWidget/sankeyColor2.html"))
# 
# # Add a 'group' column to each connection:
# links$group <- as.factor(c("type_a","type_a","type_a","type_b","type_b","type_b"))
#  
# # Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey
# nodes$group <- as.factor(c("my_unique_group"))
#  
# # Give a color for each group:
# my_color <- 'd3.scaleOrdinal() .domain(["type_a", "type_b", "my_unique_group"]) .range(["#69b3a2", "steelblue", "grey"])'
#  
# # Make the Network
# p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
#                    Value = "value", NodeID = "name", 
#                    colourScale=my_color, LinkGroup="group", NodeGroup="group")
# 
# p


