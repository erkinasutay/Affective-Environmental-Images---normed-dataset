rm(list=ls())

library(tidyverse)
library(rstatix)
library(psych)
library(lme4)
library(sjPlot)
library(ggeffects)
library(ggpubr)

load("OSFdata.rdata")
# clean the data 

# remove those failing attention check and 
# flagged by the bot detection algoritm in Qualtrics
rawData = rawData %>%
  filter(attention_check == 3) %>%
  filter(botFlag==0)


# Calculate omegas and aggregate measures over individuals
omegas = list(
  risk = omega(rawData %>% select(c(risk1, risk2, risk3)), flip = F)$omega.tot,
  hedonic = omega(rawData %>% select(c(hed1, hed2, hed3)), flip = F)$omega.tot,
  egoistic = omega(rawData %>% select(c(ego1, ego2, ego3, ego4, ego5)), flip = F)$omega.tot,
  altruistic = omega(rawData %>% select(c(alt1, alt2, alt4, alt5)), flip = F)$omega.tot,
  biospheric = omega(rawData %>% select(c(bio1, bio2, bio3, bio4)), flip = F)$omega.tot 
)
omegas

rawData = rawData %>% 
  rowwise() %>% mutate(riskCC = mean(c(risk1, risk2, risk3))) %>%
  rowwise() %>% mutate(hedonic = mean(c(hed1, hed2, hed3))) %>%
  rowwise() %>% mutate(egoistic = mean(c(ego1, ego2, ego3, ego4, ego5))) %>%
  rowwise() %>% mutate(altruistic = mean(c(alt1, alt2, alt4, alt5))) %>%
  rowwise() %>% mutate(biospheric = mean(c(bio1, bio2, bio3, bio4)))

# recode values from 1-9 to (-1)-7
rawData = rawData %>%
  mutate(biospheric = biospheric-2) %>%
  mutate(altruistic = altruistic-2) %>%
  mutate(egoistic = egoistic-2) %>%
  mutate(hedonic = hedonic-2)

# belief in climate change
# 1 = no climate change
# 2 = not sure
# 3 = there is climate change but not man made
# 4 = climate change is man-made
rawData%>%count(CC_belief)

# means, sds, and correlations (data presented in TABLE 2 in the manuscript)
rawData %>% get_summary_stats(riskCC, biospheric, altruistic, egoistic, hedonic, type = "mean_sd")
print(corr.test(rawData%>%select(c(riskCC, biospheric, altruistic, egoistic, hedonic)),
          method = "pearson", adjust = "fdr"), short=F)


##############################################################################
# pivot longer and calculate image stats

data = rawData %>%
  select(contains(c("pid","age","gender","CC",
                    "biospheric","altruistic","egoistic","hedonic",
                    ".valence",".arousal",".relevance",".trial",".IM"))) %>%
  select(-c(UserLanguage,PROLIFIC_PID))


data = data %>%
  pivot_longer(cols = contains(c("valence","arousal","relevance","trial","IM")),
               names_to = c("trialCode",".value"),
               names_sep = "\\.")

data = data %>% 
  mutate(relevance = ifelse(relevance<=2, relevance, relevance-3))

# Calculate image stats here
image_ratings = data %>% group_by(IM) %>% get_summary_stats(valence,arousal,relevance, type = "mean_sd") %>%
  pivot_wider(names_from = variable, values_from = c(n,mean,sd))

image_ratings = image_ratings %>%
  select(-c(n_relevance,n_arousal)) %>%
  rename(n = n_valence) %>%
  rename_with(~gsub("mean_","",.x, fixed = T),contains("mean_"))

# extract image ID from the URL
image_ratings = image_ratings %>%
  mutate(imgID = str_remove(IM, ".*=")) 

imageStats = inner_join(image_ratings, imageLabels, by="imgID")
rm(image_ratings,imageLabels)

# CORRELATIONS - information presented in TABLE 3
imageStats %>% get_summary_stats(valence, arousal, relevance, type = "mean_sd")
print(corr.test(imageStats%>%select(c(valence, arousal, relevance)),
                method = "pearson", adjust = "holm"), short=F)

# Figure 1 in the manuscript
fig1 = ggplot(imageStats) + 
  geom_point(aes(valence , arousal), shape=1, size=3)+
  scale_x_continuous(name = "Pleasantness",
                     limits = c(1,9),
                     breaks = c(1,3,5,7,9)) +
  scale_y_continuous(name = "Activation",
                     limits = c(1,9),
                     breaks = c(1,3,5,7,9)) +
  theme_bw()
fig1

# Figure S1 in the supplement
a1=ggplot(imageStats) + 
  geom_point(aes(relevance , valence), shape=1, size=3)+
  scale_y_continuous(name = "Pleasantness",
                     limits = c(1,9),
                     breaks = c(1,3,5,7,9)) +
  scale_x_continuous(name = "Relevance",
                     limits = c(1,5),
                     breaks = c(1,3,5)) +
  theme_bw()

r1=ggplot(imageStats) + 
  geom_point(aes(relevance, arousal), shape=1, size=3)+
  scale_y_continuous(name = "Activation",
                     limits = c(1,9),
                     breaks = c(1,3,5,7,9)) +
  scale_x_continuous(name = "Relevance",
                     limits = c(1,5),
                     breaks = c(1,3,5)) +
  theme_bw()


figS1 = ggpubr::ggarrange(fig1,a1,r1, nrow = 1, labels = c("A","B","C"))
figS1


# Linear vs quadratic relationship between valence and arousal
linMod = lm(arousal ~ 1+valence, data=imageStats)
polyMod = lm(arousal ~ 1+poly(valence,2), data=imageStats)
anova(linMod,polyMod)

# sjPlot::tab_model(linMod,polyMod, show.aicc = T)


c1=ggplot(imageStats,aes(valence , arousal)) + 
  geom_point( shape=1, size=3)+
  geom_smooth(method = "lm", formula = y~x) +
  scale_x_continuous(name = "Pleasantness",
                     limits = c(1,9),
                     breaks = c(1,3,5,7,9)) +
  scale_y_continuous(name = "Activation",
                     limits = c(1,9),
                     breaks = c(1,3,5,7,9)) +
  theme_bw()

c2=ggplot(imageStats,aes(x=valence , y=arousal)) + 
  geom_point( shape=1, size=3)+
  geom_smooth(method = "lm", formula = y~poly(x,2), level=.99) +
  scale_x_continuous(name = "Pleasantness",
                     limits = c(1,9),
                     breaks = c(1,3,5,7,9)) +
  scale_y_continuous(name = "Activation",
                     limits = c(1,9),
                     breaks = c(1,3,5,7,9)) +
  theme_bw()
ggpubr::ggarrange(c1,c2,labels = c("A","B"))

# figure 1 with the quadratic regression line
fig1 = c2
fig1


################################################################################
# 2way ANOVAs: image valence X human aspect
# presened in the supplementary analyses

imageStats %>% anova_test(valence ~ IMval*human_aspect)
imageStats %>%
  group_by(IMval) %>%
  pairwise_t_test(valence ~ human_aspect, p.adjust.method = "fdr")

imageStats %>% anova_test(arousal ~ IMval*human_aspect)
imageStats %>% 
  group_by(IMval) %>%
  pairwise_t_test(arousal ~ human_aspect, p.adjust.method = "fdr")

imageStats %>% anova_test(relevance ~ IMval*human_aspect)
imageStats %>%
  group_by(IMval) %>%
  pairwise_t_test(relevance ~ human_aspect, p.adjust.method = "fdr")


# Figure S2 in the supplementary materials
imageStats = imageStats %>% 
  mutate(`Human aspect` = ifelse(human_aspect==1, "Yes", "No"))
vB = ggplot(imageStats, aes(x=IMval, y=valence, group=`Human aspect`, color=`Human aspect`)) + 
  geom_point(position = position_dodge2(0.5), alpha=0.2) + 
  stat_summary(fun.data = "mean_cl_boot", linewidth=0.7, shape="_", size=3, position = position_dodge2(0.5)) + 
  scale_x_discrete(name = "Image valence", label = c("Negative","Positive")) + 
  scale_y_continuous(name = "Pleasantness", limits = c(1,9), breaks = c(1,3,5,7,9)) + 
  theme_bw() + theme(legend.position = "bottom")

aB = ggplot(imageStats, aes(x=IMval, y=arousal, group=`Human aspect`, color=`Human aspect`)) + 
  geom_point(position = position_dodge2(0.5), alpha=0.2) + 
  stat_summary(fun.data = "mean_cl_boot", linewidth=0.7, shape="_", size=3, position = position_dodge2(0.5)) + 
  scale_x_discrete(name = "Image valence", label = c("Negative","Positive")) + 
  scale_y_continuous(name = "Activation", limits = c(1,9), breaks = c(1,3,5,7,9)) + 
  theme_bw() + theme(legend.position = "bottom")

rB = ggplot(imageStats, aes(x=IMval, y=relevance, group=`Human aspect`, color=`Human aspect`)) + 
  geom_point(position = position_dodge2(0.5), alpha=0.2) + 
  stat_summary(fun.data = "mean_cl_boot", linewidth=0.8, shape="_", size=3, position = position_dodge2(0.5)) + 
  scale_x_discrete(name = "Image valence", label = c("Negative","Positive")) + 
  scale_y_continuous(name = "Relevance", limits = c(1,5), breaks = c(1,3,5)) + 
  theme_bw() + theme(legend.position = "bottom")

figS2 = ggpubr::ggarrange(vB, aB, rB, nrow = 1, labels = c("A","B","C"))
figS2


panelA = fig1 + theme(axis.title = element_text(size = 20),
                    axis.text = element_text(size=16))
panelB = r1 + theme(axis.title = element_text(size = 16),
                    axis.text = element_text(size=12))
panelC = a1 + theme(axis.title = element_text(size = 16),
                    axis.text = element_text(size=12))
panelD = vB + theme(axis.title = element_text(size = 20),
                    axis.text = element_text(size=16))
panelE = aB + theme(axis.title = element_text(size = 20),
                    axis.text = element_text(size=16))
panelF = rB + theme(axis.title = element_text(size = 20),
                    axis.text = element_text(size=16))

f1=ggarrange(ggarrange(panelA, ggarrange(panelB,panelC,nrow = 2, labels = c("B","C"),font.label = list(size=20)), 
          nrow = 1, widths = c(2,1), labels = "A",font.label = list(size=20)),
          ggarrange(panelD,panelE,panelF, nrow = 1, labels = c("D","E","F"),font.label = list(size=20)),
          nrow=2, heights = c(1,0.75))



ggsave("figX.tiff",
       plot = f1,
       width=2700,
       height = 3200,
       units = "px",
       dpi=300,
       bg="white",
       compression="lzw+p")

############################################################################
# Liear mixed models looking into individual differences in affective reactions 
# based on biospheric values and perceived risk of climate change

data = data%>%
  mutate(imgGroup = ifelse(str_detect(trialCode,"neg"), "neg", "pos")) %>%
  mutate(female = ifelse(gender==2,1,0)) %>%
  mutate(riskCC2 = (riskCC/12.5)+1)

data = data %>%
  mutate(logTrial = log(trialNo))

valence.model = lmer(valence ~ 1 + imgGroup*biospheric + imgGroup*riskCC2 + 
                       logTrial + (1|pid) + (1|IM),
                     data=data, control = lmerControl(optimizer = "bobyqa"))

arousal.model = lmer(arousal ~ 1 + imgGroup*biospheric + imgGroup*riskCC2 + 
                       logTrial + (1|pid) + (1|IM),
                     data=data, control = lmerControl(optimizer = "bobyqa"))

relevance.model = lmer(relevance ~ 1 + imgGroup*biospheric + imgGroup*riskCC2 +
                         logTrial + (1|pid) + (1|IM),
                       data=data, control = lmerControl(optimizer = "bobyqa"))

# TABLE 4 in the manuscript
tab_model(valence.model,arousal.model,relevance.model, p.adjust = "fdr")

# Preparation of Figure 2 in the manuscript
v1=as_tibble(ggpredict(valence.model, terms = c("imgGroup","biospheric"))) %>%
  rename(`Image Valence`=x, Valence=predicted) %>% mutate(Factor = "Biospheric Values")
levels(v1$group)=c(1,2,3)

v2 = as_tibble(ggpredict(valence.model, terms = c("imgGroup","riskCC2")))  %>%
  rename(`Image Valence`=x, Valence=predicted) %>% mutate(Factor = "Perceived Risk")
levels(v2$group)=c(1,2,3)

val.pred = rbind(v1,v2) %>%
  mutate(`Image Valence` = ifelse(`Image Valence`=="pos","Positive","Negative"))
                          
valence.fig=ggplot(val.pred, aes(x=group, y=Valence, 
                     ymin=conf.low, ymax=conf.high, 
                     fill=`Image Valence`, group=`Image Valence`)) + 
  geom_line() + geom_ribbon(alpha=0.2) + facet_wrap(~Factor)+
  ylim(c(1,9))+ theme_bw() +
  theme(legend.position = 'none',
        axis.title.x = element_blank()) + 
  scale_x_discrete(labels=c("-1 SD", "Mean", "+1 SD"))
  

a1=as_tibble(ggpredict(arousal.model, terms = c("imgGroup","biospheric"))) %>%
  rename(`Image Valence`=x, Arousal=predicted) %>% mutate(Factor = "Biospheric Values")
levels(a1$group)=c(1,2,3)

a2 = as_tibble(ggpredict(arousal.model, terms = c("imgGroup","riskCC2")))  %>%
  rename(`Image Valence`=x, Arousal=predicted) %>% mutate(Factor = "Perceived Risk")
levels(a2$group)=c(1,2,3)

aro.pred = rbind(a1,a2) %>%
  mutate(`Image Valence` = ifelse(`Image Valence`=="pos","Positive","Negative"))

arousal.fig = ggplot(aro.pred, aes(x=group, y=Arousal, 
                     ymin=conf.low, ymax=conf.high,
                     fill=`Image Valence`, group=`Image Valence`)) + 
  geom_line() + geom_ribbon(alpha=0.2) + facet_wrap(~Factor) +
  ylim(c(1,9)) + theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_x_discrete(labels=c("-1 SD","Mean","+1 SD"))

fig2 = ggarrange(valence.fig,arousal.fig,nrow = 1,labels = c("A","B"))
fig2

# r1=as_tibble(ggpredict(relevance.model, terms = c("imgGroup","biospheric"))) %>%
#   rename(`Image Valence`=x, Relevance=predicted) %>% mutate(Factor = "Biospheric Values") 
# levels(r1$group)=c(1,2,3)
# 
# r2 = as_tibble(ggpredict(relevance.model, terms = c("imgGroup","riskCC2")))  %>%
#   rename(`Image Valence`=x, Relevance=predicted) %>% mutate(Factor = "Perceived Risk")
# levels(r2$group)=c(1,2,3)
# 
# rel.pred = rbind(r1,r2) %>%
#   mutate(`Image Valence` = ifelse(`Image Valence`=="pos","Positive","Negative"))
# 
# 
# relfig=ggplot(rel.pred, aes(x=group, y=Relevance, 
#                      ymin=conf.low, ymax=conf.high, 
#                      fill=`Image Valence`, group=`Image Valence`)) + 
#   geom_line() + geom_ribbon(alpha=0.2) + facet_wrap(~Factor) + 
#   ylim(c(1,5))+ theme_bw() + 
#   theme(legend.position = "none", 
#         axis.title.x = element_blank()) + 
#   scale_x_discrete(labels = c("-1 SD","Mean","+1 SD"))


ggarrange(valence.fig,arousal.fig,nrow = 1,labels = c("A","B"))



