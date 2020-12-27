library(tidyverse)
library(ggpubr)
library(gridExtra)
library(lmtest)

# Load the data
mushroom <- read.csv("~/Desktop/Other/Mushroom Experiment/NEW STUFF/mushroom_complete.csv", header=TRUE) 

# Change to factors to make sure. Drop unused levels.
mushroom$Time <- as.factor(droplevels(as.factor(mushroom$Time)))
mushroom$Temperature <- droplevels(as.factor(mushroom$Temperature))
mushroom$Type <- droplevels(as.factor(mushroom$Type))

str(mushroom)

################################# Plots #####################################

# Plot change in weight histogram
ggplot(data = mushroom, aes(x = after - before)) +
  geom_histogram(aes(y = ..count..),
                 color = "black",
                 fill="#FF6666",
                 bins = 15) +
  labs(x = "Change in Weight",
       y = "Count") +
  theme_bw()

###################### Interaction Plots Using GGPLOT2 ########################
# Interaction for S*M
sm <- mushroom %>% 
  ggplot(aes(x = as.factor(Type), y = after - before, color = as.factor(Method), 
             group = as.factor(Method), linetype = as.factor(Method),
             shape = as.factor(Method))) + 
  stat_summary(fun.y = mean, geom = "line") +
  ylab("Change in Weight") + 
  xlab("Type") +
  scale_x_discrete(breaks = c("Button", "Shiitake"), 
                   labels=c("Button","Shiitake")) +
  labs(color = "Method\n") +
  
  scale_color_manual(name = "Method", labels = c("Rinse", "Soak"), values = c("blue", "red")) +
  scale_linetype_manual(name = "Method", labels = c("Rinse", "Soak"), values = c("solid", "longdash")) +
  ggtitle("Type and Method") + 
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(0.3, 1.6))

# Interaction for S*L
sl <- mushroom %>% 
  ggplot(aes(x = as.factor(Type), y = after - before, color = as.factor(Time), group = as.factor(Time),
             linetype = as.factor(Time))) +
  stat_summary(fun.y = mean, geom = "line") +
  ylab("Change in Weight") + 
  xlab("Type") +
  scale_x_discrete(breaks = c("Button", "Shiitake"), 
                   labels=c("Button","Shiitake")) +
  labs(color = "Time") +
  scale_color_manual(name = "Time", labels = c("1 min", "5 mins"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Time", labels = c("1 min", "5 mins"), values = c("solid", "longdash")) +
  ggtitle("Type and Time") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(0.3, 1.6))

# Interaction for S*T
st <- mushroom %>% 
  ggplot(aes(x = as.factor(Type), y = after - before, color = as.factor(Temperature), group = as.factor(Temperature),
             linetype = as.factor(Temperature))) +
  stat_summary(fun.y = mean, geom = "line") +
  ylab("Change in Weight") + 
  xlab("Type") +
  scale_x_discrete(breaks = c("Button", "Shiitake"), 
                   labels=c("Button","Shiitake")) +
  labs(color = "Temperature") +
  scale_color_manual(name = "Temperature", labels = c("10˚C", "40˚C"), values = c("blue", "red"))+  
  scale_linetype_manual(name = "Temperature", labels = c("10˚C", "40˚C"), values = c("solid", "longdash")) +
  ggtitle("Type and Temperature") +   
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(0.3, 1.6))

# Interaction for M*L
ml <- mushroom %>% 
  ggplot(aes(x = as.factor(Method), y = after - before, color = as.factor(Time), group = as.factor(Time),
             linetype = as.factor(Time))) +
  stat_summary(fun.y = mean, geom = "line") +
  ylab("Change in Weight") + 
  xlab("Method") +
  scale_x_discrete(breaks = c("Rinse", "Soak"), 
                   labels=c("Rinse","Soak")) +
  labs(color = "Time") +
  scale_color_manual(name = "Time", labels = c("1 min", "5 mins"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Time", labels =  c("1 min", "5 mins"), values = c("solid", "longdash")) +
  ggtitle("Method and Time") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(0.3, 1.6))

# Interaction for M*T
mt <- mushroom %>% 
  ggplot(aes(x = as.factor(Method), y = after - before, color = as.factor(Temperature), group = as.factor(Temperature),
             linetype = as.factor(Temperature))) +
  stat_summary(fun.y = mean, geom = "line") +
  ylab("Change in Weight") + 
  xlab("Method") +
  scale_x_discrete(breaks = c("Rinse", "Soak"), 
                   labels=c("Rinse","Soak")) +
  labs(color = "Temperature") +
  scale_color_manual(name = "Temperature", labels = c("10˚C", "40˚C"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Temperature", labels =c("10˚C", "40˚C"), values = c("solid", "longdash")) +
  ggtitle("Method and Temperature") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(0.3, 1.6))

# Interaction for L*T
lt <- mushroom %>% 
  ggplot(aes(x = as.factor(Time), y = after - before, color = as.factor(Temperature), group = as.factor(Temperature),
             linetype = as.factor(Temperature))) +
  #stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  ylab("Change in Weight") + 
  xlab("Time") +
  scale_x_discrete(breaks = c("1", "5"), 
                   labels=c("1 min","5 mins")) +
  labs(color = "Temperature") +
  scale_color_manual(name = "Temperature", labels = c("10˚C", "40˚C"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Temperature", labels = c("10˚C", "40˚C"), values = c("solid", "longdash")) +
  ggtitle("Time and Temperature") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(0.3, 1.6))

# Produce the image of all 6 on the same page.
gridExtra::grid.arrange(sm, sl, st, ml, mt, lt, nrow = 2)




#################### Models #######################
# Basic with 2 way interactions
m1 <- lm(after ~ before + (Type + Method + Time + Temperature)^2, 
         data = mushroom)
summary(m1)

# Remove non significant ones
m2 <- lm(after ~ before + Type + Method + Time + Temperature +
           Time*Temperature, data = mushroom)

# Test if this model is different from previous
lrtest(m2, m1)

# Check model diagnostics just to be safe
qqnorm(m2$residuals, pch = 1, frame = FALSE)
qqline(m2$residuals, col = "steelblue", lwd = 2)