#Adeel Arshid
#Econometrics
#11/3/222
# Group names : John Robison, Kseniia Huseinova and Mario Wisa.



load("/Users/adeelarshid/Desktop/Econometrics/Household_Pulse_data_w48.RData")
attach(Household_Pulse_data)

######################################################################
model_logit2 <- glm(TWDAYS ~ EEDUC,
                    family = binomial, data = Household_Pulse_data)


###############################################################################


Household_Pulse_data$TWDAYS <- (Household_Pulse_data$TWDAYS == "had no telework days in past week")
is.na(Household_Pulse_data$TWDAYS) <- which(Household_Pulse_data$TWD == "NA") 


table(Household_Pulse_data$TWDAYS,Household_Pulse_data$EEDUC)



#############################################################################
# creating subset

pick_use1 <- (Household_Pulse_data$TWDAYS < 2000) 
dat_use1 <- subset(Household_Pulse_data, pick_use1)



##############################################################################

# and to be finicky, might want to use this for factors after subsetting in case some get lost
dat_use1$REGION <- droplevels(dat_use1$REGION) 


###############################################################################

model_logit1 <- glm(TWDAYS ~ TBIRTH_YEAR + EEDUC + MS + RRACE + SEXUAL_ORIENTATION + RHISPANIC + GENID_DESCRIBE,
                    family = binomial, data = dat_use1)
summary(model_logit1)





########################################################################

new_data_to_be_predicted <- data.frame(TBIRTH_YEAR = 1990,
                                       EEDUC = factor("bach deg", levels = levels(dat_use1$EEDUC)),
                                       MS = factor("never",levels = levels(dat_use1$MS)),
                                       RRACE = factor("Black",levels = levels(dat_use1$RRACE)),
                                       RHISPANIC = factor("Hispanic",levels = levels(dat_use1$RHISPANIC)),
                                       GENID_DESCRIBE = factor("male", levels = levels(dat_use1$GENID_DESCRIBE))
)
predict(model_logit1,new_data_to_be_predicted)



#changing the year 

new_data_to_be_predicted <- data.frame(TBIRTH_YEAR = 2005,
                                       EEDUC = factor("avd deg", levels = levels(dat_use1$EEDUC)),
                                       MS = factor("divorced",levels = levels(dat_use1$MS)),
                                       RRACE = factor("white",levels = levels(dat_use1$RRACE)),
                                       RHISPANIC = factor("Not Hispanic",levels = levels(dat_use1$RHISPANIC)),
                                       GENID_DESCRIBE = factor("female", levels = levels(dat_use1$GENID_DESCRIBE))
)
predict(model_logit1,new_data_to_be_predicted)


