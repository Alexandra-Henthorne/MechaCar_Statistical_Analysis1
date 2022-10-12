lm(AWD ~ mpg,MechaCar_mpg) #create linear model
summary(lm(AWD~mpg,MechaCar_mpg)) #summarize linear model
model <- lm(AWD ~ mpg,MechaCar_mpg) #create linear model
yvals <- model$coefficients['mpg']*MechaCar_mpg$mpg +
  model$coefficients['(Intercept)'] #determine y-axis values from linear model
plt <- ggplot(MechaCar_mpg,aes(x=mpg,y=AWD)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") #plot scatter and linear model
lm(vehicle_length~ vehicle_weight + spoiler_angle + ground_clearance + AWD + mpg,data= MechaCar_mpg) #generate multiple linear regression model
summary(lm(vehicle_length~ vehicle_weight + spoiler_angle + ground_clearance + AWD + mpg,data= MechaCar_mpg)) #generate summary statistics

total_summary <- Suspension_Coil %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Var_PSI=var(PSI),Std_Dev_PSI=sd(PSI),Num_Coil=n(), .groups = 'keep') 
lot_summary <- Suspension_Coil  %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Var_PSI=var(PSI),Std_Dev_PSI=sd(PSI),Num_Coil=n(), .groups = 'keep')                                                                                  

t.test(Suspension_Coil$PSI,mu=1500)
lot1 <- subset(Suspension_Coil, Manufacturing_Lot=="Lot1")
lot2 <- subset(Suspension_Coil, Manufacturing_Lot=="Lot2")
lot3 <- subset(Suspension_Coil, Manufacturing_Lot=="Lot3")

t.test(lot1$PSI,mu=1500)
t.test(lot2$PSI,mu=1500)
t.test(lot3$PSI,mu=1500)
       