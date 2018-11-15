#Import required libraries
#make sure to have installed
library("reshape2")
library("ggplot2")

# Fixed exchange rate

#define variables
a_0 <- 80 # autonomous consumption
a_1 <- 0.8 # marginal propensity to consume
i_0 <- 25 # autonomous private investment
i_1 <- -5 # marginal propensity to invest
g_0 <- 40 # government expenditure
x_0 <- 25 # autonomous level of exports
m_0 <- 20 # autonomous level of imports
m_1 <- 0.15 # marginal propensity to import
t_rate <- 0.15 # Tax rate
c_1 <- (a_0+i_0+g_0+x_0-m_0) # autonomous values
c_2 <- (1-a_1*(1-t_rate)+m_1) # marginal values
l_1 <- 0.5 # liquidity function for motive 1
l_2 <- -5 # liquidity function for motive 2
k_prm <- 6 # increase in financial account (function of the interest rate)

# Calculate interest rate, equilibrium y, and money supply
int_rate_calc <- (((x_0-m_0)/m_1)-(c_1/c_2))/((i_1/c_2)-(k_prm/m_1))
y_calc <- ((x_0-m_0)/m_1)+(k_prm*int_rate_calc/m_1)
m_no_bar <- l_1*y_calc+l_2*int_rate_calc

print("Interest rate:")
int_rate_calc
print("Output:")
y_calc
print("Money Supply:")
m_no_bar

#Create empty dataframe with 1000 rows
obs <- c(seq(1,1000,by=1))
IS_LM_BP <- data.frame(obs)
head(IS_LM_BP)

#Generate Data
IS_LM_BP["int_rate_s"] <- IS_LM_BP["obs"]/100
IS_LM_BP["Y_IS"] <- (c_1/c_2)+(i_1*IS_LM_BP["int_rate_s"]/c_2)
IS_LM_BP["Y_LM"] <- (m_no_bar/l_1)-(l_2*IS_LM_BP["int_rate_s"]/l_1)
IS_LM_BP["Y_BP"] <- ((x_0-m_0)/m_1)+(k_prm*IS_LM_BP["int_rate_s"]/m_1)

#Drop obs cols
IS_LM_BP <- IS_LM_BP[-c(1)]
head(IS_LM_BP)

#Generate Equilibrium levels
print("Equilibrium level of output from the IS curve:")
y_is = (c_1/c_2)+(i_1*int_rate_calc/c_2)
y_is

print("Equilibrium level of output from the LM curve")
y_lm = (m_no_bar/l_1)-(l_2*int_rate_calc/l_1)
y_lm

print("Equilibrium level of output from the BP curve")
y_bp = ((x_0-m_0)/m_1)+(k_prm*int_rate_calc/m_1)
y_bp

#reshape df 
df <- melt(IS_LM_BP, id="int_rate_s")
head(df,10)

#plot IS LM BP
ggplot(data=df, aes(x=value,y=int_rate_s, color=variable))+
  geom_line() +
  geom_point(x=y_is,y=int_rate_calc) +
  labs(title="IS LM BP under a Floating Exchange Rate Regime",x="Output (Y)",y="Interest Rate (i)",colour="Curve")
  
str(df)
summary(df)
summary(df["int_rate_s"])
names(df)
