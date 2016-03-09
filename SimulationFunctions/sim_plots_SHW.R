#  Mountain lion population simulation model and plotting
#  Mukacs Lab
#  03/07/2016
################################################################################

#  Load packages
library(dplyr)
library(ggplot2)
library(reshape)
library(wesanderson)
library(grid)
################################################################################

#  Source simulation function code
source(file.path("C:/Users", 
                                   Sys.info()["login"],
                                   "Documents/GitHub/SDP/SimulationFunctions/mtlion_sim_fun.R"))

#  Determine number of iterations and years
tot_sims <- 100
nyr <- 50

#  Generate matrix for each age class/sex to save iterations
f_N_mat1 <- matrix(NA, nrow = nyr, ncol = tot_sims)
f_N_mat2 <- matrix(NA, nrow = nyr, ncol = tot_sims)
f_N_mat3 <- matrix(NA, nrow = nyr, ncol = tot_sims)
f_N_mat4 <- matrix(NA, nrow = nyr, ncol = tot_sims)

m_N_mat1 <- matrix(NA, nrow = nyr, ncol = tot_sims)
m_N_mat2 <- matrix(NA, nrow = nyr, ncol = tot_sims)
m_N_mat3 <- matrix(NA, nrow = nyr, ncol = tot_sims)
m_N_mat4 <- matrix(NA, nrow = nyr, ncol = tot_sims)

#  Run for tot_sims iterations and save within appropriate age class
for(j in 1:tot_sims){

     sims <- sim_pop(nyr = nyr, 
                                        n1 = matrix(c(30, 30, 
                                                                    20, 20,    
                                                                    60, 40,
                                                                    120, 80), 
                                                                    ncol = 2, byrow = T),
                                        fphi = matrix( c(0.6, 0.7, 
                                                                         0.4, 0.6, 
                                                                         0.7, 0.8, 
                                                                         0.8, 0.9), 
                                                                         ncol = 2, byrow = T),
                                        mphi = matrix( c(0.6, 0.7, 
                                                                           0.4, 0.6, 
                                                                           0.5, 0.8, 
                                                                           0.6, 0.9), 
                                                                           ncol = 2, byrow = T),
                                        fec = c(1, 3))
                                          
  f_N_mat1[,j] <- sims$N[,1,1]
  f_N_mat2[,j] <- sims$N[,2,1]
  f_N_mat3[,j] <- sims$N[,3,1]
  f_N_mat4[,j] <- sims$N[,4,1]
  
  m_N_mat1[,j] <- sims$N[,1,2]
  m_N_mat2[,j] <- sims$N[,2,2]
  m_N_mat3[,j] <- sims$N[,3,2]
  m_N_mat4[,j] <- sims$N[,4,2]
  }

#  Generata data frame for each age class/sex and total N/sex for plotting
#   all iterations for each age class/sex combo in a single plot.
Year <- seq(1, 50, 1)
 
#  Female age class 1 
f_N_1 <- as.data.frame(f_N_mat1)
pop_age1_f <- cbind(melt(f_N_1), Year)
pop_age1_f <- dplyr::rename(pop_age1_f, simulation = variable)
#  Female age class 2
f_N_2 <- cbind(as.data.frame(f_N_mat2))
pop_age2_f <- cbind(melt(f_N_2), Year)
pop_age2_f <- dplyr::rename(pop_age2_f, simulation = variable)
#  Female age class 3
f_N_3 <- cbind(as.data.frame(f_N_mat3))
pop_age3_f <- cbind(melt(f_N_3), Year)
pop_age3_f <- dplyr::rename(pop_age3_f, simulation = variable)
#  Female age class 4
f_N_4 <- cbind(as.data.frame(f_N_mat4))
pop_age4_f <- cbind(melt(f_N_4), Year)
pop_age4_f <- dplyr::rename(pop_age4_f, simulation = variable)
#  Full N
pop_age1_f <- mutate(pop_age1_f, age = 1)
pop_age2_f <- mutate(pop_age2_f, age = 2)
pop_age3_f <- mutate(pop_age3_f, age = 3)
pop_age4_f <- mutate(pop_age4_f, age = 4)
pop_tmp <- cbind(pop_age1_f , pop_age2_f , pop_age3_f , pop_age4_f)
names(pop_tmp)[1] <- "sim_num"
names(pop_tmp)[2] <- "age_1"
names(pop_tmp)[3] <- "pop_year"
names(pop_tmp)[4] <- "age_class1"
names(pop_tmp)[8] <- "age_class2"
names(pop_tmp)[12] <- "age_class3"
names(pop_tmp)[16] <- "age_class4"
names(pop_tmp)[5] <- "sim2"
names(pop_tmp)[6] <- "age_2"
names(pop_tmp)[7] <- "year2"
names(pop_tmp)[11] <- "year3"
names(pop_tmp)[15] <- "year4"
names(pop_tmp)[9] <- "sim3"
names(pop_tmp)[13] <- "sim4"
names(pop_tmp)[10] <- "age_3"
names(pop_tmp)[14] <- "age_4"
pop_all_f <- pop_tmp %>%
                         select(sim_num, pop_year, age_1, age_2, age_3, age_4) %>%
                         mutate(total_pop = age_1+age_2+age_3+age_4) %>%
                         as.data.frame()

#  Male age class 1
m_N_1 <- cbind(as.data.frame(m_N_mat1))
pop_age1_m <- cbind(melt(m_N_1), Year)
pop_age1_m <- dplyr::rename(pop_age1_m, simulation = variable) 
#  Male age class 2
m_N_2 <- cbind(as.data.frame(m_N_mat2))
pop_age2_m <- cbind(melt(m_N_2), Year)
pop_age2_m <- dplyr::rename(pop_age2_m, simulation = variable) 
#  Male age class 3
m_N_3 <- cbind(as.data.frame(m_N_mat3))
pop_age3_m <- cbind(melt(m_N_3), Year)
pop_age3_m <- dplyr::rename(pop_age3_m, simulation = variable) 
#  Male age class 4
m_N_4 <- cbind(as.data.frame(m_N_mat4))
pop_age4_m <- cbind(melt(m_N_4), Year)
pop_age4_m <- dplyr::rename(pop_age4_m, simulation = variable) 
#  Full N
pop_age1_m <- mutate(pop_age1_m, age = 1)
pop_age2_m <- mutate(pop_age2_m, age = 2)
pop_age3_m <- mutate(pop_age3_m, age = 3)
pop_age4_m <- mutate(pop_age4_m, age = 4)
pop_tmp <- cbind(pop_age1_m , pop_age2_m , pop_age3_m , pop_age4_m)
names(pop_tmp)[1] <- "sim_num"
names(pop_tmp)[2] <- "age_1"
names(pop_tmp)[3] <- "pop_year"
names(pop_tmp)[4] <- "age_class1"
names(pop_tmp)[8] <- "age_class2"
names(pop_tmp)[12] <- "age_class3"
names(pop_tmp)[16] <- "age_class4"
names(pop_tmp)[5] <- "sim2"
names(pop_tmp)[6] <- "age_2"
names(pop_tmp)[7] <- "year2"
names(pop_tmp)[11] <- "year3"
names(pop_tmp)[15] <- "year4"
names(pop_tmp)[9] <- "sim3"
names(pop_tmp)[13] <- "sim4"
names(pop_tmp)[10] <- "age_3"
names(pop_tmp)[14] <- "age_4"
pop_all_m <- pop_tmp %>%
                           select(sim_num, pop_year, age_1, age_2, age_3, age_4) %>%
                           mutate(total_pop = age_1+age_2+age_3+age_4) %>%
                           as.data.frame()


#  Plots using stat_summary for means
plot_1_f <- ggplot(pop_age1_f, aes(x = Year, y = value, group = simulation)) + 
                       geom_point(colour = "#3B9AB2", alpha = 0.4) +
                       stat_summary(aes(group = 1), geom = "line", fun.y = mean, colour = "#3B9AB2", size = 3, alpha = 0.4) +
                       xlab("Year") +
                       ylab("Population Size\n") +
                       ylim(0,100) +
                       scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50)) +
                       theme_bw() +
                       ggtitle("Female  - age class 1") +
                       theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())

plot_2_f <- ggplot(pop_age2_f, aes(x = Year, y = value, group = simulation)) + 
                       geom_point(colour = "#E1AF00", alpha = 0.4) +
                      stat_summary(aes(group = 1), geom = "line", fun.y = mean, colour = "#E1AF00", size = 3, alpha = 0.4) +
                       xlab("Year") +
                       ylab("Population Size\n") +
                       ylim(0,100) +
                       scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50)) +
                       theme_bw() +
                       ggtitle("Female  - age class 2") +
                       theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())

plot_3_f <- ggplot(pop_age3_f, aes(x = Year, y = value, group = simulation)) + 
                       geom_point(colour = "#446455", alpha = 0.4) +
                       stat_summary(aes(group = 1), geom = "line", fun.y = mean, colour = "#446455", size = 3, alpha = 0.4) +
                       xlab("Year") +
                       ylab("Population Size\n") +
                       ylim(0,100) +
                       scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50)) +
                       theme_bw() +
                       ggtitle("Female  - age class 3") +
                       theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())

plot_4_f <- ggplot(pop_age4_f, aes(x = Year, y = value, group = simulation)) + 
                       geom_point(colour = "#972D15", alpha = 0.4) +
                       stat_summary(aes(group = 1), geom = "line", fun.y = mean, colour = "#972D15", size = 3, alpha = 0.4) +
                       xlab("Year") +
                       ylab("Population Size\n") +
                       ylim(0,200) +
                       scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50)) +
                       theme_bw() +
                       ggtitle("Female  - age class 4") +
                       theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())

plot_all_f <- ggplot(pop_all_f, aes(x = pop_year, y = total_pop, group = sim_num)) + 
                       geom_point(colour = "#3B9AB2", alpha = 0.4) +
                       stat_summary(aes(group = 1), geom = "line", fun.y = mean, colour = "#3B9AB2", size = 3, alpha = 0.4) +
                       xlab("Year") +
                       ylab("Population Size\n") +
                       ylim(0,350) +
                       scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50)) +
                       theme_bw() +
                       ggtitle("Female  - all age classes") +
                       theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())

#  Plots for males
plot_1_m <- ggplot(pop_age1_m, aes(x = Year, y = value, group = simulation)) + 
                       geom_point(colour = "#3B9AB2", alpha = 0.4) +
                      stat_summary(aes(group = 1), geom = "line", fun.y = mean, colour = "#3B9AB2", size = 3, alpha = 0.4) +
                       xlab("Year") +
                       ylab("Population Size\n") +
                       ylim(0,100) +
                       scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50)) +
                       theme_bw() +
                       ggtitle("Male - age class 1") +
                       theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())

plot_2_m <- ggplot(pop_age2_m, aes(x = Year, y = value, group = simulation)) + 
                       geom_point(colour = "#E1AF00", alpha = 0.4) +
                        stat_summary(aes(group = 1), geom = "line", fun.y = mean, colour = "#E1AF00", size = 3, alpha = 0.4) +
                       xlab("Year") +
                       ylab("Population Size\n") +
                       ylim(0,100) +
                       scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50)) +
                       theme_bw() +
                       ggtitle("Male - age class 2") +
                       theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())

plot_3_m <- ggplot(pop_age3_m, aes(x = Year, y = value, group = simulation)) + 
                       geom_point(colour = "#446455", alpha = 0.4) +
                       stat_summary(aes(group = 1), geom = "line", fun.y = mean, colour = "#446455", size = 3, alpha = 0.4) +
                       xlab("Year") +
                       ylab("Population Size\n") +
                       ylim(0,100) +
                       scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50)) +
                       theme_bw() +
                       ggtitle("Male - age class 3") +
                       theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())

plot_4_m <- ggplot(pop_age4_m, aes(x = Year, y = value, group = simulation)) + 
                       geom_point(colour = "#972D15", alpha = 0.4) +
                      stat_summary(aes(group = 1), geom = "line", fun.y = mean, colour = "#972D15", size = 3, alpha = 0.4) +
                       xlab("Year") +
                       ylab("Population Size\n") +
                       ylim(0,200) +
                       scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50)) +
                       theme_bw() +
                       ggtitle("Male - age class 4") +
                       theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())

plot_all_m <- ggplot(pop_all_m, aes(x = pop_year, y = total_pop, group = sim_num)) + 
                       geom_point(colour = "#972D15", alpha = 0.4) +
                       stat_summary(aes(group = 1), geom = "line", fun.y = mean, colour = "#972D15", size = 3, alpha = 0.4) +
                       xlab("Year") +
                       ylab("Population Size\n") +
                       ylim(0,350) +
                       scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50)) +
                       theme_bw() +
                       ggtitle("Male  - all age classes") +
                       theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())


#  Function to put mulitplots on same page from http://www.cookbook-r.com/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




#  Final plots
all_plots_f <- multiplot(plot_1_f, plot_2_f, plot_3_f, plot_4_f, cols=2)
all_plots_m <- multiplot(plot_1_m, plot_2_m, plot_3_m, plot_4_m, cols=2)
all_plots_both <- multiplot(plot_1_f, plot_2_f, plot_3_f, plot_4_f, plot_1_m, plot_2_m, plot_3_m, plot_4_m, cols=2)
all_plots_both_tot <- multiplot(plot_all_f, plot_all_m, cols=2)





#  Alternative plotting methods using geom_line(method = "loess") to make smooth line and leaving points invisible.
#  Plots for females
plot_1_f <- ggplot(pop_age1_f, aes(x = Year, y = value, group = simulation)) + 
                       #geom_point(colour = "#3B9AB2", alpha = 0.4) +
                       geom_line(stat="smooth", method = "loess", colour = "#78B7C5", se = F, size = 0.1, alpha = 0.4) +
                       xlab("Year") +
                       ylab("Population Size\n") +
                       ylim(0,100) +
                       scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50)) +
                       theme_bw() +
                       ggtitle("Female  - age class 1") +
                       theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())

plot_2_f <- ggplot(pop_age2_f, aes(x = Year, y = value, group = simulation)) + 
                       #geom_point(colour = "#E1AF00", alpha = 0.4) +
                       geom_line(stat="smooth", method = "loess", colour = "#EBCC2A", se = F, size = 0.1, alpha = 0.4) +
                       xlab("Year") +
                       ylab("Population Size\n") +
                       ylim(0,100) +
                       scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50)) +
                       theme_bw() +
                       ggtitle("Female  - age class 2") +
                       theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())

plot_3_f <- ggplot(pop_age3_f, aes(x = Year, y = value, group = simulation)) + 
                       #geom_point(colour = "#446455", alpha = 0.4) +
                       geom_line(stat="smooth", method = "loess", colour = "#81A88D", se = F, size = 0.1, alpha = 0.4) +
                       xlab("Year") +
                       ylab("Population Size\n") +
                       ylim(0,100) +
                       scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50)) +
                       theme_bw() +
                       ggtitle("Female  - age class 3") +
                       theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())

plot_4_f <- ggplot(pop_age4_f, aes(x = Year, y = value, group = simulation)) + 
                       #geom_point(colour = "#972D15", alpha = 0.4) +
                       geom_line(stat="smooth", method = "loess", colour = "#E66447", se = F, size = 0.1, alpha = 0.4) +
                       xlab("Year") +
                       ylab("Population Size\n") +
                       ylim(0,200) +
                       scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50)) +
                       theme_bw() +
                       ggtitle("Female  - age class 4") +
                       theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())

plot_all_f <- ggplot(pop_all_f, aes(x = pop_year, y = total_pop, group = sim_num)) + 
                       #geom_point(colour = "#972D15", alpha = 0.4) +
                       geom_line(stat="smooth", method = "loess", colour = "#E66447", se = F, size = 0.1, alpha = 0.4) +
                       xlab("Year") +
                       ylab("Population Size\n") +
                       ylim(0,350) +
                       scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50)) +
                       theme_bw() +
                       ggtitle("Female  - all age classes") +
                       theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())


#  Plots for males
plot_1_m <- ggplot(pop_age1_m, aes(x = Year, y = value, group = simulation)) + 
                       #geom_point(colour = "#3B9AB2", alpha = 0.4) +
                       geom_line(stat="smooth", method = "loess", colour = "#78B7C5", se = F, size = 0.1, alpha = 0.4) +
                       xlab("Year") +
                       ylab("Population Size\n") +
                       ylim(0,100) +
                       scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50)) +
                       theme_bw() +
                       ggtitle("Male - age class 1") +
                       theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())

plot_2_m <- ggplot(pop_age2_m, aes(x = Year, y = value, group = simulation)) + 
                       #geom_point(colour = "#E1AF00", alpha = 0.4) +
                       geom_line(stat="smooth", method = "loess", colour = "#EBCC2A", se = F, size = 0.1, alpha = 0.4) +
                       xlab("Year") +
                       ylab("Population Size\n") +
                       ylim(0,100) +
                       scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50)) +
                       theme_bw() +
                       ggtitle("Male - age class 2") +
                       theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())

plot_3_m <- ggplot(pop_age3_m, aes(x = Year, y = value, group = simulation)) + 
                       #geom_point(colour = "#446455", alpha = 0.4) +
                       geom_line(stat="smooth", method = "loess", colour = "#81A88D", se = F, size = 0.1, alpha = 0.4) +
                       xlab("Year") +
                       ylab("Population Size\n") +
                       ylim(0,100) +
                       scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50)) +
                       theme_bw() +
                       ggtitle("Male - age class 3") +
                       theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())

plot_4_m <- ggplot(pop_age4_m, aes(x = Year, y = value, group = simulation)) + 
                      #geom_point(colour = "#972D15", alpha = 0.4) +
                       geom_line(stat="smooth", method = "loess", colour = "#E66447", se = F, size = 0.1, alpha = 0.4) +
                       xlab("Year") +
                       ylab("Population Size\n") +
                       ylim(0,200) +
                       scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50)) +
                       theme_bw() +
                       ggtitle("Male - age class 4") +
                       theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())

plot_all_m <- ggplot(pop_all_m, aes(x = pop_year, y = total_pop, group = sim_num)) + 
                       #geom_point(colour = "#972D15", alpha = 0.4) +
                       geom_line(stat="smooth", method = "loess", colour = "#E66447", se = F, size = 0.1, alpha = 0.4) +
                       xlab("Year") +
                       ylab("Population Size\n") +
                       ylim(0,350) +
                       scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50)) +
                       theme_bw() +
                       ggtitle("Male  - all age classes") +
                       theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())
                       
# #  Older versions of data arrangement
# f_N_1 <- cbind(as.data.frame(f_N_mat1), sex_f)
# m_N_1 <- cbind(as.data.frame(m_N_mat1), sex_m)
# f_N_1 <- dplyr::rename(f_N_1,  sex = sex_f)
# m_N_1 <- dplyr::rename(m_N_1,  sex = sex_m)
# N_1 <- rbind(f_N_1, m_N_1)
# pop_age1 <- cbind(melt(N_1), Year)
# pop_age1_f <- cbind(melt(f_N_1), Year)

# f_N_2 <- cbind(as.data.frame(f_N_mat2), sex_f)
# m_N_2 <- cbind(as.data.frame(m_N_mat2), sex_m)
# f_N_2 <- dplyr::rename(f_N_2,  sex = sex_f)
# m_N_2 <- dplyr::rename(m_N_2,  sex = sex_m)
# N_2 <- rbind(f_N_2, m_N_2)
# pop_age2 <- cbind(melt(N_2), Year)

# f_N_3 <- cbind(as.data.frame(f_N_mat3), sex_f)
# m_N_3 <- cbind(as.data.frame(m_N_mat3), sex_m)
# f_N_3 <- dplyr::rename(f_N_3,  sex = sex_f)
# m_N_3 <- dplyr::rename(m_N_3,  sex = sex_m)
# N_3 <- rbind(f_N_3, m_N_3)
# pop_age3 <- cbind(melt(N_4), Year)

# f_N_4 <- cbind(as.data.frame(f_N_mat4), sex_f)
# m_N_4 <- cbind(as.data.frame(m_N_mat4), sex_m)
# f_N_4 <- dplyr::rename(f_N_4,  sex = sex_f)
# m_N_4 <- dplyr::rename(m_N_4,  sex = sex_m)
# N_4 <- rbind(f_N_4, m_N_4)
# pop_age4 <- cbind(melt(N_4), Year)