###############################################################################################
### Project  : A slack based Malmquist productivity analysis of the city gas industry in Korea
### Script   : Gas_on_GIT.R
### Contents : SBM based Malmquist Index analysis on 33 Korean Gas Providers
###############################################################################################

###############################################################################################
### Setting up Environment
###############################################################################################

# Load library
pkgs <- c("ggplot2", "DJL")
sapply(pkgs, require, character.only = T)

# Load data
load(url("http://webhard.skku.edu:8081/api.link/3d_baLkMErzHTPkM_cTv.RData"), verbose = F)


###############################################################################################
### Descriptive Statistics
###############################################################################################

# Stats averaged
df.avg <- data.frame(rowMeans(df[,-6,], dims = 2),
                     IU  = rowMeans(df[,6,])/rowMeans(df[,4,]) * 100,
                     Loc = loc)

# Figure 1. The percentage of industrial supply of 33 Korean city gas providers (900*700)
ggplot(df.avg, aes(reorder(rownames(df.avg), IU), IU, fill = Loc)) + 
  geom_bar(stat = "identity") + theme_bw() + 
  scale_x_discrete(name = NULL, expand = c(0, 0)) +
  scale_y_continuous(name   = "Supply proportion for industrial use (%)", 
                     limits = c(0, 90), breaks = seq(0, 90, 10), expand = c(0, 0)) +
  theme(legend.title      = element_blank(),
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.direction  = "vertical", legend.justification = c(1,0), legend.position = c(1,0)) +
  geom_text(aes(label = round(IU, 2), 
                x     = reorder(rownames(df.avg), IU),
                y     = IU, color = Loc), size = 4, 
            position  = position_dodge(width = 0.8), vjust = 0.35, hjust = -0.1) + coord_flip()

# Figure 2. The total supply and the number of household per pipe length (800*600)
ggplot(df.avg, aes(x = SP/PP, y = HH/PP, color = Loc, label = rownames(df.avg))) + 
  geom_point(aes(size = IU)) + scale_size(guide = "none") + 
  geom_text(vjust = 1.5, show.legend = F) + theme_bw() + 
  scale_x_continuous(name   = "The total supply per Pipe length (km3/km)", 
                     limits = c(100, 1300), breaks = seq(100, 1300, 100), expand = c(0, 0)) +
  scale_y_continuous(name   = "The number of households served per Pipe length (HH/km)", 
                     limits = c(150, 650), breaks = seq(150, 650, 100), expand = c(0, 0)) +
  theme(legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "vertical", 
        legend.justification = c(1, 1), legend.position = c(1, 1))

# Figure 3. The total supply and the number of household per employee (800*600)
ggplot(df.avg, aes(x = SP/EP, y = HH/EP, color = Loc, label = rownames(df.avg)))  + 
  geom_point(aes(size = IU)) + scale_size(guide = "none") + 
  geom_text(vjust = 1.5, show.legend = F) + theme_bw() + 
  scale_x_continuous(name   = "The total supply per Employee (km3/person)", 
                     limits = c(0, 8000), breaks = seq(0, 8000, 1000), expand = c(0, 0)) +
  scale_y_continuous(name   = "The number of household served per Employee (HH/person)", 
                     limits = c(0, 4500), breaks = seq(0, 4500, 1000), expand = c(0, 0)) +
  theme(legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "vertical", 
        legend.justification = c(1, 1), legend.position = c(1, 1))

# Stats: % changes per year 
aggregate((((df[,-6, 10] - df[,-6, 1])/df[,-6, 1])*100)/9, list(loc), mean)

# DF long
df.all <- data.frame(Year  = rep(2007:2016, each = nrow(df)),
                     apply(df, 2, c),
                     Loc   = rep(loc, 10))

# Figure 4. Changes of pipe length and the number of employee from 2007 to 2016 (800*600)
ggplot(df.all, aes(x = EP, y = PP, color = Loc)) + 
  geom_point(aes(alpha = Year), size = 4) + 
  scale_alpha(guide = "none") + theme_bw() +
  scale_x_continuous(name   = "The number of Employee (person)",
                     breaks = seq(0, 800, 100)) +
  scale_y_continuous(name   = "Pipe length (km)",
                     limits = c(0, 6000), breaks = seq(0, 6000, 1000)) +
  theme(legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "vertical", 
        legend.justification = c(1,0), legend.position = c(1,0))

# Figure 5. Changes of the total supply and the number of households from 2007 to 2016 (800*600)
ggplot(df.all, aes(x = SP/1000, y = HH/1000, color = Loc)) + 
  geom_point(aes(alpha = Year), size = 4) + 
  scale_alpha(guide = "none") + theme_bw() +
  scale_x_continuous(name   = "Total supply (1,000*km3)", 
                     breaks = seq(0, 4000, 500)) +
  scale_y_continuous(name   = "The number of Household served (thousands)", 
                     breaks = seq(0, 3000, 500)) +
  theme(legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "vertical", 
        legend.justification = c(1,0), legend.position = c(1,0))


###############################################################################################
### Productivity Analysis
###############################################################################################

# Run SBM each year
res.sbm <- matrix(NA, nrow(df), 10, dimnames = list(row.names(df), 2007:2016))
for(i in 1:10){res.sbm[, i] <- dm.sbm(df[, 1:3, i], df[, 4:5, i])$eff}

# SBM long
df.sbm.raw <- data.frame(Year = rep(2007:2016, each = nrow(df)),
                         DMU  = row.names(df),
                         Loc  = rep(loc, 10),
                         Eff  = c(res.sbm))

# SBM averaged
df.sbm.avg <- data.frame(Year = rep(2007:2016, each = 3),
                         Loc  = rep(levels(loc), 10),
                         Eff  = c(apply(res.sbm, 2, 
                                        function(x) aggregate(x, list(loc), mean)$x)))

# Figure 6. Prodictivity changes of 33 gas providers (800*600)
ggplot(data = df.sbm.raw, aes(x = Year, y = Eff, group = Loc, colour = Loc)) + 
  geom_point(alpha = 0.2, size = 1.2) + theme_bw() + 
  geom_line(data = df.sbm.avg, aes(x = Year, y = Eff, group = Loc, colour = Loc), size = 1.2) + 
  scale_x_continuous(name = "Year",       breaks = seq(2007, 2016,   1)) +
  scale_y_continuous(name = "Efficiency (SBM)", breaks = seq(0.1,     1, 0.1)) +
  theme(legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "horizontal", 
        legend.justification = c(1, 1), legend.position = c(1, 1))

# Run Malmquist
res.malm.raw <- roc.malmquist(df[,1:3,], df[,4:5,], tm = 2007:2016, dm = "sbm", orientation = "n")

# Summary of results
res.malm.avg <- data.frame(Period = rep(levels(res.malm.raw$cu$Period), 3),
                           Loc    = factor(rep(levels(loc), each = 9), levels = levels(loc)),
                           CU     = aggregate(res.malm.raw$cu$CU, list(res.malm.raw$cu$Period, rep(loc, 9)), mean)$x,
                           FS     = aggregate(res.malm.raw$fs$FS, list(res.malm.raw$fs$Period, rep(loc, 9)), mean)$x,
                           MI     = aggregate(res.malm.raw$mi$MI, list(res.malm.raw$mi$Period, rep(loc, 9)), mean)$x)

# Figure 7. CU (800*600)
ggplot(data = res.malm.raw$cu, aes(x = Period, y = CU, group = rep(loc, 9), colour = rep(loc, 9))) + 
  geom_point(alpha = 0.2, size = 1.2) + theme_bw() + 
  geom_line(data = res.malm.avg, aes(x = Period, y = CU, group = Loc, colour = Loc), size = 1.2) +
  scale_y_continuous(name = "Technical Efficiency Change (TEC)", limits = c(0.8, 1.3), breaks = seq(0.8, 1.3, 0.1)) +
  theme(legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "horizontal", 
        legend.justification = c(1, 1), legend.position = c(1, 1))

# Figure 8. FS (800*600)
ggplot(data = res.malm.raw$fs, aes(x = Period, y = FS, group = rep(loc, 9), colour = rep(loc, 9))) + 
  geom_point(alpha = 0.2, size = 1.2) + theme_bw() + 
  geom_line(data = res.malm.avg, aes(x = Period, y = FS, group = Loc, colour = Loc), size = 1.2) + 
  scale_y_continuous(name = "Frontier Shift (FS)", limits = c(0.8, 1.3), breaks = seq(0.8, 1.3, 0.1)) +
  theme(legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "horizontal", 
        legend.justification = c(1, 1), legend.position = c(1, 1))

# Figure 9. MI (800*600)
ggplot(data = res.malm.raw$mi, aes(x = Period, y = MI, group = rep(loc, 9), colour = rep(loc, 9))) + 
  geom_point(alpha = 0.2, size = 1.2) + theme_bw() + 
  geom_line(data = res.malm.avg, aes(x = Period, y = MI, group = Loc, colour = Loc), size = 1.2) + 
  scale_y_continuous(name = "Malmquist Index (MI)", limits = c(0.8, 1.3), breaks = seq(0.8, 1.3, 0.1)) +
  theme(legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "horizontal", 
        legend.justification = c(1, 1), legend.position = c(1, 1))

# Footnote 6. Regression MI ~ IU
IU.annual.c <- c(df[,6, 2] - df[,6,1], df[,6, 3] - df[,6,2], df[,6, 4] - df[,6,3], df[,6, 5] - df[,6,4],
                 df[,6, 6] - df[,6,5], df[,6, 7] - df[,6,6], df[,6, 8] - df[,6,7], df[,6, 9] - df[,6,8],
                 df[,6,10] - df[,6,9])

summary(lm(res.malm.raw$mi$MI ~ IU.annual.c), na.rm = T)

# Price long
m.price <- data.frame(Tick  = rep(1:49, 3),
                      Price = c(price$Dubai_oil*10, price$LNG, price$LPG),
                      Type  = factor(rep(c("Oil", "LNG", "LPG"), each = 49), levels = c("Oil", "LNG", "LPG")))

# Figure 10. LPG vs LNG (vs Oil) prices (850*600)
ggplot(data = m.price, aes(x = Tick, y = Price, group = Type, colour = Type)) + 
  geom_line(size = 1.2) + theme_bw() + 
  scale_colour_manual(values = c("lightgrey", "royalblue", "orangered")) +
  scale_x_continuous(name = "Year", breaks = c(1, 13, 25, 37, 49), labels = seq(2013, 2017, 1)) +
  scale_y_continuous(name = "Gas price (₩/9,393kcal)", limits = c(200, 1200), breaks = seq(200, 1200, 200),
                     sec.axis = sec_axis(~.*0.1, name = "Oil price - Dubai crude ($/B)", breaks = seq(20, 120, 20))) +
  theme(legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "horizontal", 
        legend.justification = c(1, 1), legend.position = c(1, 1))
