#########################################################################################################################
### Project  : A slack based Malmquist productivity analysis of the city gas industry in Korea
### Script   : Gas_on_GIT.R
### Contents : SBM based Malmquist Index analysis on 33 Korean Gas Providers
#########################################################################################################################

#########################################################################################################################
### Setting up Environment
#########################################################################################################################

# Load library
pkgs <- c("ggplot2", "DJL")
sapply(pkgs, require, character.only = T)

# Load data & parameters
load("Gas.RData")
df.3d <- simplify2array(by(df.2d[,-c(1:3)], df.2d$Year, as.matrix))
id.x  <- 1:3 #  in: pipe & employee & cost
id.y  <- 4:5 # out: supply & household


#########################################################################################################################
### Descriptive Statistics
#########################################################################################################################

# Stats averaged
df.avg <- data.frame(rowMeans(df.3d[,-6,], dims = 2),
                     IU  = rowMeans(df.3d[,6,])/rowMeans(df.3d[,4,]) * 100,
                     Loc = loc)

# Figure 2. The percentage of industrial supply of 33 Korean city gas providers (900*700)
ggplot(df.avg, aes(reorder(name, IU), IU, fill = Loc)) + 
  geom_bar(stat = "identity") + theme_bw() + 
  scale_x_discrete(name = NULL, expand = c(0, 0)) +
  scale_y_continuous(name   = "Supply proportion for industrial use (%)", 
                     limits = c(0, 90), breaks = seq(0, 90, 10), expand = c(0, 0)) +
  theme(legend.title      = element_blank(),
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.direction  = "vertical", legend.justification = c(1, 0), legend.position = c(1, 0)) +
  geom_text(aes(label = round(IU, 2), 
                x     = reorder(name, IU),
                y     = IU, color = Loc), size = 4, 
            position  = position_dodge(width = 0.8), vjust = 0.35, hjust = -0.1) + coord_flip()

# Figure 3. The total supply and the number of household per pipe length (800*600)
ggplot(df.avg, aes(x = SP/PP, y = HH/PP, color = Loc, label = name)) + 
  geom_point(aes(size = IU)) + scale_size(guide = "none") + 
  geom_text(vjust = 1.5, show.legend = F) + theme_bw() + 
  scale_x_continuous(name   = "The total supply per Pipe length (km3/km)", 
                     limits = c(100, 1300), breaks = seq(100, 1300, 200), expand = c(0, 0)) +
  scale_y_continuous(name   = "The number of households served per Pipe length (HH/km)", 
                     limits = c(150, 650), breaks = seq(150, 650, 100), expand = c(0, 0)) +
  theme(legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "vertical", 
        legend.justification = c(1, 1), legend.position = c(1, 1),
        plot.margin = margin(10, 20, 10, 10))

# Figure 4. The total supply and the number of household per employee (800*600)
ggplot(df.avg, aes(x = SP/EP, y = HH/EP, color = Loc, label = name))  + 
  geom_point(aes(size = IU)) + scale_size(guide = "none") + 
  geom_text(vjust = 1.5, show.legend = F) + theme_bw() + 
  scale_x_continuous(name   = "The total supply per Employee (km3/person)", 
                     limits = c(0, 8000), breaks = seq(0, 8000, 1000), expand = c(0, 0)) +
  scale_y_continuous(name   = "The number of household served per Employee (HH/person)", 
                     limits = c(0, 4500), breaks = seq(0, 4500, 1000), expand = c(0, 0)) +
  theme(legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "vertical", 
        legend.justification = c(1, 1), legend.position = c(1, 1),
        plot.margin = margin(10, 20, 10, 10))

# Stats: % changes per year 
aggregate((((df.3d[,-6, 11] - df.3d[,-6, 1])/df.3d[,-6, 1])*100)/10, list(loc), mean)


#########################################################################################################################
### Productivity Analysis
#########################################################################################################################

# Run SBM each year
res.sbm <- matrix(NA, nrow(df.3d), length(f.t), dimnames = list(name, f.t))
for(i in 1:length(f.t)){res.sbm[, i] <- dm.sbm(df.3d[, id.x, i], df.3d[, id.y, i])$eff}

# SBM long
df.sbm.raw <- data.frame(Year = df.2d$Year,
                         DMU  = name,
                         Loc  = df.2d$Location,
                         Eff  = c(res.sbm))

# SBM averaged
df.sbm.avg <- data.frame(Year = rep(f.t, each = length(unique(loc))),
                         Loc  = rep(levels(loc), length(f.t)),
                         Eff  = c(apply(res.sbm, 2, 
                                        function(x) aggregate(x, list(loc), mean)$x)))

# Figure 5. Prodictivity changes of 33 gas providers (800*600)
ggplot(data = df.sbm.raw, aes(x = Year, y = Eff, group = Loc, colour = Loc)) + 
  geom_point(alpha = 0.2, size = 1.2) + theme_bw() + 
  geom_line(data = df.sbm.avg, aes(x = Year, y = Eff, group = Loc, colour = Loc), size = 1.2) + 
  scale_x_continuous(name = "Year",             breaks = seq(2007, 2017,   1)) +
  scale_y_continuous(name = "Efficiency (SBM)", breaks = seq( 0.1,    1, 0.1)) +
  theme(legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "horizontal", 
        legend.justification = c(1, 1), legend.position = c(1, 1))

# Run Malmquist
res.malm.raw <- roc.malmquist(df.3d[,id.x,], df.3d[,id.y,], tm = f.t, dm = "sbm", orientation = "n")

# Summary of results
res.malm.avg <- data.frame(Period = rep(levels(res.malm.raw$cu$Period), length(unique(loc))),
                           Loc    = factor(rep(levels(loc), each = length(f.t) - 1), levels = levels(loc)),
                           CU     = aggregate(res.malm.raw$cu$CU, list(res.malm.raw$cu$Period, rep(loc, length(f.t) - 1)), mean)$x,
                           FS     = aggregate(res.malm.raw$fs$FS, list(res.malm.raw$fs$Period, rep(loc, length(f.t) - 1)), mean)$x,
                           MI     = aggregate(res.malm.raw$mi$MI, list(res.malm.raw$mi$Period, rep(loc, length(f.t) - 1)), mean)$x)

# Figure 6. CU (800*600)
ggplot(data = res.malm.raw$cu, 
       aes(x = Period, y = CU, group = rep(loc, length(f.t) - 1), colour = rep(loc, length(f.t) - 1))) + 
  geom_hline(yintercept = 1.0, color = "gray", size = 1) +
  geom_point(alpha = 0.2, size = 1.2) + theme_bw() + 
  geom_line(data = res.malm.avg, aes(x = Period, y = CU, group = Loc, colour = Loc), size = 1.2) +
  scale_y_continuous(name = "Technical Efficiency Change (TEC)", limits = c(0.8, 1.3), breaks = seq(0.8, 1.3, 0.1)) +
  theme(legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "horizontal", 
        legend.justification = c(1, 1), legend.position = c(1, 1))

# Figure 7. FS (800*600)
ggplot(data = res.malm.raw$fs, 
       aes(x = Period, y = FS, group = rep(loc, length(f.t) - 1), colour = rep(loc, length(f.t) - 1))) + 
  geom_hline(yintercept = 1.0, color = "gray", size = 1) +
  geom_point(alpha = 0.2, size = 1.2) + theme_bw() + 
  geom_line(data = res.malm.avg, aes(x = Period, y = FS, group = Loc, colour = Loc), size = 1.2) + 
  scale_y_continuous(name = "Frontier Shift (FS)", limits = c(0.8, 1.3), breaks = seq(0.8, 1.3, 0.1)) +
  theme(legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "horizontal", 
        legend.justification = c(1, 1), legend.position = c(1, 1))

# Figure 8. MI (800*600)
ggplot(data = res.malm.raw$mi, 
       aes(x = Period, y = MI, group = rep(loc, length(f.t) - 1), colour = rep(loc, length(f.t) - 1))) + 
  geom_hline(yintercept = 1.0, color = "gray", size = 1) +
  geom_point(alpha = 0.2, size = 1.2) + theme_bw() + 
  geom_line(data = res.malm.avg, aes(x = Period, y = MI, group = Loc, colour = Loc), size = 1.2) + 
  scale_y_continuous(name = "Malmquist Index (MI)", limits = c(0.8, 1.3), breaks = seq(0.8, 1.3, 0.1)) +
  theme(legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "horizontal", 
        legend.justification = c(1, 1), legend.position = c(1, 1))

# Table 1. Rationale of MI changes
SP.2010.2013 <- (df.3d[,4, 7] - df.3d[,4,4])/df.3d[,4,4]*100/3
SP.2013.2016 <- (df.3d[,4,10] - df.3d[,4,7])/df.3d[,4,7]*100/3
IU.2010.2013 <- (df.3d[,6, 7] - df.3d[,6,4])/df.3d[,6,4]*100/3
IU.2013.2016 <- (df.3d[,6,10] - df.3d[,6,7])/df.3d[,6,7]*100/3

t(matrix(c(round(aggregate(IU.2010.2013, list(loc), mean, na.rm = T)$x, 2),
           round(aggregate(SP.2010.2013, list(loc), mean, na.rm = T)$x, 2),
           round(aggregate(IU.2013.2016, list(loc), mean, na.rm = T)$x, 2),
           round(aggregate(SP.2013.2016, list(loc), mean, na.rm = T)$x, 2)),
         nrow = 3, ncol = 4,
         dimnames = list(levels(loc), 
                         c("IU (2010-2013)", "SP (2010-2013)", "IU (2013-2016)", "SP (2013-2016)"))))

# Footnote 6. Regression MI ~ IU
IU.annual.c <- c(df.3d[,6, 2] - df.3d[,6, 1], df.3d[,6, 3] - df.3d[,6, 2], df.3d[,6, 4] - df.3d[,6, 3], 
                 df.3d[,6, 5] - df.3d[,6, 4], df.3d[,6, 6] - df.3d[,6, 5], df.3d[,6, 7] - df.3d[,6, 6],
                 df.3d[,6, 8] - df.3d[,6, 7], df.3d[,6, 9] - df.3d[,6, 8], df.3d[,6,10] - df.3d[,6, 9], 
                 df.3d[,6,11] - df.3d[,6,10])

summary(lm(res.malm.raw$mi$MI ~ IU.annual.c), na.rm = T)

# Figure 9. LPG vs LNG (vs Oil) prices (850*600)
p.eff    <- price[-c(1:24, 122:132),]
df.price <- data.frame(Tick  = rep(1:nrow(p.eff), 3),
                       Price = c(p.eff$Dubai_oil*10, p.eff$LNG, p.eff$LPG),
                       Type  = factor(rep(c("Oil", "LNG", "LPG"), each = nrow(p.eff)), levels = c("Oil", "LNG", "LPG")))

ggplot(data = na.omit(df.price), aes(x = Tick, y = Price, group = Type, colour = Type)) + 
  geom_line(size = 1.2) + theme_bw() + 
  scale_colour_manual(values = c("lightgrey", "royalblue", "orangered")) +
  scale_x_continuous(name = "Year", breaks = seq(1, nrow(p.eff) + 1, 12), labels = seq(2010, 2018, 1)) +
  scale_y_continuous(name = "Gas price (￦/9,393kcal)", limits = c(200, 1400), breaks = seq(200, 1400, 200),
                     sec.axis = sec_axis(~.*0.1, name = "Oil price - Dubai crude ($/B)", breaks = seq(20, 140, 20))) +
  theme(axis.title.x         = element_text(size = 14, colour = "gray35"),
        axis.title.y         = element_text(size = 14, colour = "gray35"),
        legend.title         = element_blank(),
        legend.background    = element_rect(fill = "transparent", colour = "transparent"), 
        legend.direction     = "horizontal", 
        legend.justification = c(1, 1), legend.position = c(1, 1))
