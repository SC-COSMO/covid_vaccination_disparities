

pops_16 <- fread("~/COVID/vaccination_analysis/covid_vaccination_disparities_PPML/prepped/pops_16.csv")
df <- fread("~/COVID/vaccination_analysis/covid_vaccination_disparities_PPML/prepped/effective_demand_weights_state.csv")
df <- merge(df, pops_16, by = c("state_name", "race_grp"))

df <- df[race_grp=="White", nhw:=effective_demand]
df <- df[, nhw:=mean(nhw, na.rm=T), by = c("state_name")]
df <- df[, rr:=nhw/effective_demand]

quantile(df$rr[df$race_grp=="Black" & df$state_pop>=200000], na.rm=T)
quantile(df$rr[df$race_grp=="Hispanic" & df$state_pop>=200000], na.rm=T)

plot_data <- copy(df)
plot_data <- plot_data[state_pop<500000, pop_cat:="0.2-0.4"]
plot_data <- plot_data[state_pop<1000000 & state_pop>=500000, pop_cat:="0.5-0.9"]
plot_data <- plot_data[state_pop>=1000000 & state_pop<2000000, pop_cat:="1.0-1.9"]
plot_data <- plot_data[state_pop>=2000000, pop_cat:="2.0+"]
plot_data <- plot_data[, pop_cat:=factor(pop_cat, levels = c("0.2-0.4", "0.5-0.9", "1.0-1.9", "2.0+"))]

plot_data <- plot_data[is.na(effective_demand) & race_grp=="White", missing_data:=1]
plot_data <- plot_data[, missing_data:=mean(missing_data, na.rm=T), by = "state_name"]
plot_data <- plot_data[, state_name_label:=state_name]
plot_data <- plot_data[missing_data==1, state_name_label:=paste0(state_name, "*")]


pdf("~/Documents/Vax Disparities Letter/JNO Submission/Figure 1_Resubmission_logtransform_FINAL.pdf", width = 8, height = 10)
ggplot(data = plot_data[race_grp%in%c("Hispanic", "White", "Asian", "Black") & state_pop>200000],
       aes(x = effective_demand, y = state_name_label, color = race_grp, size = pop_cat)) + 
  geom_point(data = plot_data[race_grp%in%c("White") & state_pop>200000], alpha = .8) +
  geom_point(data = plot_data[race_grp%in%c("Hispanic", "Asian", "Black") & state_pop>200000], alpha = .8) +
  scale_x_continuous(trans = 'log', breaks = c(.5, .75, 1, 1.25, 1.5, 1.75, 2)) +
  # scale_x_continuous(limits = c(0.25, 1.75), expand = c(0,0)) +
  # scale_x_continuous(limits = c(-.8, 0.8), expand = c(0,0)) +
  geom_vline(xintercept = 1) +
  theme_bw() +
  labs(x = "Relative Rates of Uptake", y = "", color = "Race/Ethnicity",
       size = "Population\n(Millions)", title = "Figure 1",
       caption = "*State data not reported by race/ethnicity as of March 31, 2021") +
  scale_y_discrete(limits = rev(levels(as.factor(plot_data$state_name_label)))) +
  theme(legend.position = "bottom") +
  guides(color=guide_legend(nrow=2,byrow=TRUE, override.aes = list(size=5))) +
  guides(size=guide_legend(nrow=2,byrow=TRUE)) +
  scale_color_manual(values = c("American Indian or Alaska Native" = "#D56D27",
                                "Native Hawaiian or Other Pacific Islander" = "#3D6879",
                                "Hispanic" = "#c42e31", "White" = "#832543",
                                "Asian" = "#6399AC", "Black" = "#e5a825")) +
  scale_size_manual(values = c(1, 2, 4, 6)) +
  theme(legend.box = "horizontal") +
  theme(text = element_text(size = 12), plot.margin = margin(1,1,1,1, unit = "cm"))
dev.off()


files <- list.files("~/COVID/vaccination_analysis/covid_vaccination_disparities_PPML/results/", pattern = ".csv")
df <- NULL
for (f in files) {
  temp <- fread(paste0("~/COVID/vaccination_analysis/covid_vaccination_disparities_PPML/results/", f))
  temp <- temp[, daily_vax:=sum(daily_vax, na.rm=T), by = c("race_grp", "day", "scenario")]
  temp <- temp[, vaccinated:=sum(vaccinated, na.rm=T), by = c("race_grp", "day", "scenario")]
  temp <- temp[, state_pop:=sum(state_pop, na.rm=T), by = c("race_grp", "day", "scenario")]
  temp <- unique(temp[,.(race_grp, daily_vax, vaccinated, day, state_pop, scenario)])
  df <- rbind(df, temp, fill = T)
}

all <- NULL
for (f in files) {
  temp <- fread(paste0("~/COVID/vaccination_analysis/covid_vaccination_disparities_PPML/results/", f))
  temp <- temp[, daily_vax:=sum(daily_vax, na.rm=T), by = c("day", "scenario")]
  temp <- temp[, vaccinated:=sum(vaccinated, na.rm=T), by = c("day", "scenario")]
  temp <- temp[, state_pop:=sum(state_pop, na.rm=T), by = c("day", "scenario")]
  temp <- temp[, race_grp:="All"]
  temp <- unique(temp[,.(race_grp, daily_vax, vaccinated, day, state_pop, scenario)])
  all <- rbind(all, temp, fill = T)
}

df <- rbind(df, all, fill =T)

df <- df[, coverage:=vaccinated/state_pop]

df <- df[scenario%in%c("Status Quo", "Equalized Uptake", 
                       "Equalized Uptake and Geographic Targeting")]
df <- df[scenario=="Status Quo", scenario:="Persistent Differential Uptake"]

df <- df[, scenario_label:=factor(scenario, levels = c("Persistent Differential Uptake",
                                                       "Equalized Uptake", "Equalized Uptake and Geographic Targeting"))]

df <- df[coverage>=.5, min_day_50:=min(day), by = c("race_grp", "scenario")]
df <- df[min_day_50==day, point_50:=.5]
df <- df[coverage>=.7, min_day_75:=min(day), by = c("race_grp", "scenario")]
df <- df[min_day_75==day, point_75:=.7]

df <- df[day<="2021-07-01"]

nat <- fread("~/COVID/vaccination_analysis/covid_vaccination_disparities_PPML/prepped/national_time_series.csv")
nat <- nat[Date=="2021-07-01"]
nat <- nat[race_grp%in%c("Asian", "Black", "Hispanic", "White")]
nat_temp <- nat[race_grp=="Asian"]
nat_temp <- nat_temp[, race_grp:="All"]
nat_temp <- nat_temp[, nat_race:=nat_race_agg]
nat <- rbind(nat, nat_temp)

nat <- nat[, Date:=ymd("2021-07-04")]

pdf("~/Documents/Vax Disparities Letter/JNO Submission/Figure 2_Resubmission_Final_v5_smooth.pdf", width = 12, height = 7)
ggplot(data = df[race_grp%in%c("All", "Asian", "White", "Black", "Hispanic") & state_pop>=200000],
       aes(x = day, y = coverage)) +
  geom_rect(aes(xmin=ymd("2021-07-01"), xmax=ymd("2021-07-08"), ymin=0, ymax=Inf), fill = "#eae9e9", alpha = .4) +
  geom_line(data = df[race_grp%in%c("All") & state_pop>=200000], aes(color = race_grp, linetype = race_grp), size = 1, alpha = .8, show.legend = FALSE) +
  # geom_point(data = nat[race_grp!="All"], aes(x = Date, y = nat_race/100), size = 4, shape = 18, alpha = .8) +
  geom_hpline(data = nat[race_grp!="All"], aes(x = Date, y = nat_race/100, color = race_grp, linetype = race_grp), width = 6, size = 1.4) +
  geom_line(data = df[race_grp%in%c("Asian", "White", "Black", "Hispanic") & state_pop>=200000], aes(color = race_grp, linetype = race_grp),  size = 1.3, alpha = .96) +
  geom_point(data = df[race_grp%in%c("Asian", "White", "Black", "Hispanic") & state_pop>=200000],
             aes(y = point_50, color = race_grp, linetype = race_grp), fill = "white", shape = 21, size = 1.4, stroke =1.4, show.legend = FALSE) +
  geom_point(data = df[race_grp%in%c("Asian", "White", "Black", "Hispanic") & state_pop>=200000],
             aes(y = point_75, color = race_grp, linetype = race_grp), fill = "white", shape = 21, size = 1.4, stroke =1.4, show.legend = FALSE) +
  theme_bw() + labs(x = "", y = "Coverage of 1+ doses among population\n18 years and older", color = "", fill = "",
                    title = "Figure 2", linetype = "") +
                    #caption = "State-specific figures available in supplemental materials.") +
  scale_fill_manual(breaks= c("Hispanic", "White", "Black", "Asian"), values = c("All" = "#767676", "Hispanic" = "#c42e31", "White" = "#832543", "Asian" = "#6399AC", "Black" = "#e5a825")) + #https://coolors.co/e5a825-d56d27-c42e31-832543-3d6879-6399ac
  scale_color_manual(breaks= c("Hispanic", "White", "Black", "Asian"), values = c("All" = "#767676", "Hispanic" = "#c42e31", "White" = "#832543", "Asian" = "#6399AC", "Black" = "#e5a825")) +
  scale_linetype_manual(breaks= c("Hispanic", "White", "Black", "Asian"), values = c("All" = "dashed", "Hispanic" = "solid", "White" = "solid", "Asian" = "solid", "Black" = "solid")) +
  theme(text = element_text(size = 12), legend.position = "bottom", strip.text = element_text(size = 10)) +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  facet_wrap(~scenario_label, ncol = 4) +
  scale_y_continuous(labels = scales::percent, breaks = c(0, .1, .2, .3, .4, .5, .6, .7, .8, 0.9), limits = c(0, 1), expand = c(0,0)) +
  theme(panel.grid.minor = element_blank(), strip.background =element_rect(fill="white")) +
  theme(legend.key.width = unit(1.2,"cm"), panel.grid.major.y = element_blank(), plot.margin = margin(2, 2, 2, 2, "cm")) +
  coord_cartesian(xlim = c(ymd("2021-04-01"), ymd("2021-07-02"))) +
  annotate(geom = "text", x = ymd("2021-07-03"), vjust = 0.6, y = 0.17, label = "Estimated Actuals", angle = 90)
dev.off()



files <- list.files("~/COVID/vaccination_analysis/covid_vaccination_disparities_PPML/results/", pattern = ".csv")

df <- NULL
for (f in files) {
  temp <- fread(paste0("~/COVID/vaccination_analysis/covid_vaccination_disparities_PPML/results/", f))
  df <- rbind(df, temp, fill = T)
}

all <- NULL
for (f in files) {
  temp <- fread(paste0("~/COVID/vaccination_analysis/covid_vaccination_disparities_PPML/results/", f))
  temp <- temp[, daily_vax:=sum(daily_vax, na.rm=T), by = c("state_name", "day", "scenario")]
  temp <- temp[, vaccinated:=sum(vaccinated, na.rm=T), by = c("state_name", "day", "scenario")]
  temp <- temp[, state_pop:=sum(state_pop, na.rm=T), by = c("state_name", "day", "scenario")]
  temp <- temp[, race_grp:="All"]
  temp <- unique(temp[,.(race_grp, daily_vax, vaccinated, state_name, day, state_pop, scenario)])
  all <- rbind(all, temp, fill = T)
}

df <- rbind(df, all, fill =T)

df <- df[, coverage:=vaccinated/state_pop]

df <- df[scenario%in%c("Status Quo", "Equalized Uptake", 
                       "Equalized Uptake and Geographic Targeting")]
df <- df[scenario=="Status Quo", scenario:="Persistent Differential Uptake"]

df <- df[, scenario_label:=factor(scenario, levels = c("Persistent Differential Uptake",
                                                       "Equalized Uptake", "Equalized Uptake and Geographic Targeting"))]

df <- df[coverage>=.5, min_day_50:=min(day), by = c("state_name", "race_grp", "scenario")]
df <- df[min_day_50==day, point_50:=.5]
df <- df[coverage>=.7, min_day_75:=min(day), by = c("state_name", "race_grp", "scenario")]
df <- df[min_day_75==day, point_75:=.7]

df <- df[,.(state_name, race_grp, day, coverage, scenario, scenario_label, min_day_50, point_50, min_day_75, point_75, state_pop)]
df <- df[day<="2021-07-01"]

nat <- fread("~/COVID/vaccination_analysis/covid_vaccination_disparities_PPML/prepped/coverage_time_series.csv")
setnames(nat, "vaccinated_pct_12", "nat_race")
nat <- nat[race_grp%in%c("All","Asian", "Black", "Hispanic", "White")]
nat <- nat[Date=="2021-07-01"]
nat <- nat[, day:=ymd("2021-07-01")]
temp <- merge(df, nat, by = c("race_grp", "day", "state_name"))


nrow(temp[race_grp=="Black" & coverage < nat_race/100 & scenario=="Equalized Uptake" & state_pop>=200000 & missing == 0])
nrow(temp[race_grp=="Black" & scenario=="Equalized Uptake" & state_pop>=200000 & missing == 0])

nrow(temp[race_grp=="Hispanic" & coverage < nat_race/100 & scenario=="Equalized Uptake" & state_pop>=200000 & missing == 0])
nrow(temp[race_grp=="Hispanic" & scenario=="Equalized Uptake" & state_pop>=200000 & missing == 0])

nrow(temp[race_grp=="Black" & coverage < nat_race/100 & scenario=="Equalized Uptake and Geographic Targeting" & state_pop>=200000 & missing == 0])
nrow(temp[race_grp=="Hispanic" & coverage < nat_race/100 & scenario=="Equalized Uptake and Geographic Targeting" & state_pop>=200000 & missing == 0])


nat <- nat[, Date:=ymd("2021-07-04")]

pdf("~/Documents/Vax Disparities Letter/JNO Submission/Figure 2_Resubmission_Final_v5_State_smooth.pdf", width = 12, height = 7)
for (s in unique(df$state_name)) {
  races <- unique(df$race_grp[df$race_grp%in%c("Asian", "White", "Black", "Hispanic") & df$state_pop>=200000 & df$state_name==s])
  races2 <- unique(nat$race_grp[nat$missing==0 & nat$state_name==s])
  races <- races[races%in%races2]
  plot <- ggplot(data = df[state_name == s & race_grp%in%c("All", "Asian", "White", "Black", "Hispanic") & state_pop>=200000],
         aes(x = day, y = coverage)) +
    geom_rect(aes(xmin=ymd("2021-07-01"), xmax=ymd("2021-07-08"), ymin=0, ymax=Inf), fill = "#eae9e9", alpha = .4) +
    geom_line(data = df[state_name == s & race_grp%in%c("All") & state_pop>=200000], aes(color = race_grp, linetype = race_grp), size = 1, alpha = .8, show.legend = FALSE) +
    geom_hpline(data = nat[state_name == s & race_grp%in%races & missing==0], aes(x = Date, y = nat_race/100, color = race_grp, linetype = race_grp), width = 6, size = 1.4) +
    geom_line(data = df[state_name == s & race_grp%in%c("Asian", "White", "Black", "Hispanic") & race_grp%in%races & state_pop>=200000], aes(color = race_grp, linetype = race_grp),  size = 1.3, alpha = .96) +
    geom_point(data = df[state_name == s & day!="2021-04-01" & race_grp%in%c("Asian", "White", "Black", "Hispanic") & race_grp%in%races & state_pop>=200000],
               aes(y = point_50, color = race_grp, linetype = race_grp), fill = "white", shape = 21, size = 1.4, stroke =1.4, show.legend = FALSE) +
    geom_point(data = df[state_name == s & day!="2021-04-01" & race_grp%in%c("Asian", "White", "Black", "Hispanic") & race_grp%in%races & state_pop>=200000],
               aes(y = point_75, color = race_grp, linetype = race_grp), fill = "white", shape = 21, size = 1.4, stroke =1.4, show.legend = FALSE) +
    theme_bw() + labs(x = "", y = "Coverage of 1+ doses among population\n18 years and older", color = "", fill = "",
                      title = paste0(s), linetype = "", caption = "Dashed line shows overall coverage, scenarios for specific racial/ethnic groups shown for\npopulations > 200K and data by race/ethnicity reported by state.") +
    scale_fill_manual(breaks= c("Hispanic", "White", "Black", "Asian"), values = c("All" = "#767676", "Hispanic" = "#c42e31", "White" = "#832543", "Asian" = "#6399AC", "Black" = "#e5a825")) + #https://coolors.co/e5a825-d56d27-c42e31-832543-3d6879-6399ac
    scale_color_manual(breaks= c("Hispanic", "White", "Black", "Asian"), values = c("All" = "#767676", "Hispanic" = "#c42e31", "White" = "#832543", "Asian" = "#6399AC", "Black" = "#e5a825")) +
    scale_linetype_manual(breaks= c("Hispanic", "White", "Black", "Asian"), values = c("All" = "dashed", "Hispanic" = "solid", "White" = "solid", "Asian" = "solid", "Black" = "solid")) +
    theme(text = element_text(size = 12), legend.position = "bottom", strip.text = element_text(size = 10)) +
    guides(color=guide_legend(nrow=1,byrow=TRUE)) +
    facet_wrap(~scenario_label, ncol = 4) +
    scale_y_continuous(labels = scales::percent, breaks = c(0, .1, .2, .3, .4, .5, .6, .7, .8, 0.9), limits = c(0, 1), expand = c(0,0)) +
    theme(panel.grid.minor = element_blank(), strip.background =element_rect(fill="white")) +
    theme(legend.key.width = unit(1.2,"cm"), panel.grid.major.y = element_blank(), plot.margin = margin(1, 1, 1, 1, "cm")) +
    coord_cartesian(xlim = c(ymd("2021-04-01"), ymd("2021-07-02"))) +
    annotate(geom = "text", x = ymd("2021-07-03"), vjust = 0.6, y = 0.17, label = "Estimated Actuals", angle = 90)
  print(plot)
  
}
dev.off()
 