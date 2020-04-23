library(plm)
library(stargazer)
df <- read.csv("data/all_congresses.csv")
recent_df <- df[df$congress >= 103, ]

filter_nas <- function(v) {
  return(v[!is.na(v)])
}

fields <- list(
  congress = "congress",
  congress_factor = "factor(congress)",
  experience = "experience",
  leg_id = "leg_id",
  bills_cosponsored = "bills_cosponsored",
  bills_sponsored = "bills_sponsored",
  cosponsored_sponsored_ratio = "cosponsored_sponsored_ratio",
  cosponsors_per_bill = "cosponsors_per_bill",
  chamber_factor = "factor(chamber)",
  chamber = "chamber",
  committee_count = "committee_count",
  committee_min_rank = "committee_min_rank", 
  committee_rank_recips = "committee_rank_recips",
  max_coeff = "max_coeff",
  leadership = "leadership",
  nominate_variance = "nominate_variance",
  same_party_cosponsors_prop = "same_party_cosponsors_prop",
  enacted = "enacted", 
  sponsor_party_factor = "factor(sponsor_party)",
  committee_max_coeff = "committee_max_coeff",
  after_reform = "after_reform"
)
cross <- function(a, b) {
  return(paste(c(a, b), collapse = "*"))
}
reg_result <- function(data, outcome, independent, controls = c(), fe = T, 
                       fe_index = c("leg_id", "congress"), full = T) {
  variables <- c(independent, controls)
  reg_formula <- as.formula(paste(c(outcome, 
          paste(
            variables,
            collapse = " + "
          )),
    collapse = " ~ "))
  if(fe) {
    s <- plm(reg_formula,
                     index = fe_index,
                     effect = "twoway",
                     model = "within",
                     data = data)
  }
  else {
    s <- lm(reg_formula, data = data)
  }
  if(full) {
    return(s)
  }
  else {
    return(summary(s))
  }
}

all_leg_regs <- function(independent, controls, fe = T, fe_index = c("leg_id", "congress"), full = T, data = recent_df){
  return(list(
      bills_cosponsored = reg_result(data, fields$bills_cosponsored, independent, controls = controls, 
                                     fe = fe, fe_index = fe_index, full = full),
      bills_sponsored = reg_result(data, fields$bills_sponsored, independent, controls = controls, 
                                     fe = fe, fe_index = fe_index, full = full),
      cosponsors_per_bill = reg_result(data, fields$cosponsors_per_bill, independent, controls = controls, 
                                   fe = fe, fe_index = fe_index, full = full),
      cosponsored_sponsored_ratio = reg_result(data, fields$cosponsored_sponsored_ratio, independent, controls = controls, 
                                       fe = fe, fe_index = fe_index, full = full)
  ))
}

reg_result(recent_df, fields$cosponsors_per_bill, fields$bills_cosponsored, 
               controls = c(fields$experience, fields$chamber, fields$leadership, fields$committee_count, 
                            fields$committee_min_rank, fields$committee_rank_recips), full = T)

# OLS regressions on experience
experience_regs <- all_leg_regs(fields$experience, controls = c(fields$chamber_factor), fe = F)

# FE regressions on experience
experience_fe_regs <- all_leg_regs(fields$experience, controls = c(fields$chamber_factor), data = recent_df)

# FE regressions on committee count, no experience control
committee_count_no_exp_regs <- all_leg_regs(fields$committee_count, controls = c(fields$chamber_factor))

# FE regressions on committee count, experience control
committee_count_regs <- all_leg_regs(fields$committee_count, controls = c(fields$experience, fields$chamber_factor), full = T)

# FE regressions on min committee rank, no experience control
committee_min_ranks_no_exp_regs <- all_leg_regs(fields$committee_min_rank, controls = c(fields$chamber_factor))

# FE regressions on min committee rank, experience control
committee_min_ranks_regs <- all_leg_regs(fields$committee_min_rank, controls = c(fields$experience, fields$chamber_factor))

# FE regressions on max committee coeff, no experience control
committee_max_coeff_no_exp_regs <- all_leg_regs(fields$max_coeff, controls = c(fields$chamber_factor))

# FE regressions on max committee coeff, experience control
committee_max_coeff_regs <- all_leg_regs(fields$max_coeff, controls = c(fields$experience, fields$chamber_factor))

# FE regressions on committee rank recips, no experience control
committee_rank_recips_no_exp_regs <- all_leg_regs(fields$committee_rank_recips, controls = c(fields$chamber_factor))

# FE regressions on committee rank recips, experience control
committee_rank_recips_regs <- all_leg_regs(fields$committee_rank_recips, controls = c(fields$experience, fields$chamber_factor))

leadership_regs <- all_leg_regs(fields$leadership, controls = c(fields$experience, fields$chamber_factor))

all_leg_regs(cross(fields$max_coeff, fields$after_reform), 
             controls = c(fields$committe_max_coeff, fields$after_reform, fields$experience, fields$chamber_factor), 
             fe = T, full = T)

lm(cosponsors_per_bill ~ max_coeff + after_reform + factor(chamber) + committee_max_coeff * after_reform, data = df)

# stargazer cheatsheet: https://www.jakeruss.com/cheatsheets/stargazer/

stargazer(experience_regs$cosponsors_per_bill,  
          experience_fe_regs$cosponsors_per_bill, 
          committee_count_regs$cosponsors_per_bill, 
          committee_min_ranks_regs$cosponsors_per_bill,
          committee_max_coeff_regs$cosponsors_per_bill,
          committee_rank_recips_regs$cosponsors_per_bill,
          leadership_regs$cosponsors_per_bill,
          omit.stat = c("f", "ser"), 
          title = "Cosponsors per bill", 
          add.lines = list(c("Fixed effects?", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
          order = c(7, 6, 1, 2, 3, 4, 5, 8),
          covariate.labels = c("Chamber (Senate)", "Experience (# of sessions)", "Committee count", 
                               "Min committee rank", "Max committee coefficient", "Committee rank reciprocals", 
                               "Leadership"),
          dep.var.labels = c("Number of cosponsors per bill sponsored in legislative session"), 
          star.cutoffs = c(0.05, 0.01, 0.001)
          )


stargazer(experience_regs$cosponsors_per_bill,  
          experience_fe_regs$cosponsors_per_bill, 
          committee_count_regs$cosponsors_per_bill, 
          committee_min_ranks_regs$cosponsors_per_bill,
          committee_max_coeff_regs$cosponsors_per_bill,
          committee_rank_recips_regs$cosponsors_per_bill,
          leadership_regs$cosponsors_per_bill,
          omit.stat = c("f", "ser"), 
          title = "Cosponsors per bill", 
          add.lines = list(c("Fixed effects?", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
          order = c(7, 6, 1, 2, 3, 4, 5, 8),
          covariate.labels = c("Chamber (Senate)", "Experience (# of sessions)", "Committee count", 
                               "Min committee rank", "Max committee coefficient", "Committee rank reciprocals", 
                               "Leadership"),
          dep.var.labels = c("Number of cosponsors per bill sponsored in legislative session"), 
          star.cutoffs = c(0.05, 0.01, 0.001)
)

summary(reg_result(df, fields$cosponsors_per_bill, fields$committee_count, 
           controls = c(fields$chamber_factor, fields$experience, fields$bills_sponsored)))

reciprocal_reg <- reg_result(recent_df, fields$cosponsors_per_bill, fields$bills_cosponsored, 
                             controls = c(fields$bills_sponsored, fields$chamber_factor))
reciprocal_reg_prestige_controls <- reg_result(recent_df, fields$cosponsors_per_bill, fields$bills_cosponsored, 
                  controls = c(fields$experience, fields$bills_sponsored, fields$leadership, fields$committee_count,
                               fields$committee_rank_recips, fields$committee_min_rank, fields$max_coeff, 
                               fields$chamber_factor))

stargazer(reciprocal_reg,
          reciprocal_reg_prestige_controls,
          omit.stat = c("f", "ser"), 
          title = "Reciprocal cosponsorship", 
          add.lines = list(c("Fixed effects?", "Yes", "Yes")),
          order = c(9, 1, 3, 2, 4, 5, 6, 7, 8), 
          covariate.labels = c("Chamber (Senate)", "Bills cosponsored", "Bills sponsored",
                               "Experience", "Leadership", "Committee count", "Committee rank reciprocals",
                               "Min committee rank", "Max committee coefficient"),
          dep.var.labels = c("Cosponsors per bill"), 
          star.cutoffs = c(0.05, 0.01, 0.001)
)

bills_df <- read.csv("data/bill_infos.csv")



recent_bills_with_zeros <- bills_df[bills_df$congress >= 103, ]
recent_bills_df <- recent_bills_with_zeros[recent_bills_with_zeros$total_cosponsors > 0, ]

summary(reg_result(recent_bills_df, "same_party_cosponsors_prop", "cosponsor_leadership", 
           controls = c("max_cosponsor_experience", "enacted", fields$chamber_factor, fields$congress_factor, 
                        fields$sponsor_party_factor), fe = F, full = T))

mod <- lm(same_party_cosponsors_prop ~ cosponsor_leadership + enacted + factor(chamber) + factor(congress) + 
     factor(sponsor_party), data = recent_bills_df)

basic_bill_controls <- c(fields$chamber_factor, fields$congress_factor)
enacted_simple_regs <- list(
  total_cosponsors = reg_result(recent_bills_df, fields$enacted, "total_cosponsors",
                                controls = c(basic_bill_controls), 
                                fe = F, full = T),
  same_party_prop = reg_result(recent_bills_df, fields$enacted, "same_party_cosponsors_prop",
                               controls = c(basic_bill_controls), 
                               fe = F, full = T),
  nominate_variance = reg_result(recent_bills_df, fields$enacted, "nominate_variance",
                                    controls = c(basic_bill_controls), 
                                    fe = F, full = T),
  joint =  reg_result(recent_bills_df, fields$enacted, "total_cosponsors",
                      controls = c("same_party_cosponsors_prop", "nominate_variance", basic_bill_controls), 
                      fe = F, full = T)
)

stargazer(enacted_simple_regs,
          omit = c("congress"),
          omit.stat = c("f", "ser"), 
          title = "Bill enactment probability", 
          covariate.labels = c("Chamber (Senate)", "Total cosponsors", 
                               "Same party cosponsors proportion", "Nominate variance"), 
          order = c(4, 1, 2, 3),
          dep.var.labels = c("Bill enacted"), 
          add.lines = list(c("Legislative session fixed effects?", "Yes", "Yes", "Yes", "Yes")),
          star.cutoffs = c(0.05, 0.01, 0.001)
)

basic_bill_controls <- c(fields$chamber_factor, fields$congress_factor, "total_cosponsors")
nominate_variance_no_props <- list(
  experience = reg_result(recent_bills_df, "nominate_variance", "experience",
                          controls = c(basic_bill_controls), 
                          fe = F, full = T),
  leadership = reg_result(recent_bills_df, "nominate_variance", "leadership",
                          controls = c("experience", basic_bill_controls), 
                          fe = F, full = T),
  committee_count = reg_result(recent_bills_df, "nominate_variance", "committee_count",
                          controls = c("experience", basic_bill_controls), 
                          fe = F, full = T),
  min_rank = reg_result(recent_bills_df, "nominate_variance", "committee_min_rank",
                        controls = c("experience", basic_bill_controls), 
                        fe = F, full = T),
  recips = reg_result(recent_bills_df, "nominate_variance", "committee_rank_recips",
                      controls = c("experience", basic_bill_controls), 
                      fe = F, full = T)
)

nominate_variance_props <- list(
  leadership = reg_result(recent_bills_df, "nominate_variance", "leadership",
                          controls = c("experience", "same_party_cosponsors_prop", basic_bill_controls), 
                          fe = F, full = T),
  committee_count = reg_result(recent_bills_df, "nominate_variance", "committee_count",
                          controls = c("experience","same_party_cosponsors_prop",  basic_bill_controls), 
                          fe = F, full = T),
  min_rank = reg_result(recent_bills_df, "nominate_variance", "committee_min_rank",
                        controls = c("experience", "same_party_cosponsors_prop", basic_bill_controls), 
                        fe = F, full = T),
  recips = reg_result(recent_bills_df, "nominate_variance", "committee_rank_recips",
                      controls = c("experience", "same_party_cosponsors_prop", basic_bill_controls), 
                      fe = F, full = T)
)

same_party_prop_regs <- list(
  experience = reg_result(recent_bills_df, "same_party_cosponsors_prop", "experience",
                          controls = c(basic_bill_controls), 
                          fe = F, full = T),
  leadership = reg_result(recent_bills_df, "same_party_cosponsors_prop", "leadership",
                          controls = c("experience", basic_bill_controls), 
                          fe = F, full = T),
  committee_count = reg_result(recent_bills_df, "same_party_cosponsors_prop", "committee_count",
                               controls = c("experience", basic_bill_controls), 
                               fe = F, full = T),
  min_rank = reg_result(recent_bills_df, "same_party_cosponsors_prop", "committee_min_rank",
                        controls = c("experience", basic_bill_controls), 
                        fe = F, full = T),
  recips = reg_result(recent_bills_df, "same_party_cosponsors_prop", "committee_rank_recips",
                      controls = c("experience", basic_bill_controls), 
                      fe = F, full = T)
)

stargazer(same_party_prop_regs, 
          omit = c("congress"),
          omit.stat = c("f", "ser"), 
          title = "Same party cosponsorship proportion by sponsor traits",
          covariate.labels = c("Chamber (Senate)", "Experience", 
                               "Leadership", "Committee count", 
                               "Minimum committee rank", "Committee rank reciprocals"), 
          order = c(6, 5, 6, 1, 2, 3, 4),
          dep.var.labels = c("Same party cosponsorship proportion"), 
          add.lines = list(c("Legislative session fixed effects?", "Yes", "Yes", "Yes", "Yes", 
                               "Yes")),
          star.cutoffs = c(0.05, 0.01, 0.001)
)

stargazer(c(nominate_variance_no_props, nominate_variance_props), 
          omit = c("congress"),
          omit.stat = c("f", "ser"), 
          title = "Nominate variance by sponsor traits",
          float.env = "sidewaystable",
          covariate.labels = c("Chamber (Senate)", "Experience", "Same party cosponsors proportion", 
                               "Leadership", "Committee count", 
                               "Minimum committee rank", "Committee rank reciprocals"), 
          order = c(7, 5, 6, 1, 2, 3, 4),
          dep.var.labels = c("Nominate variance"), 
          add.lines = list(c("Legislative session fixed effects?", "Yes", "Yes", "Yes", "Yes", 
                             "Yes", "Yes", "Yes", "Yes", "Yes")),
          star.cutoffs = c(0.05, 0.01, 0.001)
)

           
summary(reg_result(recent_bills_df, fields$enacted, "total_cosponsors", 
           controls = c(fields$chamber_factor, fields$congress_factor, fields$experience, 
                        fields$leadership)
              , fe = F, full = T))

reg_result(recent_bills_df, "total_cosponsors", "max_cosponsor_committee_rank_recips",  
           controls = c(fields$chamber_factor, fields$congress_factor, "cosponsor_leadership", "max_cosponsor_experience"),
           fe = F, full = T)

summary(reg_result(recent_bills_df, fields$nominate_variance, fields$leadership, 
           c(fields$chamber_factor, fields$congress_factor, fields$experience, "same_party_cosponsors_prop", 
             "max_cosponsor_experience", "total_cosponsors"),
           fe = F, full = T))

reg_result(recent_bills_df, "same_party_cosponsors_prop", fields$leadership,
           c(fields$chamber_factor, fields$congress_factor, fields$experience,
             "max_cosponsor_experience", "total_cosponsors"),
           fe = F, full = T)
sd(recent_bills_df$total_cosponsors)
recent_senate_bills_df = recent_bills_df[recent_bills_df$chamber == "senate", ]
recent_house_bills_df = recent_bills_df[recent_bills_df$chamber == "house", ]

outliers <- function(data, threshold = 1.5) {
  lowerq = quantile(data)[[2]]
  upperq = quantile(data)[[4]]
  iqr = IQR(data)
  print(upperq + threshold * iqr)
  return(data[data > upperq + threshold * iqr | data < lowerq - threshold * iqr])
}

df2 <- read.csv("data/former_legs.csv"); df2$time_since <- 116 - df2$last_congress; df2$senate_indicator = df2$chamber == "senate"

basic_controls = c("factor(last_congress)", "senate_indicator")

lobbying_basic_regs <- list(
  chamber = reg_result(df2, "lobbyist", "senate_indicator", fe = F),
  experience = reg_result(df2, "lobbyist", fields$experience, controls = c(basic_controls), fe = F),
  time_since = reg_result(df2, "lobbyist", "time_since", controls = c("senate_indicator"), fe = F),
  party = reg_result(df2, "lobbyist", "party", controls = c(basic_controls), fe = F),
  joint = reg_result(df2, "lobbyist", "experience", 
                     controls = c("senate_indicator", "party", "time_since"), fe = F),
  joint_fe = reg_result(df2, "lobbyist", "experience", 
                              controls = c("senate_indicator", "party", "senate_indicator"), 
                              fe = F)
)


stargazer(lobbying_basic_regs, 
          omit = c("congress", "Constant"),
          omit.stat = c("f", "ser"), 
          title = "Probability of becoming a lobbyist",
          # float.env = "sidewaystable",
          covariate.labels = c("Experience", "Multiple parties", "Republican", 
                                "Sessions since leaving", "Chamber (Senate)"), 
          dep.var.labels = c("Became lobbyist"), 
          add.lines = list(c("Last session fixed effects?", "Yes", "Yes", "No", "Yes", "No", "Yes")),
          star.cutoffs = c(0.05, 0.01, 0.001)
)

relation_controls <- c("factor(last_congress)", "factor(party)", "experience", "factor(chamber)")
lobbying_relation_regs <- list(
  cur_relations =  reg_result(df2, "lobbyist", "cur_relations_score", 
                              controls = c(relation_controls), fe = F),
  remaining_friends = reg_result(df2, "lobbyist", "remaining_friends", controls = c(relation_controls), 
                                 fe = F, logit = T),
  last_cosponsored = reg_result(df2, "lobbyist", "last_cosponsored", controls = c(relation_controls), 
                                 fe = F, logit = T),
  last_cosponsors_per_bill =  reg_result(df2, "lobbyist", "last_cosponsors_per_bill", controls = c(relation_controls), 
                                        fe = F, logit = T)
)


stargazer(lobbying_relation_regs, 
          omit = c("factor", "Constant", "party", "indicator", "experience"),
          omit.stat = c("f", "ser"), 
          title = "Post-politics lobbying and legisative relationships",
          # float.env = "sidewaystable",
          covariate.labels = c("Current relations score", "Remaining friends", 
                               "Bills cosponsored in last session", "Cosponsors per bill in last session"), 
          dep.var.labels = c("Became lobbyist"), 
          # add.lines = list(c("Last session fixed effects?", "Yes", "Yes", "No", "Yes", "No", "Yes")),
          star.cutoffs = c(0.05, 0.01, 0.001)
)

lobbying_prestige_regs <- list(
  leadership = reg_result(df2, "lobbyist", "last_leadership", 
                          controls = c(relation_controls),
                           fe = F),
  committee_count = reg_result(df2, "lobbyist", "last_committee_count", controls = c(relation_controls),
                               fe = F),
  committee_min_rank = reg_result(df2, "lobbyist", "last_min_committee_rank", controls = c(relation_controls),
                                  fe = F),
  committee_max_coeff = reg_result(df2, "lobbyist", "last_max_coeff", controls = c(relation_controls),
                                   fe = F),
  committee_rank_recips = reg_result(df2, "lobbyist", "last_committee_rank_recips", 
                                     controls = c(relation_controls),
                                     fe = F)
)

stargazer(lobbying_prestige_regs, 
          omit = c("factor", "Constant", "party", "indicator", "experience"),
          omit.stat = c("f", "ser"), 
          title = "Post-politics lobbying and political prestige",
          # float.env = "sidewaystable",
          covariate.labels = c("Last session leadership", "Last session committee count",
                               "Last session committee rank recips", "Last session min committee rank",
                               "Last session max committeee coefficient"), 
          dep.var.labels = c("Became lobbyist"), 
          # add.lines = list(c("Last session fixed effects?", "Yes", "Yes", "No", "Yes", "No", "Yes")),
          star.cutoffs = c(0.05, 0.01, 0.001)
)
summary(lm(lobbyist ~ cur_relations_score + factor(last_congress), data = df2))

summary(lm(lobbyist ~ last_committee_count + experience + factor(chamber) + factor(party) 
           + factor(last_congress), data = df2))

df2summary(lm(lobbyist ~ factor(party) + factor(last_congress) + factor(chamber), data = df2))

summary(lm(lobbyist ~ leadership + experience + factor(last_congress) + factor(chamber) + factor(party), 
           data = df2))


df2$chamber_indicator <- (df2$chamber == "senate") * 1

sum(df2$lobbyist) / length(df2$lobbyist)

