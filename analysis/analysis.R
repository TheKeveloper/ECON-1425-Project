library(plm)
df <- read.csv("data/all_congresses.csv"); df$after_reform <- (df$congress > 103) * 1
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

df2 <- read.csv("data/former_legs.csv")

summary(lm(lobbyist ~ cur_relations_score + factor(last_congress), data = df2))

summary(lm(lobbyist ~ remaining_friends + experience + factor(last_congress) + factor(chamber) + factor(party), data = df2))

summary(lm(lobbyist ~ factor(party) + factor(last_congress) + factor(chamber), data = df2))

df2$chamber_indicator <- (df2$chamber == "senate") * 1

sum(df2$lobbyist) / length(df2$lobbyist)

bills_df <- read.csv("data/bill_infos.csv")

recent_bills_df <- bills_df[bills_df$congress >= 103, ]

bills_df$after_reform = bills_df$congress

reg_result(recent_bills_df, "same_party_cosponsors_prop", "cosponsor_leadership", 
           controls = c("max_cosponsor_experience", "enacted", fields$chamber_factor, fields$congress_factor, fields$sponsor_party_factor), fe = F, full = T)

           
reg_result(recent_bills_df, fields$enacted, "total_cosponsors", controls = c(fields$chamber_factor, fields$congress_factor)
              , fe = F, full = T)

reg_result(recent_bills_df, "total_cosponsors", "max_cosponsor_committee_rank_recips",  
           controls = c(fields$chamber_factor, fields$congress_factor, "cosponsor_leadership", "max_cosponsor_experience"),
           fe = F, full = T)

reg_result(recent_bills_df, fields$nominate_variance, fields$leadership, 
           c(fields$chamber_factor, fields$congress_factor, fields$experience, "same_party_cosponsors_prop", 
             "max_cosponsor_experience", "total_cosponsors"),
           fe = F, full = T)

reg_result(recent_bills_df, "same_party_cosponsors_prop", fields$leadership,
           c(fields$chamber_factor, fields$congress_factor, fields$experience,
             "max_cosponsor_experience", "total_cosponsors"),
           fe = F, full = T)
sd(recent_bills_df$total_cosponsors)
recent_senate_bills_df = recent_bills_df[recent_bills_df$chamber == "senate", ]
recent_house_bills_df = recent_bills_df[recent_bills_df$chamber == "house", ]

outliers <- function(data, threshold = 1.5) {
  lowerq = quantile(data)[2]
  upperq = quantile(data)[4]
  iqr = IQR(data)
  return(data[data > upperq + threshold * iqr || data < lowerq - threshold * iqr])
}
