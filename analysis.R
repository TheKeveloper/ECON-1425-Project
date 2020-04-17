library(plm)
df <- read.csv("data/all_congresses.csv"); df$after_reform <- (df$congress > 103) * 1

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
                       fe_index = c("leg_id", "congress"), full = F) {
  variables <- c(independent, controls)
  reg_formula <- as.formula(paste(c(outcome, 
          paste(
            variables,
            collapse = " + "
          )),
    collapse = " ~ "))
  if(fe) {
    s <- summary(plm(reg_formula,
                     index = fe_index,
                     effect = "twoway",
                     model = "within",
                     data = data))
  }
  else {
    s <- summary(lm(reg_formula, data = data))
  }
  if(full) {
    return(s$coefficients)
  }
  else {
    if (fe) {
      return(s$coefficients[1, ])
    }
    else {
      return(s$coefficients[2, ])
    }
  }
}

all_leg_regs <- function(independent, controls, fe = T, fe_index = c("leg_id", "congress"), full = F){
  return(list(
      bills_cosponsored = reg_result(df, fields$bills_cosponsored, independent, controls = controls, 
                                     fe = fe, fe_index = fe_index, full = full),
      bills_sponsored = reg_result(df, fields$bills_sponsored, independent, controls = controls, 
                                     fe = fe, fe_index = fe_index, full = full),
      cosponsors_per_bill = reg_result(df, fields$cosponsors_per_bill, independent, controls = controls, 
                                   fe = fe, fe_index = fe_index, full = full),
      cosponsored_sponsored_ratio = reg_result(df, fields$cosponsored_sponsored_ratio, independent, controls = controls, 
                                       fe = fe, fe_index = fe_index, full = full)
  ))
}

# FE regressions on experience
experience_regs <- all_leg_regs(fields$experience, controls = c(fields$chamber_factor))

# FE regressions on committee count, no experience control
committee_count_no_exp_regs <- all_leg_regs(fields$committee_count, controls = c(fields$chamber_factor))

# FE regressions on committee count, experience control
committee_count_regs <- all_leg_regs(fields$committee_count, controls = c(fields$experience, fields$chamber_factor))

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

all_leg_regs(cross(fields$max_coeff, fields$after_reform), 
             controls = c(fields$committe_max_coeff, fields$after_reform, fields$experience, fields$chamber_factor), 
             fe = T, full = T)

lm(cosponsors_per_bill ~ max_coeff + after_reform + factor(chamber) + committee_max_coeff * after_reform, data = df)

df2 <- read.csv("data/former_legs.csv")

summary(lm(lobbyist ~ cur_relations_score + factor(last_congress), data = df2))


df2$chamber_indicator <- (df2$chamber == "senate") * 1

sum(df2$lobbyist) / length(df2$lobbyist)

bills_df <- read.csv("data/bill_infos.csv")

bills_df$after_reform = bills_df$congress

reg_result(bills_df, "same_party_cosponsors_prop", "cosponsor_leadership", 
           controls = c("max_cosponsor_experience", "enacted", fields$chamber_factor, fields$congress_factor, fields$sponsor_party_factor), fe = F, full = T)

           
           