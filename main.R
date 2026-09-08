# Expected OBP Plus SLG (xOPS)

# =========================================
# =============== LIBRARIES ===============
# =========================================
library(hash)
library(tidyverse)

# ====================================================
# =============== HELPERS & CONSONANTS ===============
# ====================================================

# Consonants
PA_RESULT <- c("out", "walk", "sac", "single", "double", "triple", "homerun")
CONST_VALUES <- c(0, seq(1, 10, by = 1))
FILE_NAME <- "data/2025_results.csv"
VALID_SAMPLE_SIZE <- 50
PLAYER_ID <- "650402"

# Categorizes the MLB's batting results to PA_RESULTS
# arguments: string -> string
parse_pa <- function(pa) {
  if (pa == "single") {
    return("single")
  } else if (pa == "Double") {
    return("double")
  } else if (pa == "Triple") {
    return("triple")
  } else if (pa == "Home Run") {
    return("homerun")
  } else if (pa == "Sac Bunt" | pa == "Sac Fly") {
    return("sac")
  } else if (pa == "Walk" | pa == "Hit By Pitch" | pa == "Intent Walk") {
    return("walk")
  } else {
    return("out")
  }
}

# Given the player's PAs data, returns the xOPS value
# arguments: pa[], double -> double
get_xops <- function(pas, const) {
  if (const == 0) {
    return(get_ops(pas))
  }
  
  # reverse the pas (from early to old)
  pas <- rev(pas)
  
  coe <- 1 / const # coefficient
  num <- 0.0       # numerator
  den <- 0.0       # denominator
  
  # calculate obp
  for (pa in pas) {
    
    # check if pa is 1 of the 7 PA_RESULT elements
    if (!is.element(pa, PA_RESULT)) {
      stop("unidentified PA results type")
    }
    
    den <- den + coe
    
    if (pa != "out" & pa != "sac") {
      num <- num + coe
    }
    coe <- coe * ((const - 1.0) / const)
  }
  
  obp <- num / den
  
  # reset variables
  coe <- 1 / const # coefficient
  num <- 0.0       # numerator
  den <- 0.0       # denominator
  
  # calculate slg
  for (pa in pas) {
    # check if pa is 1 of the 7 PA_RESULT elements
    if (!is.element(pa, PA_RESULT)) {
      stop("unidentified PA results type")
    }
    
    if (pa == "sac" | pa == "walk") {
      next
    }
    
    den <- den + coe
    
    if (pa == "single") {
      num <- num + coe
    } else if (pa == "double") {
      num <- num + 2 * coe
    } else if (pa == "triple") {
      num <- num + 3 * coe
    } else if (pa == "homerun") {
      num <- num + 4 * coe
    }
    coe <- coe * ((const - 1.0) / const)
  }
  
  slg <- num / den
  
  ops <- obp + slg
  
  return(round(ops, 3))
}

# Given a list of PAs data, returns the OPS value
# arguments: pa[], double -> double
get_ops <- function(pas) {
  
  num <- 0.0       # numerator
  den <- 0.0       # denominator
  
  # calculate obp
  for (pa in pas) {
    # check if pa is 1 of the 7 PA_RESULT elements
    if (!is.element(pa, PA_RESULT)) {
      print(pa)
      stop("unidentified PA results type: ", pa)
    }
    
    den <- den + 1
    
    if (pa != "out" & pa != "sac") {
      num <- num + 1
    }
  }
  
  obp = num / den
  
  # reset variables
  num <- 0.0       # numerator
  den <- 0.0       # denominator
  
  # calculate slg
  for (pa in pas) {
    # check if pa is 1 of the 7 PA_RESULT elements
    if (!is.element(pa, PA_RESULT)) {
      stop("unidentified PA results type")
    }
    
    if (pa == "sac" | pa == "walk") {
      next
    }
    
    den <- den + 1
    
    if (pa == "single") {
      num <- num + 1
    } else if (pa == "double") {
      num <- num + 2
    } else if (pa == "triple") {
      num <- num + 3
    } else if (pa == "homerun") {
      num <- num + 4
    }
  }
  
  slg <- num / den
  
  ops = obp + slg
  
  return(round(ops, 3))
}

# ============================================
# =============== LOADING DATA ===============
# ============================================

# Read in the approx. 150,000 PAs from the csv data and sort them by
# batterID -> dateTime -> appearance
data <- read.csv(FILE_NAME, header = TRUE, sep = ",") |>
  arrange(batterID, dateTime, appearance) |>
  filter(batterID == PLAYER_ID)

pas <- split(data$result, data$batterID)
print(paste("Read in", length(unlist(pas)), "PAs data of", length(pas), "players."))

# ===============================================
# =============== PROCESSING DATA ===============
# ===============================================

# Use a dictionary to store each player's all PA results
# keys: const (const = 0 for traditional OPS), values: xops_and_result
xops_and_result_dict <- hash()

# keys: const (const=0 for traditional OPS), values: diff (times of standard deviation)
const_and_diff <- list() # [[const, diff], [const, diff], [const, diff], ...]

### Get xOPS with different const
for (const in CONST_VALUES) {
  if (const == 0) {
    print("Processing const = 0 (traditional OPS)")
  } else {
    print(paste("Processing const =", const, "..."))
  }
  
  # Store the present xOPS and batting result
  xops_and_result <- list()   # [[xops, result], [xops, result], ...]
  
  total_len <- length(pas)
  
  # Use a dictionary to store each player's all PA results
  for (id in names(pas)) {
    # All PA results of this player
    player_pas <- pas[[id]]
    
    # Number of this player's PA
    n <- length(player_pas)
    
    # go through each PA of this player
    for (i in 1:n) {
      
      # result of the current PA
      player_pas[i] <- parse_pa(player_pas[i])
      
      if (i <= VALID_SAMPLE_SIZE) {
        xops <- NA # ignore sample with too small size (n < VALID_SAMPLE_SIZE)
      } else {
        player_previous_pas <- player_pas[1:(i - 1)]
        xops <- get_xops(player_previous_pas, const)
      }
      
      xops_and_result[[length(xops_and_result) + 1]] <- list(xops = xops, result = player_pas[i])
    }
    
    # sort xops_and_result by xops
    xops_values <- sapply(xops_and_result, function(x) x$xops)
    xops_order <- order(xops_values, na.last = FALSE)
    xops_and_result <- xops_and_result[xops_order]
    
    cnt <- cnt + 1
  }
  
  print("All xOPS calculated.")
  
  # calculate average_xops, sd_xops, and actual_ops
  xops <- c()
  actual_result <- list()
  
  cnt <- 0
  total_len <- length(xops_and_result)
  # Calculating differences of xOPS and actual OPS
  for (xar in xops_and_result) {
    cnt <- cnt + 1
    
    if (is.na(xar$xops)) {
      next
    }
    xops <- c(xops, xar$xops)
    actual_result[[length(actual_result) + 1]] <- xar$result
  }
  
  average_xops <- mean(xops)
  sd_xops <- sd(xops)
  actual_ops <- get_ops(actual_result)
  
  # calculate diff = (actual_ops - average_xops) / sd_xops
  diff <- (actual_ops - average_xops) / sd_xops
  
  const_and_diff[[length(const_and_diff) + 1]] <- list(const = const, diff = diff)
  
  print("Mean, SD, and actual OPS calculated.")
  
  if (const == 0) {
    print("const = 0 (traditional OPS) processed.")
  } else {
    print(paste("const =", const, " processed."))
  }
}

# =============================================
# =============== VISUALIZATION ===============
# =============================================

df <- data.frame(
  const = unlist(lapply(const_and_diff, function(x) x$const)),
  diff  = unlist(lapply(const_and_diff, function(x) x$diff))
)

max_y_val <- max(abs(df$diff), na.rm = TRUE)
x_min <- floor(min(df$const, na.rm = TRUE))
x_max <- ceiling(max(df$const, na.rm = TRUE))

ggplot(df, aes(x = const, y = diff)) +
  xlab("Const") +
  ylab("Difference (standard deviation)") +
  geom_point() +
  scale_x_continuous(
    breaks = seq(x_min, x_max, by = 1),
    limits = c(x_min, x_max)
  ) +
  scale_y_continuous(
    breaks = seq(-y_limit, y_limit, by = 0.005),
    limits = c(-0.1, 0.1)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")

# =======================================
# =============== TESTING ===============
# =======================================

# TEST SUITES
test_suite_all_outs <- c("out", "out", "out", "out", "out")
test_suite_homerun <- c("homerun")

# Tsung-Che Cheng June 2026, OPS: 0.376
test_suite_1 <- c("out", "double", "walk", "out",
                  "out", "out", "out",
                  "out", "out", "out", "sac",
                  "single", "out", "out",
                  "out", "out", "out")

get_actual_ops(test_suite_all_outs) # 0.000
get_actual_ops(test_suite_homerun)  # 5.000
get_actual_ops(test_suite_1)        # 0.376

# test_suite2 < test_suite_3
get_xops(test_suite_2, 2)
get_xops(test_suite_3, 2)

# test_suite2 < test_suite_3
get_xops(test_suite_2, 3)
get_xops(test_suite_3, 3)