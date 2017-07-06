#Generate fake output for the Microstrategy implementation of the algorithm.

#This code generates three datasets:
#1. Demand: state * scenario - Expected demand in each state.  Uses
# full state names.
#2. Allocation: scenario * FC - All possible allocations and their attendant
# values
#3. Location: FC - latitude and longitude.

library(ProjectTemplate)
load.project()

set.seed(1977)

#Set a time range
n_time_periods <- 100

#Create all the households------------------
counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
counties_sp <- map2SpatialPolygons(counties, IDs=counties$names,
                                 proj4string=CRS("+proj=longlat +datum=WGS84"))

#Loop through counties, generating observations proportional to actual
# population
pop_data <- read.csv("data/census/2016/population-by-county.csv")
pop_data <- pop_data[pop_data$SUMLEV == 50, ] #Remove non-county rows

household_locations <- NULL
options(warn = -1)
for (i in 1:nrow(county.fips)) {
  if (i %% 10 == 0)
    cat(i, "of", nrow(county.fips), "\n")
  
  n_points <- ceiling(.001 * pop_data$CENSUS2010POP[(1000 * pop_data$STATE +
                                                       pop_data$COUNTY) == 
                                                      county.fips$fips[i]])
  
  if (length(n_points) > 0) {
    this_county_polygon <- counties_sp[i]
    candidate_points <- data.frame(x = runif(10 * n_points, this_county_polygon@bbox[1, 1],
                                             this_county_polygon@bbox[1, 2]),
                                   y = runif(10 * n_points, this_county_polygon@bbox[2, 1],
                                             this_county_polygon@bbox[2, 2]))
    # Convert pointsDF to a SpatialPoints object 
    pointsSP <- SpatialPoints(candidate_points, 
                              proj4string=CRS("+proj=longlat +datum=WGS84"))
    
    # Use 'over' to get _indices_ of the Polygons object containing each point 
    indices <- over(pointsSP, this_county_polygon)
    final_locations <- data.frame(fips = county.fips$fips[i],
                                  (candidate_points[!is.na(indices), ])[1:n_points, ])
    household_locations <- rbind(household_locations, final_locations)  
  }
}

map("county")
points(household_locations$x, household_locations$y, pch = ".", col = 2)

#Break the households into regions
household_locations$state_fips <- floor(household_locations$fips / 1000)
household_locations$state <- pop_data$STNAME[match(household_locations$state_fips,
                                                   pop_data$STATE)]

household_locations$region <- NA
household_locations$region[household_locations$state %in% c("Florida", "South Carolina",
                                                            "Georgia", "Alabama", "Tennessee",
                                                            "Mississippi", "North Carolina")] <- 1
household_locations$region[household_locations$state %in% c("Louisiana", "Arkansas",
                                                            "Texas", "Oklahoma")] <- 2
household_locations$region[household_locations$state %in% c("New Mexico", "Arizona",
                                                            "Colorado", "Utah",
                                                            "Nevada")] <- 3
household_locations$region[household_locations$state %in% c("California", "Oregon",
                                                            "Washington")] <- 4
household_locations$region[household_locations$state %in% c("Idaho", "Montana",
                                                            "Wyoming", "North Dakota",
                                                            "South Dakota", "Minnesota")] <- 5
household_locations$region[household_locations$state %in% c("Wisconsin", "Illinois",
                                                            "Iowa", "Nebraska",
                                                            "Kansas", "Missouri")] <- 6
household_locations$region[household_locations$state %in% c("Michigan", "Ohio",
                                                            "Indiana", "Kentucky",
                                                            "West Virginia", "Virginia",
                                                            "Pennsylvania",
                                                            "Maryland", "Delaware",
                                                            "District of Columbia")] <- 7
household_locations$region[is.na(household_locations$region)] <- 8

n_regions <- 8

#Create the fulfillment centers
n_fulfill_centers <- 5

fulfill_centers_locations <- data.frame(
  x = c(-117.87, -82.90, -104.97, -73.92, -81.1),
  y = c(33.85, 39.86, 39.87, 42.81, 34.08),
  fc = 1:n_fulfill_centers)

#Calculate distances among fulfillment centers
transfer_distances <- matrix(NA, nrow(fulfill_centers_locations),
                             nrow(fulfill_centers_locations))
for (i in 1:nrow(fulfill_centers_locations)) {
  for (j in 1:nrow(fulfill_centers_locations)) {
    transfer_distances[i, j] <- haversine_distance(fulfill_centers_locations$y[i], 
                                                   fulfill_centers_locations$x[i], 
                                                   fulfill_centers_locations$y[j], 
                                                   fulfill_centers_locations$x[j])
  }
}

ggplot(household_locations[sample(nrow(household_locations), 10000), ]) +
  aes(x = x, y = y, color = as.factor(region)) +
  geom_point(alpha = 0.5) +
  geom_text(data = fulfill_centers_locations,
            aes(x = x, y = y,label = fc),
            color = "#000000", size = 5) +
  scale_x_continuous("") +
  scale_y_continuous("") +
  scale_color_discrete("Region") +
  theme_minimal() +
  xlab("") +
  ylab("")


#Define the posterior distribution of actual orders in different regions.
#Data distribution
#Y_{kt} ~ Poi(\mu_{kt})
#\mu_{kt} = \theta_{k1} * exp(-t / \theta_{k2})

#Posterior distribution of \theta
theta_1_means <- rnorm(n_regions, 10, 2)
theta_1_variances <- rgamma(n_regions, 1, 1)
theta_2_means <- rgamma(n_regions, 3 * n_time_periods / 3, 3)
theta_2_variances <- rgamma(n_regions, 5 * n_time_periods / 6, 5)


data.frame( expand.grid(t=1:n_time_periods,region=factor(1:8)),
           demand_mean=sapply(1:n_regions,
                  function(i){theta_1_means[i]*exp(-(seq(1:n_time_periods)/n_time_periods)*theta_2_means[i])}) %>% as.vector()) %>%
  ggplot(.,aes(x=t,y=demand_mean)) +
  geom_line(aes(colour=region)) +
  theme_minimal() +
  xlab("t") +
  ylab(expression(mu[kt]))

#For a gamma distribution
#mu = alpha / beta
#sigmasq = alpha / beta^2
#--> alpha = mu * beta
#--> sigmasq = mu * beta / beta^2
#--> sigmasq = mu / beta --> beta = mu / sigmasq
#--> alpha = mu^2 / sigmasq

curve_frame <- NULL
x <- 1:n_time_periods
for (i in 1:10) {
  for (j in 1:n_regions) {
    alpha <- theta_1_means[j]^2 / theta_1_variances[j]
    beta <- theta_1_means[j] / theta_1_variances[j]
    theta_1 <- rgamma(1, alpha, beta)
    
    alpha <- theta_2_means[j]^2 / theta_2_variances[j]
    beta <- theta_2_means[j] / theta_2_variances[j]
    theta_2 <- rgamma(1, alpha, beta)
    y <- theta_1 * exp(-x / theta_2)
    curve_frame <- rbind(curve_frame,
                         data.frame(region = rep(j, length(x)),
                                    x = x,
                                    y = cumsum(y)))
  }
}
ggplot(curve_frame) +
  aes(x = x, y = y) +
  geom_line() +
  facet_wrap(~ region) + theme_minimal()

#Get 95% intervals for the posterior on the curves
curve_frame <- NULL
x <- 1:n_time_periods
for (i in 1:100) {
  for (j in 1:n_regions) {
    alpha <- theta_1_means[j]^2 / theta_1_variances[j]
    beta <- theta_1_means[j] / theta_1_variances[j]
    theta_1 <- rgamma(1, alpha, beta)
    alpha <- theta_2_means[j]^2 / theta_2_variances[j]
    beta <- theta_2_means[j] / theta_2_variances[j]
    theta_2 <- rgamma(1, alpha, beta)
    y <- theta_1 * exp(-x / theta_2)
    curve_frame <- rbind(curve_frame,
                         data.frame(region = rep(j, length(x)),
                                    x = x,
                                    y = cumsum(y)))
  }
}
# curve_frame <- ddply(curve_frame, .(region, x),
#                      .fun = function(mdf) {
#                        data.frame(l95 = quantile(mdf$y, .025),
#                                   u95 = quantile(mdf$y, .975))
#                      })

curve_frame <- ddply(curve_frame,
                     .(region, x),
                     transform,
                     l95 = quantile(y, .025),
                     u95 = quantile(y, .975))

ggplot(curve_frame) +
  geom_point(aes(x=x,y=y),
             alpha=0.12,
             size=1.2) +
  geom_line(data=curve_frame[!duplicated(curve_frame[,c("x","region")]),],
            aes(x=x,y=l95),
            color = "#FF0000") +
  geom_line(data=curve_frame[!duplicated(curve_frame[,c("x","region")]),],
            aes(x=x,y=u95),
            color = "#FF0000") +
  facet_wrap(~ region) +
  theme_minimal() +
  scale_x_continuous("Time") +
  scale_y_continuous("Cumulative Demand (units)")

#Generate a large number of samples from the posterior distributions
n_samples <- 10000
posterior_samples <- NULL
for (j in 1:n_regions) {
  alpha_1 <- theta_1_means[j]^2 / theta_1_variances[j]
  beta_1 <- theta_1_means[j] / theta_1_variances[j]
  alpha_2 <- theta_2_means[j]^2 / theta_2_variances[j]
  beta_2 <- theta_2_means[j] / theta_2_variances[j]
  posterior_samples <- rbind(posterior_samples,
                             data.frame(region = rep(j, n_samples),
                                        theta_1 = rgamma(n_samples, alpha_1, beta_1),
                                        theta_2 = rgamma(n_samples, alpha_2, beta_2)))
}

#Determine the buy. Set it at 90% of the predicted sales
#For each of the sampled theta values, we need to generate the distribution of
#actual sales by Monte Carlo.
x <- 1:n_time_periods
n_replications <- 10
sales_matrix <- matrix(NA, nrow(posterior_samples), n_replications)
for (i in 1:nrow(posterior_samples)) {
  if (i %% 10000 == 0)
    cat(i, "of", nrow(posterior_samples), " ")
  for (j in 1:n_replications) {
    mu <- sum(posterior_samples$theta_1[i] * 
                exp(-x / posterior_samples$theta_2[i]))
    sales_matrix[i, j] <- rpois(1, mu)
  }
}

total_sales <- matrix(0, n_samples, n_replications)
for (j in 1:n_regions) {
  total_sales <- total_sales + sales_matrix[posterior_samples$region == j,]
}
total_buy <- quantile(as.numeric(total_sales), .9)

#Set up the rules for fulfillment and transfer between DCs
#For transfer:
# 1. Calculate the difference between expected sales and current stock in each FC
# 2. If one FC is more than x% below expected sales find the FC that is the
#     highest amount above expected sales.  If this highest amount is x% over
#     expected sales, transfer.  Except, no transfers of fewer than y units
#For fulfillment:
# If all FCs have enough to fulfill the closest buys, do that.
# If not, perform an optimization

#Calculate the expected sales in each region
expected_sales <- rep(0, n_regions)
for (j in 1:n_regions) {
  expected_sales[j] <- mean(sales_matrix[posterior_samples$region == j,])
}

#Calculate distances from each FC to each region centroid
fc_to_region_distances <- matrix(0, n_fulfill_centers, n_regions)
for (i in 1:n_fulfill_centers) {
  for (j in 1:n_regions) {
    fc_to_region_distances[i, j] <- haversine_distance(fulfill_centers_locations$y[i],
        fulfill_centers_locations$x[i],
        mean(household_locations$y[household_locations$region == j]),
        mean(household_locations$x[household_locations$region == j]))
  }
}

#Figure out which region is served by each FC
fc_region_assignments <- apply(fc_to_region_distances, 2, which.min)

#Sum up to get expected sales to fulfill from each FC
fc_expected_fulfillment <- ddply(data.frame(fc = fc_region_assignments,
                                            expected_sales = expected_sales),
                                 .(fc), summarize,
                                 expected_sales = sum(expected_sales))

initial_allocation <- round(total_buy * fc_expected_fulfillment$expected_sales /
                              sum(fc_expected_fulfillment$expected_sales))

#Set up cost parameters
alpha_1 <- 5 #fixed cost of delivering a unit
alpha_2 <- 0.005 #variable cost of delivering a unit 1 distance unit ($5 per 1000)
alpha_3 <- 2 #variable cost of delivering a unit 1 time unit
miles_per_day <- 500 #1 time unit increase in delivery for this many miles travelled
AUR <- 29.99 #Item sale price
delta <- 0.00069 #Discount factor per additional unit over demand (markdown rate)
                #0.00069 implies that AUR is cut in half for an over-order of 1000 units

#Simulate the system
options(warn = 2)
n_generations <- 100 #This must be less than or equal to n_samples
value_storage <- data.frame(total_value = rep(NA, n_generations),
                            amt_sold = rep(NA, n_generations),
                            total_buy = rep(NA, n_generations),
                            total_demand = rep(NA, n_generations),
                            total_delivery_cost = rep(NA, n_generations),
                            total_transfer_cost = rep(NA, n_generations),
                            full_price = rep(NA, n_generations),
                            marked_down = rep(NA, n_generations))
for (this_generation in 1:n_generations) {
  print(this_generation)
  
  #Generate the number of buys in each region across all time points
  expected_buys <- matrix(NA, n_regions, n_time_periods)
  true_buys <- matrix(NA, n_regions, n_time_periods)
  for (j in 1:n_regions) {
    this_index <- n_samples * (j - 1) + this_generation
    expected_buys[j, ] <- posterior_samples$theta_1[this_index] * 
      exp(-(1:n_time_periods) / posterior_samples$theta_2[this_index])
    true_buys[j, ] <- rpois(n_time_periods, expected_buys[j, ])
  }
  
  #Go over time applying the fulfillment and transfer rules.  Also, calculate
  #total costs as you go.
  current_inventory <- initial_allocation
  total_fulfilled <- 0
  total_delivery_cost <- 0
  total_transfer_cost <- 0
  for (time_point in 1:n_time_periods) {

    #If we're all out of inventory, we're done
    if (sum(current_inventory) == 0)
      next
    
    #If no one bought anything at this time point, we're done
    if (sum(true_buys[, time_point]) == 0)
      next
    
    #Randomly select the set of houses that placed orders
    true_total_buy <- sum(true_buys[, time_point])
    order_hhs <- NULL
    for (j in 1:n_regions) {
      order_hhs <- c(order_hhs, sample(which(household_locations$region == j),
                                       true_buys[j, time_point], replace = FALSE))
    }
    
    #Calculate distances from each ordering HH to each FC
    deliver_distances <- matrix(NA, n_fulfill_centers, length(order_hhs))
    for (i in 1:n_fulfill_centers) {
      for (j in 1:length(order_hhs)) {
        deliver_distances[i, j] <- haversine_distance(fulfill_centers_locations$y[i],
                                                      fulfill_centers_locations$x[i],
                                                      household_locations$y[order_hhs[j]],
                                                      household_locations$x[order_hhs[j]])
      }
    }

    #Find the closest FC to each HH
    closest_delivery <- apply(deliver_distances, 2, which.min)
    
    #Count up how many need to be sent from each FC
    n_to_fulfill <- as.data.frame(table(closest_delivery))
    n_to_fulfill$closest_delivery <- as.character(n_to_fulfill$closest_delivery)
    n_to_fulfill <- merge(n_to_fulfill, 
                          data.frame(closest_delivery = as.character(1:n_fulfill_centers)),
                          all = TRUE)
    n_to_fulfill$Freq[is.na(n_to_fulfill$Freq)] <- 0
    n_to_fulfill <- n_to_fulfill[order(n_to_fulfill$closest_delivery),]
    
    #Make a table of current delivery strategy.
    current_delivery_strategy <- matrix(0, n_fulfill_centers, true_total_buy)
    current_delivery_strategy[matrix(c(closest_delivery, 1:true_total_buy),
                                     ncol = 2)] <- 1
    
    #Check if they all have enough.
    if (min(current_inventory - n_to_fulfill$Freq) < 0) { #Not enough to fulfill
      
      #Check if we're going to run completely out of inventory.
      if (sum(current_inventory) < sum(n_to_fulfill$Freq)) {  #We're going to run out of inventory at this time point
        
        #Randomly select which ones we'll actually fulfill
        random_delivery <- sample(1:true_total_buy, sum(current_inventory))
        
        #Update the objects
        true_total_buy <- sum(current_inventory)
        order_hhs <- order_hhs[random_delivery]
        deliver_distances <- deliver_distances[, random_delivery]
        deliver_distances <- matrix(as.numeric(deliver_distances), nrow = n_fulfill_centers)
        closest_delivery <- apply(deliver_distances, 2, which.min)
        n_to_fulfill <- as.data.frame(table(closest_delivery))
        n_to_fulfill$closest_delivery <- as.character(n_to_fulfill$closest_delivery)
        n_to_fulfill <- merge(n_to_fulfill, 
                              data.frame(closest_delivery = as.character(1:n_fulfill_centers)),
                              all = TRUE)
        n_to_fulfill$Freq[is.na(n_to_fulfill$Freq)] <- 0
        n_to_fulfill <- n_to_fulfill[order(n_to_fulfill$closest_delivery),]
        
        #Make a table of current delivery strategy.
        current_delivery_strategy <- matrix(0, n_fulfill_centers, true_total_buy)
        current_delivery_strategy[matrix(c(closest_delivery, 1:true_total_buy),
                                         ncol = 2)] <- 1
      }
      
      #Modify this to a viable delivery strategy
      fulfill_total <- apply(current_delivery_strategy, 1, sum)
      while(min(current_inventory - fulfill_total) < 0) {
        worst_fc <- which.min(current_inventory - fulfill_total)[1]
        move_to <- which(current_inventory > fulfill_total)
        to_move <- which.min(deliver_distances[move_to, current_delivery_strategy[worst_fc,] == 1])[1]
        move_to_index <- to_move %% length(move_to)
        if (move_to_index == 0) {
          move_to_index <- length(move_to)
        }
        fc <- move_to[move_to_index]
        hh <- which(current_delivery_strategy[worst_fc,] == 1)[ceiling(to_move / length(move_to))]
        current_delivery_strategy[fc, hh]  <- 1
        current_delivery_strategy[worst_fc, hh] <- 0
        fulfill_total <- apply(current_delivery_strategy, 1, sum)
      }
      
      #Optimize the fulfillment strategy
      #TODO!!!

    }
    
    #Calculate the cost of fulfilling under this strategy
    delivery_cost <- sum(alpha_1 + alpha_2 * 
      as.numeric(deliver_distances)[as.numeric(current_delivery_strategy) == 1] +
        alpha_3 *
        floor(as.numeric(deliver_distances)[as.numeric(current_delivery_strategy) == 1] /
                miles_per_day))
    
    #Update inventory
    fulfill_total <- apply(current_delivery_strategy, 1, sum)
    current_inventory <- current_inventory - fulfill_total
    
    #Transfer component was removed 2016-08-25
    
    total_fulfilled <- total_fulfilled + fulfill_total
    total_delivery_cost <- total_delivery_cost + delivery_cost
#    total_transfer_cost <- total_transfer_cost + transfer_cost
    
    
  }
  
  #Record the total value of the strategy
  amt_sold <- sum(total_fulfilled)
  total_demand <- sum(true_buys)
  total_value <- amt_sold * AUR * (total_buy < total_demand) +
    ((amt_sold * AUR + (total_buy - amt_sold) * AUR * exp(-delta * (total_buy - amt_sold))) * 
       (total_buy >= total_demand)) -
    total_delivery_cost #-
#    total_transfer_cost
  value_storage$total_value[this_generation] <- total_value
  value_storage$amt_sold[this_generation] <- amt_sold
  value_storage$total_buy[this_generation] <- total_buy
  value_storage$total_demand[this_generation] <- total_demand
  value_storage$total_delivery_cost[this_generation] <- total_delivery_cost
#  value_storage$total_transfer_cost[this_generation] <- total_transfer_cost
  value_storage$full_price[this_generation] <- amt_sold * AUR
  value_storage$marked_down[this_generation] <- (total_buy - amt_sold) * AUR * exp(-delta * (total_buy - amt_sold)) * (total_buy >= total_demand)

}

expected_value <- mean(value_storage$total_value)
value_storage

#Implement an optimization on this.
set.seed(1978)
#Set up cost parameters
alpha_1 <- 5 #fixed cost of delivering a unit
alpha_2 <- 0.005 #variable cost of delivering a unit 1 distance unit ($5 per 1000)
alpha_3 <- 2 #variable cost of delivering a unit 1 time unit
AUR <- 29.99 #Item sale price
delta <- 0.00069 #Discount factor per additional unit over demand (markdown rate)
#0.00069 implies that AUR is cut in half for an over-order of 1000 units

fc_expected_fulfillment <- ddply(data.frame(fc = fc_region_assignments,
                                            expected_sales = expected_sales),
                                 .(fc), summarize,
                                 expected_sales = sum(expected_sales))
initial_allocation <- round(total_buy * fc_expected_fulfillment$expected_sales /
                              sum(fc_expected_fulfillment$expected_sales))


#Make a matrix of the allocation schemes to try
allocations <- matrix(initial_allocation, nrow = 1)
allocation_list <- vector("list", n_fulfill_centers - 1)
for (j in 1:(n_fulfill_centers - 1)) {
  allocation_list[[j]] <- round(initial_allocation[j], -2)
  allocation_list[[j]] <- seq(max(allocation_list[[j]] - 200, 100),
                              min(allocation_list[[j]] + 200, 100 * floor(total_buy / 100)),
                              by = 100)
}
# #Override manually since this didn't find the max
# allocation_list[[1]] <- seq(300, 700, by = 100)
# allocation_list[[2]] <- seq(500, 900, by = 100)
# allocation_list[[3]] <- seq(400, 800, by = 100)
# allocation_list[[4]] <- seq(400, 800, by = 100)
other_allocations <- as.matrix(expand.grid(allocation_list))
other_allocations <- cbind(other_allocations,
                           total_buy - apply(other_allocations, 1, sum))
allocations <- rbind(allocations, other_allocations)

#Remove invalid allocations
allocations <- allocations[allocations[, ncol(allocations)] >= 0, ]

options(warn = 2)
output_storage <- NULL
t1 <- Sys.time()
for (this_allocation in 1:nrow(allocations)) {
  print(this_allocation)
  
  initial_allocation <- as.numeric(allocations[this_allocation, ])

  n_generations <- 10 #This must be less than or equal to n_samples
  value_storage <- data.frame(total_value = rep(NA, n_generations),
                              amt_sold = rep(NA, n_generations),
                              total_buy = rep(NA, n_generations),
                              total_demand = rep(NA, n_generations),
                              total_delivery_cost = rep(NA, n_generations),
                              total_transfer_cost = rep(NA, n_generations),
                              full_price = rep(NA, n_generations),
                              marked_down = rep(NA, n_generations))
  
  for (this_generation in 1:n_generations) {

    #Generate the number of buys in each region across all time points
    expected_buys <- matrix(NA, n_regions, n_time_periods)
    true_buys <- matrix(NA, n_regions, n_time_periods)
    for (j in 1:n_regions) {
      this_index <- n_samples * (j - 1) + this_generation
      expected_buys[j, ] <- posterior_samples$theta_1[this_index] * 
        exp(-(1:n_time_periods) / posterior_samples$theta_2[this_index])
      true_buys[j, ] <- rpois(n_time_periods, expected_buys[j, ])
    }
    
    #Go over time applying the fulfillment and transfer rules.  Also, calculate
    #total costs as you go.
    current_inventory <- initial_allocation
    total_fulfilled <- 0
    total_delivery_cost <- 0
    #total_transfer_cost <- 0
    for (time_point in 1:n_time_periods) {
      
      #If we're all out of inventory, we're done
      if (sum(current_inventory) == 0)
        next
      
      #If no one bought anything at this time point, we're done
      if (sum(true_buys[, time_point]) == 0)
        next
      
      #Randomly select the set of houses that placed orders
      true_total_buy <- sum(true_buys[, time_point])
      order_hhs <- NULL
      for (j in 1:n_regions) {
        order_hhs <- c(order_hhs, sample(which(household_locations$region == j),
                                         true_buys[j, time_point], replace = FALSE))
      }
      
      #Calculate distances from each ordering HH to each FC
      deliver_distances <- matrix(NA, n_fulfill_centers, length(order_hhs))
      for (i in 1:n_fulfill_centers) {
        for (j in 1:length(order_hhs)) {
          deliver_distances[i, j] <- haversine_distance(fulfill_centers_locations$y[i],
                                                        fulfill_centers_locations$x[i],
                                                        household_locations$y[order_hhs[j]],
                                                        household_locations$x[order_hhs[j]])
        }
      }
      
      #Find the closest FC to each HH
      closest_delivery <- apply(deliver_distances, 2, which.min)
      
      #Count up how many need to be sent from each FC
      n_to_fulfill <- as.data.frame(table(closest_delivery))
      n_to_fulfill$closest_delivery <- as.character(n_to_fulfill$closest_delivery)
      n_to_fulfill <- merge(n_to_fulfill, 
                            data.frame(closest_delivery = as.character(1:n_fulfill_centers)),
                            all = TRUE)
      n_to_fulfill$Freq[is.na(n_to_fulfill$Freq)] <- 0
      n_to_fulfill <- n_to_fulfill[order(n_to_fulfill$closest_delivery),]
      
      #Make a table of current delivery strategy.
      current_delivery_strategy <- matrix(0, n_fulfill_centers, true_total_buy)
      current_delivery_strategy[matrix(c(closest_delivery, 1:true_total_buy),
                                       ncol = 2)] <- 1
      
      #Check if they all have enough.
      if (min(current_inventory - n_to_fulfill$Freq) < 0) { #Not enough to fulfill
        
        #Check if we're going to run completely out of inventory.
        if (sum(current_inventory) < sum(n_to_fulfill$Freq)) {  #We're going to run out of inventory at this time point
          
          #Randomly select which ones we'll actually fulfill
          random_delivery <- sample(1:true_total_buy, sum(current_inventory))
          
          #Update the objects
          true_total_buy <- sum(current_inventory)
          order_hhs <- order_hhs[random_delivery]
          deliver_distances <- deliver_distances[, random_delivery]
          deliver_distances <- matrix(as.numeric(deliver_distances), nrow = n_fulfill_centers)
          closest_delivery <- apply(deliver_distances, 2, which.min)
          n_to_fulfill <- as.data.frame(table(closest_delivery))
          n_to_fulfill$closest_delivery <- as.character(n_to_fulfill$closest_delivery)
          n_to_fulfill <- merge(n_to_fulfill, 
                                data.frame(closest_delivery = as.character(1:n_fulfill_centers)),
                                all = TRUE)
          n_to_fulfill$Freq[is.na(n_to_fulfill$Freq)] <- 0
          n_to_fulfill <- n_to_fulfill[order(n_to_fulfill$closest_delivery),]
          
          #Make a table of current delivery strategy.
          current_delivery_strategy <- matrix(0, n_fulfill_centers, true_total_buy)
          current_delivery_strategy[matrix(c(closest_delivery, 1:true_total_buy),
                                           ncol = 2)] <- 1
        }
        
        #Modify this to a viable delivery strategy
        fulfill_total <- apply(current_delivery_strategy, 1, sum)
        while(min(current_inventory - fulfill_total) < 0) {
          worst_fc <- which.min(current_inventory - fulfill_total)[1]
          move_to <- which(current_inventory > fulfill_total)
          to_move <- which.min(deliver_distances[move_to, current_delivery_strategy[worst_fc,] == 1])[1]
          move_to_index <- to_move %% length(move_to)
          if (move_to_index == 0) {
            move_to_index <- length(move_to)
          }
          fc <- move_to[move_to_index]
          hh <- which(current_delivery_strategy[worst_fc,] == 1)[ceiling(to_move / length(move_to))]
          current_delivery_strategy[fc, hh]  <- 1
          current_delivery_strategy[worst_fc, hh] <- 0
          fulfill_total <- apply(current_delivery_strategy, 1, sum)
        }
        
        #Optimize the fulfillment strategy
        #TODO!!!
        
      }
      
      #Calculate the cost of fulfilling under this strategy
      delivery_cost <- sum(alpha_1 + alpha_2 * 
                             as.numeric(deliver_distances)[as.numeric(current_delivery_strategy) == 1] +
                             alpha_3 *
                             floor(as.numeric(deliver_distances)[as.numeric(current_delivery_strategy) == 1] /
                                     miles_per_day))
      
      #Update inventory
      fulfill_total <- apply(current_delivery_strategy, 1, sum)
      current_inventory <- current_inventory - fulfill_total
      
      #Transfer component was removed 2016-08-25
      
      total_fulfilled <- total_fulfilled + fulfill_total
      total_delivery_cost <- total_delivery_cost + delivery_cost
      #total_transfer_cost <- total_transfer_cost + transfer_cost
      
      
    }
    
    #Record the total value of the strategy
    amt_sold <- sum(total_fulfilled)
    total_demand <- sum(true_buys)
    total_value <- amt_sold * AUR * (total_buy < total_demand) +
      ((amt_sold * AUR + (total_buy - amt_sold) * AUR * exp(-delta * (total_buy - amt_sold))) * 
         (total_buy >= total_demand)) -
      total_delivery_cost #-
  #    total_transfer_cost
    value_storage$total_value[this_generation] <- total_value
    value_storage$amt_sold[this_generation] <- amt_sold
    value_storage$total_buy[this_generation] <- total_buy
    value_storage$total_demand[this_generation] <- total_demand
    value_storage$total_delivery_cost[this_generation] <- total_delivery_cost
    value_storage$total_transfer_cost[this_generation] <- total_transfer_cost
    value_storage$full_price[this_generation] <- amt_sold * AUR
    value_storage$marked_down[this_generation] <- (total_buy - amt_sold) * AUR * exp(-delta * (total_buy - amt_sold)) * (total_buy >= total_demand)
    
  }
  
  value_storage$allocation_number <- this_allocation
  output_storage <- rbind(output_storage, value_storage)
  t2 <- Sys.time()
  print(t2 - t1)
}


save.image(file.path("data",
                     paste0("microstrategy-run-",
                     gsub(" ","-",gsub(":","-",Sys.time())),".RData")))
load(file.path("data", "microstrategy-run-2017-06-20-16-51-23.RData"))

allocation_frame <- as.data.frame(allocations)
allocation_frame$allocation_number <- 1:nrow(allocation_frame)
output_storage <- merge(output_storage, allocation_frame)

fit <- lm(total_value ~ Var1 + Var2 + Var3 + Var4
          + Var1 * Var2 + Var1 * Var3 + Var1 * Var4
          + Var2 * Var3 + Var2 * Var4 + Var3 * Var4
          + I(Var1^2) + I(Var2^2) + I(Var3^2) + I(Var4^2),
          data = output_storage)

pred_frame <- expand.grid(Var1 = seq(min(output_storage$Var1),
                                     max(output_storage$Var1),
                                     by = 50),
                          Var2 = seq(min(output_storage$Var2),
                                     max(output_storage$Var2),
                                     by = 50),
                          Var3 = seq(min(output_storage$Var3),
                                     max(output_storage$Var3),
                                     by = 50),
                          Var4 = seq(min(output_storage$Var4),
                                     max(output_storage$Var4),
                                     by = 50))
pred_frame$V5 <- total_buy - apply(pred_frame[, c("Var1", "Var2", "Var3", "Var4")],
                                   1, sum)
pred_frame <- pred_frame[pred_frame$V5 >= 0,]
pred_frame$predicted <- predict(fit, newdata = pred_frame)
pred_frame[which.max(pred_frame$predicted),]

#1. Demand: state * scenario - Expected demand in each state.  Uses
# full state names.

#Create a demand table at the state level and call this the normal demand
state_to_region <- unique(household_locations[, c("state_fips", "region")])
pop_data <- merge(pop_data, plyr::rename(state_to_region, c("state_fips" = "STATE")),
              all.x = TRUE)
region_pop_totals <- ddply(pop_data, .(region), summarize,
                           CENSUS2010POP_total = sum(CENSUS2010POP))
pop_data <- merge(pop_data, region_pop_totals)
pop_data$region_fraction <- pop_data$CENSUS2010POP / pop_data$CENSUS2010POP_total
demand <- merge(pop_data, data.frame(region = 1:8, expected_sales = expected_sales))
demand$expected_sales <- demand$expected_sales * demand$region_fraction
demand <- ddply(demand, .(STATE, STNAME), summarize,
                expected_sales = sum(expected_sales))
demand <- plyr::rename(demand, c("STATE" = "state_fips", "STNAME" = "state_name"))
demand$scenario_nbr <- 1
demand$scenario_name <- "Normal"
orig_demand <- demand

#Make a "high sales" scenario
new_demand <- orig_demand
new_demand$expected_sales <- 1.1 * new_demand$expected_sales
new_demand$scenario_nbr <- 2
new_demand$scenario_name <- "Sales Increase 10%"
demand <- rbind(demand, new_demand)

#Make a "low sales" scenario
new_demand <- orig_demand
new_demand$expected_sales <- 0.9 * new_demand$expected_sales
new_demand$scenario_nbr <- 3
new_demand$scenario_name <- "Sales Decrease 10%"
demand <- rbind(demand, new_demand)

#Make a "Florida and SC disappear" scenario
new_demand <- orig_demand
new_demand$expected_sales[new_demand$state_name %in% c("Florida", "South Carolina")] <- 0
new_demand$scenario_nbr <- 4
new_demand$scenario_name <- "FL and SC hit by hurricane"
demand <- rbind(demand, new_demand)

#Make a "Trump Tweets" scenario
new_demand <- orig_demand
red_states <- c("Idaho", "Montana", "Wyoming", "Utah", "Arizona", "Texas", "Oklahoma",
                "Kansas", "Nebraska", "South Dakota", "North Dakota", "Iowa", "Missouri",
                "Arkansas", "Louisiana", "Mississippi", "Alabama", "Georgia", "Florida",
                "South Carolina", "North Carolina", "Tennessee", "Kentucky", "Wisconsin",
                "Michigan", "Indiana", "Ohio", "West Virginia", "Pennsylvania")
new_demand$expected_sales[new_demand$state_name %in% red_states] <- 1.1 * new_demand$expected_sales[new_demand$state_name %in% red_states]
new_demand$expected_sales[new_demand$state_name %in% red_states] <- 0.9 * new_demand$expected_sales[new_demand$state_name %in% red_states]
new_demand$scenario_nbr <- 5
new_demand$scenario_name <- "Trump tweet"
demand <- rbind(demand, new_demand)

write.csv(demand[demand$scenario_nbr %in% c(1, 4, 5), ], 
          "data/demand.csv", row.names = FALSE)

#2. Allocation: scenario * FC - All possible allocations and their attendant
# values
allocation <- plyr::rename(pred_frame, c("Var1" = "fulfillment_center_1",
                                         "Var2" = "fulfillment_center_2",
                                         "Var3" = "fulfillment_center_3",
                                         "Var4" = "fulfillment_center_4",
                                         "V5" = "fulfillment_center_5",
                                         "predicted" = "revenue_minus_delivery_cost"))
allocation$scenario_nbr <- 1
allocation$scenario_name <- "Normal"
orig_allocation <- allocation

#Make a "high sales" scenario
set.seed(1978)
sd_dampening <- 50
new_allocation <- orig_allocation
new_allocation$revenue_minus_delivery_cost <- (
  2.612e+04 +
    rnorm(1, 4.336e+01, 4.336e+01 / sd_dampening) * new_allocation$fulfillment_center_1 + 
    rnorm(1, 1.199e+01, 1.199e+01 / sd_dampening) * new_allocation$fulfillment_center_2 + 
    rnorm(1, 3.537e+01, 3.537e+01 / sd_dampening) * new_allocation$fulfillment_center_3 + 
    rnorm(1, 1.502e+01, 1.502e+01 / sd_dampening) * new_allocation$fulfillment_center_4 + 
    rnorm(1, -4.419e-03, 4.419e-03 / sd_dampening) * new_allocation$fulfillment_center_1 * new_allocation$fulfillment_center_2 + 
    rnorm(1, -2.825e-02, 2.825e-02 / sd_dampening) * new_allocation$fulfillment_center_1 * new_allocation$fulfillment_center_3 + 
    rnorm(1, -4.181e-03, 4.181e-03 / sd_dampening) * new_allocation$fulfillment_center_1 * new_allocation$fulfillment_center_4 + 
    rnorm(1, -4.473e-03, 4.473e-03 / sd_dampening) * new_allocation$fulfillment_center_2 * new_allocation$fulfillment_center_3 + 
    rnorm(1, -6.499e-03, 6.499e-03 / sd_dampening) * new_allocation$fulfillment_center_2 * new_allocation$fulfillment_center_4 + 
    rnorm(1, -3.662e-03, 3.662e-03 / sd_dampening) * new_allocation$fulfillment_center_3 * new_allocation$fulfillment_center_4 + 
    rnorm(1, -2.004e-02, 2.004e-02 / sd_dampening) * new_allocation$fulfillment_center_1^2 + 
    rnorm(1, -3.976e-03, 3.976e-03 / sd_dampening) * new_allocation$fulfillment_center_2^2 + 
    rnorm(1, -1.335e-02, 1.335e-02 / sd_dampening) * new_allocation$fulfillment_center_3^2 +
    rnorm(1, -1.054e-02, 1.054e-02 / sd_dampening) * new_allocation$fulfillment_center_4^2)
#new_allocation[which.max(new_allocation$revenue_minus_delivery_cost),]
new_allocation$scenario_nbr <- 2
new_allocation$scenario_name <- "Sales Increase 10%"
allocation <- rbind(allocation, new_allocation)

#Make a "low sales" scenario
new_allocation <- orig_allocation
new_allocation$revenue_minus_delivery_cost <- (
  2.612e+04 +
    rnorm(1, 4.336e+01, 4.336e+01 / sd_dampening) * new_allocation$fulfillment_center_1 + 
    rnorm(1, 1.199e+01, 1.199e+01 / sd_dampening) * new_allocation$fulfillment_center_2 + 
    rnorm(1, 3.537e+01, 3.537e+01 / sd_dampening) * new_allocation$fulfillment_center_3 + 
    rnorm(1, 1.502e+01, 1.502e+01 / sd_dampening) * new_allocation$fulfillment_center_4 + 
    rnorm(1, -4.419e-03, 4.419e-03 / sd_dampening) * new_allocation$fulfillment_center_1 * new_allocation$fulfillment_center_2 + 
    rnorm(1, -2.825e-02, 2.825e-02 / sd_dampening) * new_allocation$fulfillment_center_1 * new_allocation$fulfillment_center_3 + 
    rnorm(1, -4.181e-03, 4.181e-03 / sd_dampening) * new_allocation$fulfillment_center_1 * new_allocation$fulfillment_center_4 + 
    rnorm(1, -4.473e-03, 4.473e-03 / sd_dampening) * new_allocation$fulfillment_center_2 * new_allocation$fulfillment_center_3 + 
    rnorm(1, -6.499e-03, 6.499e-03 / sd_dampening) * new_allocation$fulfillment_center_2 * new_allocation$fulfillment_center_4 + 
    rnorm(1, -3.662e-03, 3.662e-03 / sd_dampening) * new_allocation$fulfillment_center_3 * new_allocation$fulfillment_center_4 + 
    rnorm(1, -2.004e-02, 2.004e-02 / sd_dampening) * new_allocation$fulfillment_center_1^2 + 
    rnorm(1, -3.976e-03, 3.976e-03 / sd_dampening) * new_allocation$fulfillment_center_2^2 + 
    rnorm(1, -1.335e-02, 1.335e-02 / sd_dampening) * new_allocation$fulfillment_center_3^2 +
    rnorm(1, -1.054e-02, 1.054e-02 / sd_dampening) * new_allocation$fulfillment_center_4^2)
new_allocation$scenario_nbr <- 3
new_allocation$scenario_name <- "Sales Decrease 10%"
allocation <- rbind(allocation, new_allocation)

#Make a "Florida and SC disappear" scenario
new_allocation <- orig_allocation
new_allocation$revenue_minus_delivery_cost <- (
  2.612e+04 +
    rnorm(1, 4.336e+01, 4.336e+01 / sd_dampening) * new_allocation$fulfillment_center_1 + 
    rnorm(1, 1.199e+01, 1.199e+01 / sd_dampening) * new_allocation$fulfillment_center_2 + 
    rnorm(1, 3.537e+01, 3.537e+01 / sd_dampening) * new_allocation$fulfillment_center_3 + 
    rnorm(1, 1.502e+01, 1.502e+01 / sd_dampening) * new_allocation$fulfillment_center_4 + 
    rnorm(1, -4.419e-03, 4.419e-03 / sd_dampening) * new_allocation$fulfillment_center_1 * new_allocation$fulfillment_center_2 + 
    rnorm(1, -2.825e-02, 2.825e-02 / sd_dampening) * new_allocation$fulfillment_center_1 * new_allocation$fulfillment_center_3 + 
    rnorm(1, -4.181e-03, 4.181e-03 / sd_dampening) * new_allocation$fulfillment_center_1 * new_allocation$fulfillment_center_4 + 
    rnorm(1, -4.473e-03, 4.473e-03 / sd_dampening) * new_allocation$fulfillment_center_2 * new_allocation$fulfillment_center_3 + 
    rnorm(1, -6.499e-03, 6.499e-03 / sd_dampening) * new_allocation$fulfillment_center_2 * new_allocation$fulfillment_center_4 + 
    rnorm(1, -3.662e-03, 3.662e-03 / sd_dampening) * new_allocation$fulfillment_center_3 * new_allocation$fulfillment_center_4 + 
    rnorm(1, -2.004e-02, 2.004e-02 / sd_dampening) * new_allocation$fulfillment_center_1^2 + 
    rnorm(1, -3.976e-03, 3.976e-03 / sd_dampening) * new_allocation$fulfillment_center_2^2 + 
    rnorm(1, -1.335e-02, 1.335e-02 / sd_dampening) * new_allocation$fulfillment_center_3^2 +
    rnorm(1, -1.054e-02, 1.054e-02 / sd_dampening) * new_allocation$fulfillment_center_4^2)
new_allocation$scenario_nbr <- 4
new_allocation$scenario_name <- "FL and SC hit by hurricane"
allocation <- rbind(allocation, new_allocation)

#Make a "Trump Tweets" scenario
new_allocation <- orig_allocation
red_states <- c("Idaho", "Montana", "Wyoming", "Utah", "Arizona", "Texas", "Oklahoma",
                "Kansas", "Nebraska", "South Dakota", "North Dakota", "Iowa", "Missouri",
                "Arkansas", "Louisiana", "Mississippi", "Alabama", "Georgia", "Florida",
                "South Carolina", "North Carolina", "Tennessee", "Kentucky", "Wisconsin",
                "Michigan", "Indiana", "Ohio", "West Virginia", "Pennsylvania")
new_allocation$revenue_minus_delivery_cost <- (
  2.612e+04 +
    rnorm(1, 4.336e+01, 4.336e+01 / sd_dampening) * new_allocation$fulfillment_center_1 + 
    rnorm(1, 1.199e+01, 1.199e+01 / sd_dampening) * new_allocation$fulfillment_center_2 + 
    rnorm(1, 3.537e+01, 3.537e+01 / sd_dampening) * new_allocation$fulfillment_center_3 + 
    rnorm(1, 1.502e+01, 1.502e+01 / sd_dampening) * new_allocation$fulfillment_center_4 + 
    rnorm(1, -4.419e-03, 4.419e-03 / sd_dampening) * new_allocation$fulfillment_center_1 * new_allocation$fulfillment_center_2 + 
    rnorm(1, -2.825e-02, 2.825e-02 / sd_dampening) * new_allocation$fulfillment_center_1 * new_allocation$fulfillment_center_3 + 
    rnorm(1, -4.181e-03, 4.181e-03 / sd_dampening) * new_allocation$fulfillment_center_1 * new_allocation$fulfillment_center_4 + 
    rnorm(1, -4.473e-03, 4.473e-03 / sd_dampening) * new_allocation$fulfillment_center_2 * new_allocation$fulfillment_center_3 + 
    rnorm(1, -6.499e-03, 6.499e-03 / sd_dampening) * new_allocation$fulfillment_center_2 * new_allocation$fulfillment_center_4 + 
    rnorm(1, -3.662e-03, 3.662e-03 / sd_dampening) * new_allocation$fulfillment_center_3 * new_allocation$fulfillment_center_4 + 
    rnorm(1, -2.004e-02, 2.004e-02 / sd_dampening) * new_allocation$fulfillment_center_1^2 + 
    rnorm(1, -3.976e-03, 3.976e-03 / sd_dampening) * new_allocation$fulfillment_center_2^2 + 
    rnorm(1, -1.335e-02, 1.335e-02 / sd_dampening) * new_allocation$fulfillment_center_3^2 +
    rnorm(1, -1.054e-02, 1.054e-02 / sd_dampening) * new_allocation$fulfillment_center_4^2)
new_allocation$scenario_nbr <- 5
new_allocation$scenario_name <- "Trump tweet"
allocation <- rbind(allocation, new_allocation)

write.csv(allocation[allocation$scenario_nbr %in% c(1, 4, 5), ], 
          "data/allocation.csv", row.names = FALSE)

#3. Location: FC - latitude and longitude.
write.csv(plyr::rename(fulfill_centers_locations,
                       c("x" = "longitude",
                         "y" = "latitude",
                         "fc" = "fulfillment_center")),
          "data/fulfillment_center_locations.csv",
          row.names = FALSE)


