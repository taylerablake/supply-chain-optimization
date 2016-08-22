#Create a set of households and fulfillment centers.  Create a forecast
#of orders and some rules for fulfillment.  Then, simulate the fulfillment
#under various initial allocations.  Find the optimal initial allocation.

#Note: This is a scratch script that will be fully cleaned up later.

#Author: Chris Holloman

library(ProjectTemplate)
load.project()

set.seed(1977)

#Set a time range
n_time_periods <- 100

#Create all the households
n_households <- 10000

household_locations <- data.frame(x = runif(n_households),
                                  y = runif(n_households))

#Break the households into regions
n_regions <- 8

clusters <- kmeans(household_locations, n_regions)

household_locations$region <- clusters$cluster

#Create the fulfillment centers
n_fulfill_centers <- 4

fulfill_centers_locations <- data.frame(x = runif(n_fulfill_centers),
                                        y = runif(n_fulfill_centers),
                                        fc = 1:n_fulfill_centers)

#Calculate distances among fulfillment centers
transfer_distances <- as.matrix(dist(fulfill_centers_locations[, c("x", "y")]))

ggplot(household_locations) +
  aes(x = x, y = y, color = as.factor(region)) +
  geom_point() +
  geom_text(data = fulfill_centers_locations,
            aes(x = x, y = y, label = fc),
            color = "#000000", size = 4)


#Define the posterior distribution of actual orders in different regions.
#Data distribution
#Y_{kt} ~ Poi(\mu_{kt})
#\mu_{kt} = \theta_{k1} * exp(-t / \theta_{k2})
#Posterior distribution of \theta
theta_1_means <- rnorm(n_regions, 10, 2)
theta_1_variances <- rgamma(n_regions, 1, 1)
theta_2_means <- rgamma(n_regions, 3 * n_time_periods / 3, 3)
theta_2_variances <- rgamma(n_regions, 5 * n_time_periods / 6, 5)

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
  facet_wrap(~ region)

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
    fc_to_region_distances[i, j] <- dist(matrix(
      c(fulfill_centers_locations$x[i],
        fulfill_centers_locations$y[i],
        mean(household_locations$x[household_locations$region == j]),
        mean(household_locations$y[household_locations$region == j])),
      ncol = 2, byrow = TRUE))
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
alpha_21 <- 1 #fixed cost of delivering a unit
alpha_22 <- 3 #variable cost of delivering a unit 1 distance unit
min_transfer <- 10 #Minimum number of units you can ship between FCs
alpha_11 <- 4
alpha_12 <- .1
alpha_13 <- 1
AUR <- 29.99 #Item sale price
delta <- 0.0069 #Discount factor per additional unit over demand (markdown rate)
                #0.0069 implies that AUR is cut in half for an over-order of 100 units

#Simulate the system
options(warn = 2)
n_generations <- 100 #This must be less than or equal to n_samples
total_value <- rep(NA, n_generations) #Store the value
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
    
    #Randomly select the set of houses that placed orders
    true_total_buy <- sum(true_buys[, time_point])
    order_hhs <- NULL
    for (j in 1:n_regions) {
      order_hhs <- c(order_hhs, sample(which(household_locations$region == j),
                                       true_buys[j, time_point], replace = FALSE))
    }
    
    #Calculate distances from each ordering HH to each FC
    deliver_distances <- as.matrix(dist(rbind(fulfill_centers_locations[, c("x", "y")],
                               household_locations[order_hhs, c("x", "y")])))
    deliver_distances <- deliver_distances[1:n_fulfill_centers, 
                                           -(1:n_fulfill_centers)]
    deliver_distances <- matrix(as.numeric(deliver_distances), nrow = n_fulfill_centers)
    
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
    delivery_cost <- sum(alpha_21 + alpha_22 * 
      as.numeric(deliver_distances)[as.numeric(current_delivery_strategy) == 1])
    
    #Update inventory
    fulfill_total <- apply(current_delivery_strategy, 1, sum)
    current_inventory <- current_inventory - fulfill_total
    
    #Determine whether we're going to do any transfers among FCs
    if (sum(current_inventory) > 0 & time_point < n_time_periods) {
      
      #Calculate expected buys remaining in each FC
      buys_remaining <- apply(matrix(expected_buys[, (time_point + 1):n_time_periods],
                                     nrow = n_regions), 1, sum)
      fc_expected_fulfillment <- ddply(data.frame(fc = fc_region_assignments,
                                                  buys_remaining = buys_remaining),
                                       .(fc), summarize,
                                       buys_remaining = sum(buys_remaining))
      
      #Only do something if we have a differential of at least min_transfer in each
      #direction
      gap <- current_inventory - fc_expected_fulfillment$buys_remaining
      
      transfer_cost <- 0
      while (max(gap) > min_transfer & -min(gap) > min_transfer) {
        
        #Identify which ones you can transfer from and to
        from_fcs <- which(gap > min_transfer)
        to_fcs <- which(gap < -min_transfer)
        
        #Which one has the most to give?
        giver <- from_fcs[which.max(gap[from_fcs])]
        
        #Which one needs the most?
        receiver <- to_fcs[which.min(gap[to_fcs])]
        
        #Transfer the minimum of the amount needed or the amount available to give
        to_transfer <- round(min(-gap[receiver], gap[giver]))
        current_inventory[giver] <- current_inventory[giver] - to_transfer
        current_inventory[receiver] <- current_inventory[receiver] + to_transfer
        
        transfer_cost <- transfer_cost + alpha_11 + alpha_12 * to_transfer +
          alpha_13 * transfer_distances[giver, receiver]
        
        gap <- current_inventory - fc_expected_fulfillment$buys_remaining
        
      }

    }
    
    total_fulfilled <- total_fulfilled + fulfill_total
    total_delivery_cost <- total_delivery_cost + delivery_cost
    total_transfer_cost <- total_transfer_cost + transfer_cost
    
    
  }
  
  #Record the total value of the strategy
  amt_sold <- sum(total_fulfilled)
  total_value[this_generation] <- amt_sold * AUR * (total_buy < sum(true_buys)) +
    ((amt_sold * AUR + (total_buy - amt_sold) * AUR * exp(-delta * (total_buy - amt_sold))) * 
       (sum(initial_allocation) >= sum(true_buys))) -
    total_delivery_cost -
    total_transfer_cost
  
}


ggplot(household_locations) +
  aes(x = x, y = y, color = as.factor(region)) +
  geom_point() +
  geom_text(data = fulfill_centers_locations,
            aes(x = x, y = y, label = fc),
            color = "#000000", size = 4) +
  geom_point(data = household_locations[order_hhs,],
             aes(x = x, y = y),
             color = "#000000", size = 3)

