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
  geom_point(alpha=0.5) +
  geom_text(data = fulfill_centers_locations,
            aes(x = x, y = y,label = fc),
            color = "#000000", size = 5) +
  scale_x_continuous("") +
  scale_y_continuous("") +
  scale_color_discrete("Region") +
  theme_minimal() +
  xlab("") +
  ylab("")
ggsave("reports - 1 - Planning/white-paper-figures/hh-fc-locations.eps",
       height = 4, width = 6)


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
curve_frame <- ddply(curve_frame, .(region, x),
                     .fun = function(mdf) {
                       data.frame(l95 = quantile(mdf$y, .025),
                                  u95 = quantile(mdf$y, .975))
                     })

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
ggsave(file.path(getwd(),"reports - 1 - Planning","white-paper-figures","demand-curve-95pct.eps"),
       height = 4, width = 6)
ggsave(file.path(getwd(),"reports - 1 - Planning","white-paper-figures","demand-curve-95pct.png"),
       height = 4, width = 6)

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
alpha_1 <- 5 #fixed cost of delivering a unit
alpha_2 <- 5 #variable cost of delivering a unit 1 distance unit
alpha_3 <- 2 #variable cost of delivering a unit 1 time unit
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
    delivery_cost <- sum(alpha_1 + alpha_2 * 
      as.numeric(deliver_distances)[as.numeric(current_delivery_strategy) == 1] +
        alpha_3 *
        floor(10 * as.numeric(deliver_distances)[as.numeric(current_delivery_strategy) == 1]))
    
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
alpha_2 <- 5 #variable cost of delivering a unit 1 distance unit
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
#Override manually since this didn't find the max
allocation_list[[1]] <- seq(300, 700, by = 100)
allocation_list[[2]] <- seq(500, 900, by = 100)
allocation_list[[3]] <- seq(400, 800, by = 100)
other_allocations <- as.matrix(expand.grid(allocation_list))
other_allocations <- cbind(other_allocations,
                           total_buy - apply(other_allocations, 1, sum))
allocations <- rbind(allocations, other_allocations)

options(warn = 2)
output_storage <- NULL
t1 <- Sys.time()
for (this_allocation in 1:nrow(allocations)) {
  print(this_allocation)
  
  initial_allocation <- as.numeric(allocations[this_allocation, ])

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
      delivery_cost <- sum(alpha_1 + alpha_2 * 
                             as.numeric(deliver_distances)[as.numeric(current_delivery_strategy) == 1] +
                             alpha_3 *
                             floor(10 * as.numeric(deliver_distances)[as.numeric(current_delivery_strategy) == 1]))
      
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


save.image(file.path(getwd(),
                     "data",
                     paste0("example-application-run-",
                     gsub(" ","_",gsub(":","-",Sys.time())),".RData")))

allocation_frame <- as.data.frame(allocations)
allocation_frame$allocation_number <- 1:nrow(allocation_frame)
output_storage <- merge(output_storage, allocation_frame)

fit <- lm(total_value ~ Var1 + Var2 + Var3
          + Var1 * Var2 + Var1 * Var3 + Var2 * Var3
          + I(Var1^2) + I(Var2^2) + I(Var3^2),
          data = output_storage)

pred_frame <- output_storage
pred_frame$predicted <- predict(fit)
pred_frame <- unique(pred_frame[, c("Var1", "Var2", "Var3", "V4", "predicted")])
pred_frame[which.max(pred_frame$predicted),]

pred_frame <- expand.grid(Var1 = seq(300, 700, by = 10),
                          Var2 = seq(500, 900, by = 10),
                          Var3 = seq(400, 800, by = 10))
pred_frame$predicted <- predict(fit, newdata = pred_frame)
p <- ggplot(pred_frame[pred_frame$Var3 == 700,]) +
  aes(x = Var1, y = Var2, color = predicted) +
  geom_point(size = 6) +
  facet_wrap(~ Var3) +
  ggtitle("X") +
  theme_minimal() +
  scale_color_gradientn(colors = terrain.colors(10))
p <- ggplot(pred_frame) +
  aes(x = Var1, y = Var2, z = predicted) +
  geom_contour() +
  facet_wrap(~ Var3) +
  ggtitle("X")
p <- ggplot(pred_frame) +
  aes(x = Var1, y = Var2, z = predicted) +
  geom_raster(aes(fill = predicted)) +
  scale_fill_gradient(low = "#0000FF",
                         high = "#FF0000") +
  geom_contour(color = "#000000") +
  facet_wrap(~ Var3) +
  theme_minimal() +
  ggtitle("X")
p

avgs <- ddply(output_storage, .(Var1, Var2, Var3, V4),
              summarize, ev = mean(total_value))
avgs[which.max(avgs$ev),]

p <- ggplot(avgs[avgs$Var3 != 1003,]) +
  aes(x = Var1, y = Var2, color = ev) +
  geom_point(size = 10) +
  facet_wrap(~ Var3) +
  ylim(200,900) +
  xlim(290,750) +
  theme_minimal() +
  ggtitle("X")
p

#Create images of the fitted model along with observed values.
avgs125 <- avgs[avgs$Var3 != 1003,]
avgs125$loss <- avgs125$ev - max(avgs125$ev)
fit <- lm(loss ~ Var1 + Var2 + Var3
          + Var1 * Var2 + Var1 * Var3 + Var2 * Var3
          + I(Var1^2) + I(Var2^2) + I(Var3^2),
          data = avgs125)
pred_frame$predicted <- predict(fit, newdata = pred_frame)

pred_frame125 <- pred_frame[pred_frame$Var1 != 1003,]
ggplot(pred_frame125[pred_frame125$Var1 %in% seq(300, 700, by = 100) &
                    pred_frame125$Var2 %in% seq(500, 900, by = 100),]) +
  aes(x = Var3, y = predicted) +
  geom_line() +
  geom_point(data = avgs125, aes(x = Var3,  y = loss)) +
  scale_x_continuous("Initial Allocation to Fulfillment Center 3") +
#  scale_y_continuous(label = dollar) +
  facet_grid(Var1 ~ Var2) +
  theme(axis.text.x = element_text(size = 5))
ggsave(file.path(getwd(),"reports - 1 - Planning","white-paper-figures","model-fit-broad.png"),
       height = 4, width = 6)

max_panel <- pred_frame125[pred_frame125$Var1 == 500 &
                             pred_frame125$Var2 == 800,]
max_panel[which.max(max_panel$predicted),]

final_optimum <- expand.grid(Var1 = 490:510,
                             Var2 = 790:810,
                             Var3 = 530:550)
final_optimum$predicted <- predict(fit, final_optimum)
final_optimum[which.max(final_optimum$predicted),]

