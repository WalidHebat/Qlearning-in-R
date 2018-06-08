##### Implementing Grid World

### Initial Setup of Grid World in R
actions <- c("up", "down", "left", "right")

x <- 1:4
x
y <- 1:3
y

states <- expand.grid(x=x, y=y)

rewards <- matrix(rep(0, 12), nrow=3)
rewards[2, 2] <- NA
rewards
rewards[1, 4] <- 1
rewards[2, 4] <- -1

# initial values
values <- rewards

# Transition probabilities
transition <- list("up" = c("up" = 0.8, 
                           "down" = 0, 
                           "left" = 0.1, 
                           "right" = 0.1), 
                   "down"= c("down" = 0.8, 
                          "up" = 0, 
                          "left" = 0.1, 
                          "right" = 0.1),
                   "left"= c("left" = 0.8, 
                          "right" = 0, 
                          "down" = 0.1, 
                          "up" = 0.1),
                   "right"= c("right" = 0.8, 
                          "left" = 0, 
                          "down" = 0.1, 
                          "up" = 0.1))


actions
states
values
transition

# The value of an action
action.values <- list("up" = c("x" = 0, "y" = 1), 
                      "down" = c("x" = 0, "y" = -1),
                      "left" = c("x" = -1, "y" = 0),
                      "right" = c("x" = 1, "y" = 0))
action.values

# act() function serves to move the robot 
# through states based on an action
act <- function(action, state) {
  action.value <- action.values[[action]]
  new.state <- state
  # stops if you are in 4.1 or 4.2
  if(state["x"] == 4 && state["y"] == 1 || (state["x"] == 4 && state["y"] == 2))
    return(state)
  # finds new state values for new.x and new.y
  new.x = state["x"] + action.value["x"]
  new.y = state["y"] + action.value["y"]
  # constrains movements to limits of x and y
  new.state["x"] <- min(x[length(x)], max(x[1], new.x))
  new.state["y"] <- min(y[length(y)], max(y[1], new.y))
  # avoids state 2.2 (N/A reward): state does not change
  if(is.na(rewards[new.state["y"], new.state["x"]]))
    new.state <- state
  # act() function returns the new state
  # you have moved to by the action taken
  return(new.state)
}

# Perform Bellman update to find best value function
bellman.update <- function(action, state, values, gamma=1) {
  state.transition.prob <- transition[[action]]
  q <- rep(0, length(state.transition.prob))
  for(i in 1:length(state.transition.prob)) {  
    # is using act() function above to find new state
    new.state <- act(names(state.transition.prob)[i], state) 
    # is updating value based on action
    q[i] <- (state.transition.prob[i] * (rewards[state["y"], 
                                                 state["x"]] + (gamma * values[new.state["y"], 
                                                                               new.state["x"]])))
  }
  # is summing updated values to value function
  sum(q)
}

# Iterate for find optimal policy based on best
# value Q function
value.iteration <- function(states, actions, rewards, 
                            values, gamma, niter) {
  for (j in 1:niter) {
    for (i in 1:nrow(states)) {
      state <- unlist(states[i,])
      if(i %in% c(4, 8)) next # terminal states
      q.values <- as.numeric(lapply(actions, 
                                    # applies bellman update
                                    bellman.update, 
                                    state=state, 
                                    values=values, 
                                    gamma=gamma))
      values[state["y"], state["x"]] <- max(q.values)
    }
  }
  return(values)
}

final.values <- value.iteration(states=states, 
                                actions=actions, 
                                rewards=rewards, 
                                values=values,
                                # no discount or effective
                                # infinite horizon
                                gamma=0.99, 
                                niter=100)

final.values # can see if you begin in 1.1,
# then you should go up, up, right, right,
# right to follow highest values associated
# with each subsequent action
#           [,1]      [,2]      [,3]     [,4]
# [1,] 0.9516605 0.9651596 0.9773460  1.00000
# [2,] 0.9397944        NA 0.8948359 -1.00000
# [3,] 0.9266500 0.9150957 0.9027132  0.81989