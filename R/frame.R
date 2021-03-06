#' Class: SimFrame
#' Represents the state of all individuals in a timestep
SimFrame <- R6::R6Class(
  'SimFrame',
  private = list(
    .states = list(),
    .variables = list(),
    .constants = list()
  ),
  public = list(
    #' @description
    #' Get the index of individuals with a particular state
    #' @param individual of interest
    #' @param state of interest
    get_state = function(individual, ...) {
      states <- list(...)
      if (!(individual$name %in% names(private$.states))) {
        stop('Unregistered individual')
      }
      for (state in states) {
        if(!individual$check_state(state)) {
          stop('Invalid state')
        }
      }
      individual_frame <- private$.states[[individual$name]]
      which(
        individual_frame %in% vapply(
          states,
          function(state) state$name,
          character(1)
        )
      )
    },

    #' @description
    #' Get a variable vector for an individual
    #' @param individual of interest
    #' @param variable of interest
    get_variable = function(individual, variable) {
      if (!(individual$name %in% names(private$.variables))) {
        stop('Unregistered individual')
      }
      if (!individual$check_variable(variable)) {
        stop('Invalid variable')
      }
      individual_frame <- private$.variables[[individual$name]]
      individual_frame[,variable$name,]
    },

    #' @description
    #' Get a constant vector for an individual
    #' @param individual of interest
    #' @param constant of interest
    get_constant = function(individual, constant) {
      if (!(individual$name %in% names(private$.variables))) {
        stop('Unregistered individual')
      }
      if (!individual$check_constant(constant)) {
        stop('Invalid constant')
      }
      individual_frame <- private$.constants[[individual$name]]
      individual_frame[,constant$name]
    },

    #' @description
    #' Create an initial SimFrame
    #' @param individuals is a list of Individual
    #' @param state is a list of states for each individual at the current
    #' timestep
    #' @param variables is a list of variables for each individual at the
    #' current timestep
    #' @param constants is a list of constants for each individual at the
    #' current timestep
    initialize = function(individuals, states, variables, constants) {
      names <- lapply(individuals, function(i) { i$name })
      private$.states <- setNames(states, names)
      private$.variables <- setNames(variables, names)
      private$.constants <- setNames(constants, names)
    }
  )
)
