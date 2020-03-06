#' Class: Simulation
#' Class to store and update the simulation for each type of individual
Simulation <- R6::R6Class(
  'Simulation',
  private = list(
    .impl = NULL
    #.individual_to_states = list(),
    #.individual_to_variables = list(),
    #.individual_to_constants = list(),
    #.current_timestep = 1,
    #.individuals = list()
  ),
  public = list(
    #' @description
    #' Return a list of the simulated states and variables for the simulation
    #' @param individual to render
    render = function(...) {
      private$.impl$render(...)
      #list(
        #states=private$.individual_to_states[[individual$name]],
        #variables=private$.individual_to_variables[[individual$name]]
      #)
    },

    #' @description
    #' Get a SimFrame for the current timestep
    get_current_frame = function() {
      SimFrame$new(private$.impl$get_current_frame())
    },

    #' @description
    #' Perform updates on the a simulation, increment the counter and return the
    #' next simulation frame
    #' @param updates is a list of updates to apply
    apply_updates = function(...) {
      private$.impl$apply_updates(...)
    },

    #' @description
    #' Create a blank simulation and then initialize first timestep
    #' @param individuals a list of Individual to initialise for
    #' @param timesteps the number of timesteps to initialise for
    initialize = function(...) {
      private$.impl <- new(SimulationCpp, ...)
    }
  )
)

#' Main simulation loop
#'
#' @param individuals a list of Individual to simulate
#' @param processes a list of processes to execute on each timestep
#' @param end_timestep the number of timesteps to simulate
#' @param parameters a list of named parameters to pass to the process functions
#' @export simulate
simulate <- function(individuals, processes, end_timestep, parameters=list()) {
  if (end_timestep <= 0) {
    stop('End timestep must be > 0')
  }
  if (! is.list(individuals)) {
    individuals <- list(individuals)
  }
  output <- Simulation$new(individuals, end_timestep)
  frame <- output$get_current_frame()
  for (timestep in seq_len(end_timestep - 1)) {
    updates <- unlist(
      lapply(
        processes,
        function(process) { process(frame, timestep, parameters) }
      )
    )
    output$apply_updates(updates)
    frame <- output$get_current_frame()
  }
  output
}
