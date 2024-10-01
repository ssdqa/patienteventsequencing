
#' Patient Event Sequencing Output
#'
#' @param process_output the output of the `pes_process` function
#' @param output_function the name of the output function that should be used provided in the `parameter_summary` csv
#'                        file that is output to the provided results folder after running the `pes_process` function
#'
#' @return a graph or series of graphs visualizing the results from process_output
#'
#' @export
#'
pes_output <- function(process_output,
                       output_function){


  if(output_function == 'pes_ss_exp_nt'){

    pes_output <- pes_ss_exp_nt(process_output = process_output)

  }else if(output_function == 'pes_ms_exp_nt'){

    pes_output <- pes_ms_exp_nt(process_output = process_output)

  }else if(output_function == 'pes_ms_anom_nt'){

    pes_output <- pes_ms_anom_nt(process_output = process_output)

  }else if(output_function == 'pes_ss_exp_at'){

    pes_output <- pes_ss_exp_at(process_output = process_output)

  }else if(output_function == 'pes_ss_anom_at'){

    pes_output <- pes_ss_anom_at(process_output = process_output)

  }else if(output_function == 'pes_ms_exp_at'){

    pes_output <- pes_ms_exp_at(process_output = process_output)

  }else if(output_function == 'pes_ms_anom_at'){

    pes_output <- pes_ms_anom_at(process_output = process_output)

  }else{cli::cli_abort('Please enter a valid output_function for this check type.')}

  return(pes_output)
}
