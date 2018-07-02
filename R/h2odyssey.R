##----- Adapted from http://docs.h2o.ai/h2o/latest-stable/h2o-docs/faq/clusters.html

#' Detect the nodes of a SLURM job
#'
#' @param None
#'
#' @return A list of nodes
#'
#' @examples
#' detect_nodes()
#'
#' @export
detect_nodes <- function() {
  # Check on the nodes we have access to.
  node_list <- Sys.getenv("SLURM_JOB_NODELIST")
  cat("SLURM nodes:", node_list, "\n")
  return(node_list)
}

#' Detect the IPs of the nodes of a SLURM job
#'
#' @param node_list Nodes of the SLURM jobs
#'
#' @return List of SLURM IPs
#'
#' @examples
#' node_list <- detect_nodes()
#' ips <- get_ips(node_list)
#'
#' @export
get_ips <- function(node_list = NULL) {
  if (is.null(node_list))
    node_list <- detect_nodes()
  # Loop up IPs of the allocated nodes.
  if (node_list != "") {
    nodes <- strsplit(node_list, "\\[")[[1]]
    if (length(nodes) > 1) {
      name <- nodes[[1]]
      numbers <-  nodes[[2]]
      numbers <- strsplit(numbers, "\\]")[[1]]
      numbers <- strsplit(numbers, ",")[[1]]
      machines <- NULL
      for (n in numbers) {
        nn <- strsplit(n, "-")[[1]]
        if (length(nn) == 1)
          machines <- c(machines, nn)
        else
          machines <-
            c(machines,
              formatC(
                as.numeric(nn[1]:nn[2]),
                width = nchar(nn[1]),
                format = "d",
                flag = "0"
              ))
      }
      nodes <- paste0(name, machines)
    }
    ips <- rep(NA, length(nodes))
    for (i in 1:length(nodes)) {
      args <- c("hosts", nodes[i])
      result <- system2("getent", args = args, stdout = TRUE)
      # Extract the IP from the result output.
      ips[i] <- sub("^([^ ]+) +.*$", "\\1", result, perl = TRUE)
    }
    cat("SLURM IPs:", paste(ips, collapse = ", "), "\n")

  }
  return(ips)
}

#' Detect the network of the nodes of a SLURM job
#'
#' @param ips IPs of the nodes of the SLURM job
#'
#' @return List of SLURM network
#'
#' @examples
#' node_list <- detect_nodes()
#' ips <- get_ips(node_list)
#' network <- get_network(ips)
#'
#' @export
get_network <- function(ips = NULL) {
  if (is.null(ips))
    ips <- get_ips()
  # Combine into a network string for h2o.
  network <- paste0(paste0(ips, "/32"), collapse = ",")
  cat("Network:", network, "\n")
  return(network)
}

#' Create a Java call to start the h2o worker
#'
#' @param network Network information of the SLURM job
#' @param memory The amount of memory per node in GB
#' @param path Path of h2o.jar
#' @param port Port of the h2o cluster
#'
#' @return A vector of strings to start the h2o workers
#'
#' @examples
#' node_list <- detect_nodes()
#' ips <- get_ips(node_list)
#' network <- get_network(ips)
#' args <- create_java_call(network)
#'
#' @export
create_java_call <-
  function(network = NULL,
           memory = 2,
           path = NULL,
           port = NULL) {
    if (is.null(network))
      network <- get_network()
    if (is.null(path))
      path <-
        system.file(file.path("java", "h2o.jar"), package = "h2o")
    if (is.null(port))
      port <- 54321
    # Options to pass to java call:
    args <- c(
      # -Xmx30g allocate 30GB of RAM per node. Needs to come before "-jar"
      paste0("-Xmx", memory, "g"),
      # Specify path to downloaded h2o jar.
      paste0("-jar ", path),
      # Specify a cloud name for the cluster.
      "-name h2o_r",
      # Specify IPs of other nodes.
      paste("-network", network),
      # Specify port.
      paste("-port", port)
    )
    cat(paste0("Args:\n", paste(args, collapse = "\n"), "\n"))
    return(args)
  }

#' Add the SLURM nodes to the list of known hosts
#'
#' @param ips IPs of the nodes of the SLURM job
#'
#' @return None This function is used for its side effects.
#'
#' @examples
#' node_list <- detect_nodes()
#' ips <- get_ips(node_list)
#' make_nodes_known_hosts(ips)
#'
#' @export
make_nodes_known_hosts <- function(ips = NULL) {
  if (is.null(ips))
    ips <- get_ips()
  # Add nodes to known hosts
  for (ip in ips) {
    system(paste("ssh-keyscan", ip, ">> ~/.ssh/known_hosts"))
  }
}

#' Start an h2o worker on each node of the SLURM cluster
#'
#' @param ips IPs of the nodes of the SLURM job
#' @param memory The amount of memory per node in GB
#' @param path Path of h2o.jar
#' @param port Port of the h2o cluster
#'
#' @return None This function is used for its side effects.
#'
#' @examples
#' node_list <- detect_nodes()
#' ips <- get_ips(node_list)
#' start_h2o_workers(ips)
#'
#' @export
start_h2o_workers <- function(ips = NULL,
                              memory = 2,
                              path = "/n/home03/cchoirat/apps/R/h2o/java/h2o.jar",
                              port = NULL) {
  if (is.null(ips))
    ips <- get_ips()
  if (is.null(port))
    port <- 54321
  # Specify how many nodes we want h2o to use.
  h2o_num_nodes <- length(ips)
  make_nodes_known_hosts(ips)
  # Run once for each node we want to start.
  for (node_i in 1:h2o_num_nodes) {
    cat("\nLaunching h2o worker on", ips[node_i], "\n")
    args <-
      create_java_call(memory = memory,
                       path = path,
                       port = port)
    new_args <- c(ips[node_i], "java", args)
    # Ssh into the target IP and launch an h2o worker with its own
    # output and error files. These could go in a subdirectory.
    cmd_result <- system2(
      "ssh",
      args = new_args,
      stdout = paste0("h2o_out_", node_i, ".txt"),
      stderr = paste0("h2o_err_", node_i, ".txt"),
      # Need to specify wait=FALSE so that it runs in the background.
      wait = FALSE
    )
    # This should be 0.
    cat("Cmd result:", cmd_result, "\n")
    # Wait one second between inits.
    Sys.sleep(1L)
  }
  # Wait 3 more seconds to find all the nodes, otherwise we may only
  # find the node on localhost.
  Sys.sleep(30L)
  # Check if h2o is running. We will see ssh processes and one java process.
  system2("ps", c("-efww", "| grep h2o.jar"), stdout = TRUE)
}

#' Start an h2o cluster with an h2o worker on each node of the SLURM cluster
#'
#' @param memory The amount of memory per node in GB
#' @param path Path of h2o.jar
#' @param port Port of the h2o cluster
#'
#' @return None This function is used for its side effects.
#'
#' @examples
#' start_h2o_cluster()
#' library(h2o)
#' # Connect to our existing h2o cluster.
#' # Do not try to start a new server from R.
#' h2o.init(startH2O = FALSE)
#' # Code here...
#' h2o.shutdown()
#'
#' @export
start_h2o_cluster <- function(memory = 2,
                              path = NULL,
                              port = NULL) {
  if (is.null(path))
    path <-
      system.file(file.path("java", "h2o.jar"), package = "h2o")
  if (is.null(port))
    port <- 54321
  node_list <- detect_nodes()
  ips <- get_ips(node_list)
  network <- get_network(ips)
  make_nodes_known_hosts(ips)
  start_h2o_workers(ips, memory = memory, path = path, port = port)
  h2o.init(startH2O = FALSE)
}
