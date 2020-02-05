#' Extract parameter information from a function
#'
#' @param fun_call
#'
#' @return data.frame
#'
#' @examples
#' extract_function_params("dplyr::count")
extract_function_params <- function(fun_call){
  if(is.na(fun_call)) { return(NA)}
  else{
    formals_obj <- formals(lazyeval::lazy_eval(fun_call))
    params <- names(formals_obj)
    param_type <- sapply(params, FUN = function(x) class(unlist(formals_obj)[x][[1]]))
    param_df <- data.frame(param_name = params, param_type = param_type, row.names = NULL)
    param_df <- param_df %>%
      mutate(param_default = purrr::map2(.x = params, .y = param_type,
                                         .f = function(x,y){
                                           if(y == "name" | is.null(y)) {"NULL"} else{unlist(formals_obj)[x][[1]]}
                                         }))

    return(param_df)}
}





#' Extract package information
#'
#' Extract information about an R package, including version, source,
#' objects, and dependencies.
#'
#' @param packages a package name or vector of package names
#'
#' @return a tibble
#' @export
#'
#' @examples
#' extract_package_info(packages = "ggplot2")
#' extract_package_info(packages = c("ggplot2", "dplyr"))
extract_package_info <- function(packages){
  # we first want to get some meta-information about the packages.
  # we will eventually want to make sure the input is actually a package
  # (can probably do this in the wrapper function)
  package_vec <- packages

  # identify base packages installed
  base_packages <- rownames(installed.packages(priority = "base"))

  package_source <- sapply(package_vec, FUN = function(x){
    ifelse(x %in% base_packages, "base", str_split(devtools::package_info(x)[x,]$source, " ")[[1]][1])
  })

  # identify the loaded version of the package
  package_lversion <- sapply(package_vec,
                             FUN = function(x){
                               devtools::package_info(x)[x,]$loadedversion
                             })

  package_track <- data.frame(package_name = package_vec,
                              package_source = package_source,
                              package_lversion = package_lversion,
                              stringsAsFactors = F)

  # package_gitrepo and package_gitcommit
  package_track <- package_track %>%
    mutate(package_gitinfo =
             purrr::map2(.x = package_name,
                         .y = package_source,
                         .f = function(x, y){
                           extra_info <- str_remove(str_remove(str_split(devtools::package_info(x)[x,]$source, " ")[[1]][2], "\\)"), "\\(")
                           repo_and_commit <- ifelse(y != "Github", NA, extra_info)
                           repo <- strsplit(as.character(repo_and_commit), "@")[[1]][1]
                           commit <- strsplit(as.character(repo_and_commit), "@")[[1]][2]
                           return(data.frame(package_gitrepo = repo,
                                             package_gitcommit = commit, stringsAsFactors = F))
                         })) %>% unnest(package_gitinfo)

  package_track <- package_track %>%
    mutate(package_dependencies = purrr::map(package_name, .f = function(x){
      desc_obj <- description$new(file = system.file("DESCRIPTION", package = x))
      package_deps <- desc_obj$get_deps()$package[desc_obj$get_deps()$type == "Depends" | desc_obj$get_deps()$type == "Imports"]
      package_deps <- if(length(package_deps) == 0){NA} else{package_deps}
      return(package_deps)
    }))

  package_track <- package_track %>%
    mutate(package_objects = purrr::map(package_name, .f = function(package_name){

      # find exported functions
      fun_exported <- getNamespaceExports(package_name)

      # find all exported objects
      fun_all <- ls(getNamespace(package_name), all.names = F)

      ## need a better way to grab the data objects...
      filename <- list.files(system.file(package = package_name), recursive = TRUE,
                             full.names = T, pattern = "*Rdata.rds")

      data_obj <- if(length(filename) == 0) {NA} else{names(readRDS(filename))}


      # combine all to get unique objects exported by a package
      obj_names <- unique(c(fun_exported, fun_all, data_obj))
      obj_names <- obj_names[!is.na(obj_names)]

      if(length(obj_names) == 0){return(NA)}
      else {
        # whether it's exported
        is_exported <- obj_names %in% fun_exported

        # class of object
        # is it data?
        obj_type <- ifelse(obj_names %in% data_obj, "data", "non-exported")
        # is it exported?
        obj_type <- ifelse(is_exported, "exported", obj_type)


        # character call for the function (based on class/exported)
        dots_val <- ifelse(obj_type == "exported" | obj_type == "data", "::", ":::")
        object_chr_call <- paste0(package_name, dots_val, "`", obj_names, "`")

        #
        package_objects <- data.frame(object_name = obj_names, is_exported = is_exported,
                                      obj_type = obj_type, object_chr_call = object_chr_call,
                                      stringsAsFactors = F)

        package_objects <- package_objects %>%
          # class of object and object itself
          mutate(obj_class = purrr::map(object_chr_call, .f = function(x) {class(lazyeval::lazy_eval(x))}),
                 function_text = purrr::map2(.x = object_chr_call, .y = obj_class,
                                             .f = function(x,y){
                                               if(y[1] == "function") {body(lazyeval::lazy_eval(x))}
                                               else {lazyeval::lazy_eval(x)}
                                               #lazyeval::lazy_eval(x)
                                             }),
                 # checksum for object
                 obj_checksum = purrr::map_chr(function_text, .f = function(x) digest(x, algo = "md5")),
                 fun_type = purrr::map_chr(object_chr_call, .f = function(x){
                   fun_text <- lazyeval::lazy_eval(x)
                   function_type = NA
                   function_type = ifelse(is_primitive(fun_text), "primitive", function_type)
                   function_type = ifelse(is_closure(fun_text), "closure", function_type)
                   return(function_type)
                 }),
                 fun_params = purrr::map2(.x = object_chr_call, .y = fun_type, .f = function(x, y){
                   param_input <- ifelse(is.na(y), NA, x)
                   param_df <- extract_function_params(param_input)
                   return(param_df)
                 }),
                 params_checksum = purrr::map_chr(fun_params, .f = function(x) digest(x, algo = "md5")))


        return(package_objects)}
    }))

  return(package_track)

}



#' Take inventory of entire dependency network for package or vector of packages
#'
#' @param packages package name or vector of package names
#'
#' @return a tibble
#' @export
#'
#' @examples
#' take_inventory(packages = "ggplot2")
#' take_inventory(packages = c("ggplot2", "dplyr"))
take_inventory <- function(packages){
  `%notin%` = Negate(`%in%`)
  package_list <- packages
  message("Extracting direct dependencies...")

  package_tree <- extract_package_info(packages = packages)
  package_tree$level <- "level_0"
  package_tree$stems_from <- "direct_call"

  level_num <- 0
  unlisted_deps <- unlist(package_tree$package_dependencies)
  package_list_new <- unique(c(packages, unlisted_deps[unlisted_deps != "R"]))
  package_list_new <- package_list_new[!is.na(package_list_new)]

  while(identical(package_list_new, package_list) == F){
    level_num <- level_num + 1
    level_val <- paste0("level_", level_num)
    message(paste0("Extracting level ", level_num, " dependencies..."))

    new_branches_info <- package_tree %>%
      unnest(package_dependencies) %>%
      select(package_dependencies, package_name) %>%
      filter(package_dependencies != "R",
             package_dependencies %notin% package_list) # won't give entire dependency network and all connections, but will get us to the whole "set" of roots

    # we are grabbing multiples of some packages by not simplifying this before we go forward...
    new_branches_names <- new_branches_info$package_dependencies

    new_branches_df <- extract_package_info(packages = new_branches_names)
    new_branches_df$level <- level_val
    new_branches_df$stems_from <- new_branches_info$package_name

    package_tree <- rbind(package_tree, new_branches_df)
    package_list <- package_list_new[!is.na(package_list_new)]
    unlisted_deps <- unlist(package_tree$package_dependencies)
    package_list_new <- unique(c(package_list, unlisted_deps[unlisted_deps != "R"]))
    package_list_new <- package_list_new[!is.na(package_list_new)]
  }

  return(package_tree)
}




#' Plot dependency tree
#'
#' @param inventory_object a tibble exported from the `take_inventory` function
#'
#' @return a ggplot visualization of the given dependency tree
#' @export
#'
#' @examples
#' ggplot2_inventory <- take_inventory(packages = "ggplot2")
#' plot_inventory(inventory_object = ggplot2_inventory)
plot_inventory <- function(inventory_object){
  inventory_plot <- inventory_object %>%
    mutate(num_package_objects = purrr::map_dbl(package_objects, .f = function(x){
      nrow(x)
    })) %>%
    select(c(package_name, package_source, num_package_objects, level, stems_from)) %>%
    nest(data = c(package_source, num_package_objects, stems_from)) %>%
    group_by(level) %>%
    mutate(n_level = n(), num_in_level = seq_along(n_level),
           x_coord_plot = purrr::map2_dbl(.x = n_level, .y = num_in_level, .f = function(x,y){
             sequence <- seq(0, 1, length.out = x + 2)[-c(1, x + 2)]
             return(sequence[y])
           })) %>%
    ungroup() %>%
    mutate(level_num = purrr::map_dbl(level, .f = function(x){
      as.numeric(strsplit(x, "_")[[1]][2])
    }),
    arrow_start_y = level_num - 1) %>%
    unnest(data)

  pkg_source <- c()
  row_id <- c()
  for(i in 1:nrow(inventory_plot)){
    pkg_source[i] <- ifelse(inventory_plot$stems_from[i] == "direct_call",
                            NA, inventory_plot$stems_from[i])
    row_id[i] <- ifelse(is.na(pkg_source[i]), NA, which(inventory_plot$package_name == pkg_source[i]))
  }
  inventory_plot$arrow_start_x = inventory_plot$x_coord_plot[row_id]

  labels_dat <- inventory_plot %>%
    nest(data = c(level, package_source, num_package_objects, stems_from, n_level,
                  num_in_level, arrow_start_y, arrow_start_x)) %>%
    select(c(package_name, x_coord_plot, level_num)) %>%
    unnest(c(package_name, x_coord_plot, level_num))

  n_colors <- nlevels(factor(inventory_plot$package_source))
  palette_vals <- c("grey80", "steelblue", "salmon")
  names(palette_vals) <- c("base", "CRAN", "Github")
  max_y <- max(inventory_plot$level_num)

  inventory_plot %>%
    ggplot() +
    geom_segment(aes(x = arrow_start_x, y = arrow_start_y,
                     xend = x_coord_plot, yend = level_num), alpha = 0.3) +
    geom_point(aes(x = x_coord_plot, y = level_num,
                   size = num_package_objects,
                   fill = package_source), shape = 21, color = "black") +
    geom_text(data = labels_dat,
              aes(x = x_coord_plot, y = level_num - 0.12, label = package_name),
              angle = 30) +
    theme_void() +
    ylim(c(max_y, -.5)) +
    #xlim(c(-0.2, 1.2)) +
    scale_fill_manual(name = "Package Source", values = palette_vals) +
    scale_size_continuous(name = "Number of Objects \nin Package")
}





#' Store an inventory object as a .rda file
#'
#' @param inventory_object An inventory object, a tibble from the `take_inventory` function.
#' @param filepath Location to save the inventory
#' @param timestamp Add timestamp variable to inventory? Default is `TRUE`.
#'
#' @return saved .rda file
#' @export
#'
#' @examples
#' two_pack_inventory <- take_inventory(packages = c("ggplot2", "dplyr"))
#' store_inventory(two_pack_inventory, filepath = "~/Path/To/Folder", timestamp = T)
store_inventory <- function(inventory_object, filepath, timestamp = T){
  copied_rows <- inventory_object %>% select(package_name) %>% duplicated()
  inventory_unique <- inventory_object[!copied_rows,]
  inventory_unique <- inventory_unique %>%
    mutate(stems_from_vec = purrr::map(package_name, .f = function(x){
      stems_from_set <- inventory_object %>% filter(package_name == x)
      stems_from_set <- stems_from_set$stems_from
      return(stems_from_set)
    }))
  if(timestamp == T) {inventory_unique$timestamp <- date()}

  saveRDS(inventory_unique, file = filepath)
}

