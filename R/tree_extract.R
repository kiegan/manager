#' Extract parameter information from a function
#'
#' @param fun_call text string for function call for which parameter information should be returned.
#'
#' @export
#' @importFrom dplyr `%>%` mutate
#' @importFrom stringr str_split
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
#' @importFrom utils installed.packages
#' @importFrom tidyr unnest
#' @importFrom stringr str_remove
#' @importFrom dplyr rowwise
#' @importFrom digest digest
#' @importFrom desc description
#' @importFrom rlang is_primitive is_closure
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
  package_version <- sapply(package_vec,
                             FUN = function(x){
                               devtools::package_info(x)[x,]$ondiskversion
                             })

  package_track <- data.frame(package_name = package_vec,
                              package_source = package_source,
                              package_version = package_version,
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

  #### Checksum for package meta-information
  package_track <- package_track %>% rowwise() %>%
    mutate(package_meta_checksum = digest(c(package_name, package_source, package_version,
                                            package_gitrepo, package_gitcommit), algo = c("md5"))) %>%
    as.data.frame()


  # list of package dependencies and checksum for that list
  package_track <- package_track %>%
    mutate(package_dependencies = purrr::map(package_name, .f = function(x){
      desc_obj <- description$new(file = system.file("DESCRIPTION", package = x))
      package_deps <- desc_obj$get_deps()$package[desc_obj$get_deps()$type == "Depends" | desc_obj$get_deps()$type == "Imports"]
      package_deps <- if(length(package_deps) == 0){NA} else{package_deps}
      return(package_deps)
    }), package_dependencies_checksum =
      purrr::map_chr(package_dependencies,
                     .f = function(x) {digest(x, algo = "md5")}))

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

        # make dataframe of package object info

        package_objects <- data.frame(object_name = obj_names, is_exported = is_exported,
                                      obj_type = obj_type, object_chr_call = object_chr_call,
                                      stringsAsFactors = F)

        package_objects <- package_objects %>%
          # class of object and object itself
          mutate(obj_class = purrr::map(object_chr_call, .f = function(x) {class(lazyeval::lazy_eval(x))}),
                 function_text = purrr::map2(.x = object_chr_call, .y = obj_class,
                                             .f = function(x,y){
                                               deparse(lazyeval::lazy_eval(x))
                                               #if(y[1] == "function") {body(lazyeval::lazy_eval(x))}
                                               #else {lazyeval::lazy_eval(x)} ## ADD DEPARSE CAUSE BYTECODES SNEAK INTO LIST-FUNS
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

        # Checksum for meta-information about each object in the package.
        # (checksums for object and parameter values included in this meta-checksum)
        package_objects <- package_objects %>%
          rowwise() %>%
          mutate(object_meta_checksum = digest(c(object_name, is_exported, obj_type,
                                                 object_chr_call, obj_class, obj_checksum, params_checksum),
                                               algo = "md5")) %>%
          as.data.frame()

        return(package_objects)}

    }))

  # Checksum for the whole of package objects
  package_track <- package_track %>%
    mutate(package_objects_checksum = purrr::map_chr(package_objects, .f = function(x) digest(x, algo = "md5")))

  return(package_track)

}




#' Take inventory of entire dependency network for package or vector of packages
#'
#' @param packages package name or vector of package names
#'
#' @return a tibble
#' @export
#' @importFrom dplyr filter select
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

  copied_rows <- package_tree %>% select(package_name) %>% duplicated()
  inventory_unique <- package_tree[!copied_rows,]
  inventory_unique <- inventory_unique %>%
    mutate(stems_from_vec = purrr::map(package_name, .f = function(x){
      stems_from_set <- package_tree %>% filter(package_name == x)
      stems_from_set <- stems_from_set$stems_from
      return(stems_from_set)
    }))

  return(inventory_unique)
}





#' Plot dependency tree
#'
#' @param inventory_object a tibble exported from the `take_inventory` function
#'
#' @return a ggplot visualization of the given dependency tree
#' @export
#' @importFrom dplyr group_by ungroup n
#' @import ggplot2
#'
#' @examples
#' ggplot2_inventory <- take_inventory(packages = "ggplot2")
#' plot_inventory(inventory_object = ggplot2_inventory)
plot_inventory <- function(inventory_object){
  inventory_plot <- inventory_object %>%
    mutate(num_package_objects = purrr::map_dbl(package_objects, .f = function(x){
      nrow(x)
    })) %>%
    select(c(package_name, package_source, num_package_objects, level, stems_from_vec)) %>%
    nest(data = c(package_source, num_package_objects, stems_from_vec)) %>%
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
    unnest(data) %>%
    unnest(stems_from_vec)

  pkg_source <- c()
  row_id <- c()
  for(i in 1:nrow(inventory_plot)){
    pkg_source[i] <- ifelse(inventory_plot$stems_from_vec[i] == "direct_call",
                            NA, inventory_plot$stems_from_vec[i])
    row_id[i] <- ifelse(is.na(pkg_source[i]), NA, which(inventory_plot$package_name == pkg_source[i]))
  }
  inventory_plot$arrow_start_x = inventory_plot$x_coord_plot[row_id]

  labels_dat <- inventory_plot %>%
    nest(data = c(level, package_source, num_package_objects, stems_from_vec, n_level,
                  num_in_level, arrow_start_y, arrow_start_x)) %>%
    select(c(package_name, x_coord_plot, level_num)) %>%
    unnest(c(package_name, x_coord_plot, level_num))

  n_colors <- nlevels(factor(inventory_plot$package_source))
  palette_vals <- c("grey50", "steelblue", "goldenrod2", "darkred")
  names(palette_vals) <- c("base", "CRAN", "Github", "local")
  max_y <- max(inventory_plot$level_num)

  inventory_plot %>%
    ggplot() +
    geom_segment(aes(x = arrow_start_x, y = arrow_start_y,
                     xend = x_coord_plot, yend = level_num), color = "grey70", alpha = 0.5) +
    geom_point(aes(x = x_coord_plot, y = level_num,
                   size = num_package_objects,
                   color = package_source)) +
    geom_text(data = labels_dat,
              aes(x = x_coord_plot, y = level_num - 0.12, label = package_name),
              angle = 30) +
    theme_void() +
    ylim(c(max_y, -.5)) +
    #xlim(c(-0.2, 1.2)) +
    scale_color_manual(name = "Package Source", values = palette_vals) +
    scale_size_continuous(name = "Number of Objects \nin Package")
}






#' Store an inventory object as a .rda file
#'
#' @param inventory_object An inventory object, a data.frame from the `take_inventory` function.
#' @param filepath Location to save the inventory
#'
#' @return saved .rda file
#' @export
#'
#' @examples
#' two_pack_inventory <- take_inventory(packages = c("ggplot2", "dplyr"))
#' \dontrun{store_inventory(two_pack_inventory, filepath = "~/Path/To/Folder/filename.rda")}
store_inventory <- function(inventory_object, filepath){
  inventory_object$time_stamp <- date()
  saveRDS(inventory_object, file = filepath)
}



#' Compare two inventory objects
#'
#' @param inventory1 An inventory object, a data.frame from the `take_inventory` function
#' @param inventory2 A second inventory object, a data.frame from the `take_inventory` function
#' @param object_types A vector of object types to compare. Default is "exported" objects only, but can be any of c("exported", "non-exported", "data").
#' @param report_type A vector of reporting type preferences. "table" returns a data.frame with one row for each object that differs between the two inventories. "summary" returns concatenated summaries of identified differences. "objects" returns a data.frame with one row for each object which differs between the two inventories, but also includes the objects themselves as well as checksums.
#' @param summary_file A file to store inventory difference summary information. Default is "", which will return the summary in the console.
#'
#' @return a list with element `table`. A text summary will be saved to the file specified in `summary_file`, or printed in the console.
#' @import dplyr
#' @export
#'
#' @examples \dontrun{compare_inventory(inventory1, inventory2)}
compare_inventory <- function(inventory1, inventory2,
                              object_types = "exported",
                              report_type = c("table", "summary", "objects"),
                              summary_file = ""){
  `%notin%` = Negate(`%in%`)
  # first, add column to each inventory which specifies whether it's old or new
  inventory1$inventory1 <- TRUE
  inventory2$inventory2 <- TRUE

  # define global variables to make R CMD CHECK happy
  package_name <- package_meta_checksum <- package_objects_checksum <- NULL
  package_objects <- object_name <- object_meta_checksum <- NULL
  inventory_id <- obj_type <- is_exported <- object_chr_call <- NULL
  obj_checksum <- NULL

  # second, take a look at checksum 1
  inventory_check1 <- full_join(inventory1 %>%
                                  select(package_name, package_meta_checksum, inventory1),
                                inventory2 %>%
                                  select(package_name, package_meta_checksum, inventory2),
                                by = c("package_name", "package_meta_checksum")) %>%
    filter((is.na(inventory1) | is.na(inventory2)))

  check1_drops_adds <- inventory_check1 %>%
    group_by(package_name) %>%
    summarise(n = n()) %>%
    filter(n == 1) %>%
    pull(package_name)
  #check1_drops <- inventory_check1 %>%
  #  filter(package_name %in% check1_drops_adds, inventory1 == T)
  #check1_adds <- inventory_check1 %>%
  #  filter(package_name %in% check1_drops_adds, inventory2 == T)

  check1_changes <- inventory_check1 %>%
    filter(package_name %notin% check1_drops_adds) %>%
    pull(package_name) %>%
    unique()
  # filter out to packages which remain in this vector.

  #### TO DO: MESSAGING FOR UPDATED PACKAGE VERSIONS
  inventory1 <- inventory1 %>% filter(package_name %in% check1_changes)
  inventory2 <- inventory2 %>% filter(package_name %in% check1_changes)

  # next, take a look at another checksum: the set of package objects.
  inventory_check2 <- full_join(inventory1 %>% select(package_name, package_objects_checksum, inventory1),
                                inventory2 %>% select(package_name, package_objects_checksum, inventory2),
                                by = c("package_name", "package_objects_checksum"))

  # identify package whose meta-information has changed but the package objects have not changed
  check2_ident_objects <- inventory_check2 %>%
    filter((inventory1 == TRUE & inventory2 == TRUE)) %>%
    pull(package_name)

  # next, take a look at object meta-information

  inventory1_long <- inventory1 %>%
    filter(package_name %notin% check2_ident_objects) %>%
    unnest(package_objects)
  inventory2_long <- inventory2 %>%
    filter(package_name %notin% check2_ident_objects) %>%
    unnest(package_objects)

  inventory_check3 <- full_join(inventory1_long %>%
                                  select(package_name, object_name, object_meta_checksum, inventory1),
                                inventory2_long %>%
                                  select(package_name, object_name, object_meta_checksum, inventory2),
                                by = c("package_name", "object_name", "object_meta_checksum"))

  check3_object_changes <- inventory_check3 %>%
    filter((is.na(inventory1) | is.na(inventory2))) %>% select(package_name, object_name) %>% unique()

  check3_cols <- c("package_name", "object_name", "is_exported", "obj_type", "object_chr_call", "obj_checksum","fun_type", "params_checksum")
  inventory1_check3 <- left_join(check3_object_changes, inventory1_long %>% select(all_of(check3_cols), "inventory1"),
                                 by = c("package_name", "object_name"))
  inventory2_check3 <- left_join(check3_object_changes, inventory2_long %>% select(all_of(check3_cols), "inventory2"),
                                 by = c("package_name", "object_name"))

  inventory_check3_detail <- full_join(inventory1_check3, inventory2_check3,
                                       by = c("package_name", "object_name", "is_exported",
                                              "obj_type", "object_chr_call", "obj_checksum",
                                              "fun_type", "params_checksum")) %>%
    filter((is.na(inventory1) | is.na(inventory2)))

  inventory_check3_detail <- inventory_check3_detail %>%
    mutate(inventory_id = ifelse((inventory1 == T & is.na(inventory2)), "inv1", NA),
           inventory_id = ifelse((inventory2 == T & is.na(inventory1)), "inv2", inventory_id)) %>%
    filter(obj_type %in% object_types)


  inventory_check3_meta <- inventory_check3_detail %>%
    group_by(package_name, object_name) %>%
    summarise()

  inv1_check3 <- inventory_check3_detail %>%
    filter(inventory_id == "inv1") %>%
    rename(is_exported_inv1 = is_exported,
           obj_type_inv1 = obj_type,
           obect_chr_call_inv1 = object_chr_call,
           obj_checksum_inv1 = obj_checksum,
           fun_type_inv1 = fun_type,
           params_checksum_inv1 = params_checksum) %>%
    select(-c(inventory1, inventory2, inventory_id))

  inv2_check3 <- inventory_check3_detail %>%
    filter(inventory_id == "inv2") %>%
    rename(is_exported_inv2 = is_exported,
           obj_type_inv2 = obj_type,
           obect_chr_call_inv2 = object_chr_call,
           obj_checksum_inv2 = obj_checksum,
           fun_type_inv2 = fun_type,
           params_checksum_inv2 = params_checksum) %>%
    select(-c(inventory1, inventory2, inventory_id))

  inventory_check3_report <- inventory_check3_meta %>%
    left_join(., inv1_check3, by = c("package_name", "object_name")) %>%
    left_join(., inv2_check3, by = c("package_name", "object_name")) %>%
    mutate(obj_inv1 = ifelse(is.na(obj_checksum_inv1), "not in inv1", "in inv1"),
           obj_inv2 = ifelse(is.na(obj_checksum_inv2), "not in inv2", "in inv2")) %>%
    mutate(obj_same = ifelse(obj_checksum_inv1 == obj_checksum_inv2, T, F),
           params_same = ifelse(params_checksum_inv1 == params_checksum_inv2, T, F)) %>%
    mutate(object_presence =
             purrr::map2_chr(.x = obj_inv1, .y = obj_inv2,
                             .f = function(obj_inv1, obj_inv2){
                               classification = NA
                               classification <-
                                 if(obj_inv1 == "not in inv1" & obj_inv2 == "in inv2"){
                                   "Object in Inventory 2 but not Inventory 1"}
                               else if(obj_inv2 == "not in inv2" & obj_inv1 == "in inv1"){
                                 "Object in Inventory 1 but not Inventory 2"}
                               else if(obj_inv1 == "in inv1" & obj_inv2 == "in inv2"){
                                 "Object in both Inventories"}
                             }),
           object_changes =
             purrr::pmap_chr(list(obj_same,params_same, object_presence),
                             .f = function(obj_same, params_same, object_presence){
                               classification = ifelse(object_presence ==
                                                         "Object in both Inventories", NA, "Object dropped or added")

                               obj_and_param <- paste0(obj_same, params_same)
                               classification <-
                                 if(obj_and_param == "TRUEFALSE" & is.na(classification)){
                                   "Object unchanged but parameters changed"}
                               else if(obj_and_param == "FALSETRUE" & is.na(classification)){
                                 "Objects changed but parameters unchanged"}
                               else if(obj_and_param == "FALSEFALSE" & is.na(classification)){
                                 "Both object and parameters changed. Object change may be due to parameter changes."}
                               else {classification}
                             }))

  return_table <- inventory_check3_report %>%
    select(package_name, object_name,
           object_presence, object_changes,
           obj_same, params_same) %>%
    ungroup()


  return_summary_info <- return_table %>%
    group_by(package_name) %>% summarise(n_object_changes = n(), n_param_changes = sum(params_same == F, na.rm = T))

  if(nrow(return_table) > 0){
    print_info <- ""
    for(i in 1:nrow(return_summary_info)){
      current_package <- return_summary_info$package_name[i]
      current_package_changes <- return_summary_info$n_object_changes[i]
      functions <- ""
      params <- ""
      package_objects_changed <- for(j in 1:current_package_changes){
        func <- return_table[return_table$package_name == current_package,]$object_name[j]
        param <- return_table[return_table$package_name == current_package,]$params_same[j]
        param_text <- ifelse((param == T | is.na(param)), " ", "**")
        functions <- paste0(functions, "\n", j, ". ", func, param_text)
      }
      to_print <- paste0("\nThe '", current_package,
                         "' package has ",
                         return_summary_info$n_object_changes[i],
                         " object differences between the two inventories. Differences were identified in the following objects (** denotes parameter changes): \n",
                         functions, " \n")
      print_info <- paste0(print_info, to_print)

    }}

  if(nrow(return_table) == 0) {print_info <- "No changes detected between inventories."}

  ## here I want to add a thing that pulls the right objects from the inventories.
  inventory1_include <- inventory1 %>%
    filter(package_name %in% return_table$package_name) %>%
    unnest(package_objects) %>%
    filter(object_name %in% return_table$object_name) %>%
    select(package_name, object_name, package_source, package_version,
           package_gitrepo, package_gitcommit,
           function_text, fun_params,
           obj_checksum, params_checksum) %>%
    rename(package_source_inv1 = package_source,
           package_version_inv1 = package_version,
           package_gitrepo_inv1 = package_gitrepo,
           package_gitcommit_inv1 = package_gitcommit,
           function_text_inv1 = function_text,
           fun_params_inv1 = fun_params,
           obj_checksum_inv1 = obj_checksum,
           params_checksum_inv1 = params_checksum)

  inventory2_include <- inventory2 %>%
    filter(package_name %in% return_table$package_name) %>%
    unnest(package_objects) %>%
    filter(object_name %in% return_table$object_name) %>%
    select(package_name, object_name, package_source, package_version,
           package_gitrepo, package_gitcommit,
           function_text, fun_params,
           obj_checksum, params_checksum) %>%
    rename(package_source_inv2 = package_source,
           package_version_inv2 = package_version,
           package_gitrepo_inv2 = package_gitrepo,
           package_gitcommit_inv2 = package_gitcommit,
           function_text_inv2 = function_text,
           fun_params_inv2 = fun_params,
           obj_checksum_inv2 = obj_checksum,
           params_checksum_inv2 = params_checksum)

  inventory_diff_objects <- return_table %>%
    left_join(., inventory1_include, by = c("package_name", "object_name")) %>%
    left_join(., inventory2_include, by = c("package_name", "object_name"))


  if("summary" %in% report_type) {cat(print_info, file = summary_file)}

  return_list <- list(table = return_table, objects = inventory_diff_objects)
  report_types <- report_type[report_type != "summary"]
  return(return_list[report_types])

}



#' Check an R script for presence of affected object changes
#'
#' @param compare_object Comparison object of two inventories as returned by `compare_inventory`.
#' @param script_filepath Filepath to a script of interest.
#' @param is_R_script Logical. Default is TRUE, which assumes the script is a .R file. If FALSE, applies `knitr::purl` to convert to a .R script prior to parsing.
#' @param include_specials Logical. Whether to include special characters as objects when searching script.
#' @param summary_file Character. Default is `""`, which prints resulting summary to the console. If a filepath to a .txt file is provided, summary will save to the file instead.
#'
#' @return Concatenated summary of whether script involves any affected objects from the inventory comparison object.
#' @importFrom utils getParseData
#' @importFrom tidyr nest
#' @export
#'
#' @examples \dontrun{script_check(compare_object, script_filepath, summary_file = "script-check-results.txt")}
script_check <- function(compare_object, script_filepath, is_R_script = T, include_specials = F, summary_file = ""){
  object_diffs <- compare_object$table$object_name
  if(is_R_script == T) {parse_info <- getParseData(parse(script_filepath))}
  else if(is_R_script == F) {parse_info <- getParseData(parse(knitr::purl(script_filepath)))}
  diff_table <- parse_info %>%
    filter(text %in% object_diffs) %>%
    select(line1, token, text) %>%
    group_by(text) %>%
    mutate(n = n()) %>%
    nest(data = c("line1"))

  if(include_specials == F) {diff_table <- diff_table %>% filter(token != "SPECIAL")}

  if(nrow(diff_table) > 0){
    print_info <- ""
    for(i in 1:nrow(diff_table)){
      current_diff <- diff_table$text[i]
      current_diff_lines <- nrow(diff_table[i,] %>% unnest(data))
      lines <- ""
      package_objects_changed <- for(j in 1:current_diff_lines){
        line_data <- diff_table %>% filter(text == current_diff) %>% unnest(data) %>% pull(line1)
        line <- line_data[j]
        lines <- paste0(lines, "\n", line)
      }
      to_print <- paste0("\nThe object '", current_diff,
                         "' differs between inventory1 and inventory2, and appears in the provided script file ",
                         current_diff_lines,
                         " time(s), on the following line(s): \n",
                         lines, " \n")
      print_info <- paste0(print_info, to_print)

    }}

  if(nrow(diff_table) == 0) {print_info <- "No instances of changed objects in provided script file."}

  return(cat(print_info, file = summary_file))
}

