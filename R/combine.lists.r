combine.lists <- function(list1, list2)
{
  # Combine lists 'list1' and 'list2', giving precedence to elements found in 'list2':
  # that is, if $something is found in both 'list1' and 'list2',
  # the new (output) list will have the same values as 'list2' in $something

  # Version 1.0 (August 2017)
  #
  # Function developed by
  # Patrick Belisle
  # Division of Clinical Epidemiology
  # McGill University Hospital Center
  # Montreal, Qc, Can
  #
  # patrick.belisle@rimuhc.ca
  # http://www.medicine.mcgill.ca/epidemiology/Joseph/PBelisle/BetaParmsFromQuantiles.html


  list1.names <- names(list1)
  list2.names <- names(list2)

  new.list <- list1


  tmp <- match(list2.names, list1.names)
  w <- which(!is.na(tmp))

  if (length(w) > 0)
  {
    # take values from list2 in matching dimension names
    tmp <- tmp[!is.na(tmp)]
    new.list[[tmp]] <- list2[[w]]

    # append elements of 'list2' with unmatched names
    new.list <- c(new.list, list2[-w])
  }
  else
  {
    new.list <- c(new.list, list2)
  }

  new.list
} # end of combine.lists
