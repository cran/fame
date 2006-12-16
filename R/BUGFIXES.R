format.default <- function(x, ...){
  ## BUGFIX: don't lose attributes of x
  res <- get("format.default", pos = "package:base")(x, ...)
  attributes(res) <- attributes(x)
  res
}
assign <- function (x, value, pos = -1, envir = as.environment(pos),
                    inherits = FALSE, immediate = TRUE){
  ## BUGFIX: fail gracefully if envir = NULL, since ess-mode in Emacs does it
  if(is.null(envir)) return(NULL)
  .Internal(assign(x, value, envir, inherits))
}

tk_select.list <- function (list, preselect = NULL, multiple = FALSE, title = NULL){
  ## BUGFIX: send tklistbox width = 0 to make the listbox just wide enough to
  ## hold the list items
  lvar <- tclVar()
  tclObj(lvar) <- list
  dlg <- tktoplevel()
  tkwm.title(dlg, title)
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  if (!is.null(title) && nchar(title)) {
    lab <- tklabel(dlg, text = title, fg = "blue")
    tkpack(lab, side = "top")
  }
  onOK <- function() {
    res <- 1 + as.integer(tkcurselection(box))
    ans.select_list <<- list[res]
    tkgrab.release(dlg)
    tkdestroy(dlg)
  }
  onCancel <- function() {
    tkgrab.release(dlg)
    tkdestroy(dlg)
  }
  buttons <- tkframe(dlg)
  tkpack(buttons, side = "bottom")
  OK <- tkbutton(buttons, text = "OK", width = 6, command = onOK)
  Cancel <- tkbutton(buttons, text = "Cancel", command = onCancel)
  tkpack(OK, Cancel, side = "left", fill = "x", padx = "2m")
  scht <- as.numeric(tclvalue(tkwinfo("screenheight", dlg))) - 
    200
  ht <- min(length(list), scht%/%20)
  box <- tklistbox(dlg, height = ht, listvariable = lvar, bg = "white", 
                   selectmode = ifelse(multiple, "multiple", "single"))
  tmp <- tcl("font", "metrics", tkcget(box, font = NULL))
  tmp <- as.numeric(sub(".*linespace ([0-9]+) .*", "\\1", tclvalue(tmp))) + 3
  ht <- min(length(list), scht%/%tmp)
  tkdestroy(box)
  if(ht < length(list)){
    scr <- tkscrollbar(dlg, repeatinterval = 5, command = function(...) tkyview(box, 
                                                  ...))
    box <- tklistbox(dlg,
                     height = ht,
                     width = 0,
                     listvariable = lvar, 
                     bg = "white",
                     selectmode = ifelse(multiple, "multiple", "single"),
                     yscrollcommand = function(...) tkset(scr, ...))
    tkpack(box, side = "left", fill = "both", expand = TRUE)
    tkpack(scr, side = "right", fill = "y")
  }
  else {
    box <- tklistbox(dlg, height = ht, width = 0, listvariable = lvar, 
                     bg = "white", selectmode = ifelse(multiple, "multiple", 
                                     "single"))
    tkpack(box, side = "left", fill = "both")
  }
  preselect <- match(preselect, list)
  ans.select_list <- character(0)
  for (i in preselect[preselect > 0]) tkselection.set(box, 
                                                      i - 1)
  tkbind(dlg, "<Destroy>", onCancel)
  tkfocus(box)
  tkwait.window(dlg)
  if (!multiple && !length(ans.select_list)) 
    ans.select_list <- ""
  ans.select_list
}
