#' Selecting the colour display of the nodes
#' @export
#' @author Aiden Loe

# setwd("~/Dropbox/Cambridge/PhD/ICAR/AIG/MAP")


# Create class of object
# select colour_display
colour_display <- function(base.colour, start.colour, end.colour)
{
    cbPalette <- list(palette=c("#000000","#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
	#base colour
	chosen = cbPalette$palette[base.colour]
	#starting colour
	start.node <- cbPalette$palette[start.colour]
	#ending colour
    end.node <- cbPalette$palette[end.colour]
    # put all in list
    colours = list(chosen = chosen,
    	start.node = start.node,
    	end.node = end.node)
    #Class attribute
    class(colours)=append(class(colours), "colours")
    # Allow values to be assigned to object.
    invisible(colours)
}

# Method to read class.
#Print method for colour class
print.colours <- function(object)
{
	print(object$chosen)
	print(object$start.node)
	print(object$end.node)
}





