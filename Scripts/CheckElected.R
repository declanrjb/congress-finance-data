rownames(nodes_senators) <- seq(length=nrow(nodes_senators))
elected_congress$Name <- paste(elected_congress$First,elected_congress$Last)
nodes_senators$Name <- paste(nodes_senators$firstName,nodes_senators$lastName)


for (i in 1:length(nodes_senators$Name)) {
  nodes_senators$firstName[i] <- (strsplit(nodes_senators$Name," ")[[i]])[1]
  nodes_senators$lastName[i] <- (strsplit(nodes_senators$Name," ")[[i]])[length((strsplit(nodes_senators$Name," ")[[i]]))]
}

for (i in 1:length(nodes_senators$Name)) {
  if (nodes_senators$Name[i] %in% elected_congress$Name) {
    nodes_senators$Elected[i] <- TRUE
  } else {
    if (is.na(nodes_senators$Elected[i])) {
      nodes_senators$Elected[i] <- FALSE
    }
  }
}