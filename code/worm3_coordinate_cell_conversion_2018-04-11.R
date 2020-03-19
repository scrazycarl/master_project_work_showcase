
# Simple script for converting WorM3 cell numbers to coordinates and vice versa

# Setting up table with cell numbers and their coordinates
worm3.cell_coordinate_table <- data.frame(
  CELL_NUMBER=1:288, 
  LONGITUDE_MIN=rep((0:23)*360/24-180, times=12), 
  LONGITUDE_MAX=rep((1:24)*360/24-180, times=12), 
  LATITUDE_MIN=rep((11:0)*180/12-90, each=24), 
  LATITUDE_MAX=rep((12:1)*180/12-90, each=24)
  )

# set up function for giving coordinates from cell number
worm3.coordinates_from_cell <- function(cell_number){
  return(unlist(worm3.cell_coordinate_table[match(cell_number, worm3.cell_coordinate_table$CELL_NUMBER),c("LONGITUDE_MIN", "LONGITUDE_MAX", "LATITUDE_MIN", "LATITUDE_MAX")]))
}

# test function for giving coordinates from cell number
worm3.coordinates_from_cell(cell_number = 24)
worm3.coordinates_from_cell(cell_number = 265)

# set up function for giving cell number for coordinates
worm3.cell_from_coordinates <- function(LONGITUDE, LATITUDE){
  for(cell_index in 1:nrow(worm3.cell_coordinate_table)){
    if(
      LONGITUDE >= worm3.cell_coordinate_table$LONGITUDE_MIN[cell_index]&
      LONGITUDE <= worm3.cell_coordinate_table$LONGITUDE_MAX[cell_index]&
      LATITUDE >= worm3.cell_coordinate_table$LATITUDE_MIN[cell_index]&
      LATITUDE <= worm3.cell_coordinate_table$LATITUDE_MAX[cell_index]
    ){
      return(worm3.cell_coordinate_table$CELL_NUMBER[cell_index])      
    }
  }; rm(cell_index)
  return(NA)
}

# test function for giving cell number for coordinates
worm3.cell_from_coordinates(LONGITUDE = 170, LATITUDE = 80)
worm3.cell_from_coordinates(LONGITUDE = -170, LATITUDE = -80)
worm3.cell_from_coordinates(LONGITUDE = 1000, LATITUDE = 13)


