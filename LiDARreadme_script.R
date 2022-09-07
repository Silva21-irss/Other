lidar_readme <- function(hub_site,data_origin,data_owner,contact,date,files,abbreviation){
  text_file <- paste0("##################### ",hub_site," LiDAR for Silva21 #####################\n\n",
                      "This dataset originates from ",data_origin,". It is now owned by ",data_owner,".\n",
                      "You may contact ",contact," for any LiDAR related inquiry.\n\n",
                      "The data was collected in ",date,".\nThe raw data files ",files,".")
  write(text_file,paste0(abbreviation,"_LiDAR_README.txt"))
}
