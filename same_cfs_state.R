###################
#
# This script examines if shipment data contains within state and within cfs shipment.
#
##################3

# calculate number of shipment that is within the same CFS area
commodityflow$same_cfs = commodityflow$ORIG_CFS_AREA == commodityflow$DEST_CFS_AREA
sum(commodityflow$same_cfs)

# calculate number of shipment that is within the state
commodityflow$same_state = commodityflow$ORIG_STATE == commodityflow$DEST_STATE
sum(commodityflow$same_state)

nrow(commodityflow)

# Approximately 1/4 of shipment is within the same CFS area, 1/3 is within the same state
