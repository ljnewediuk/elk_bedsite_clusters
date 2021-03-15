# bedsite_algorithm

This code can be adapted to predict the identity of hair and fecal pellet 
samples collected from suspected GPS-collared individuals based on their 
movement characteristics and some site-level information. The movement
characteristics are taken from 4-hour clusters of GPS locations surrounding
the location point closest to the sample. Predictors are the distance between
the cluster "centre" and the sample, the number of points within a buffer 
surrounding the cluster centre, the nearest neighbour distance among points
in the cluster, and the number of bed sites within 20 m of the sample.