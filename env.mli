open Expression;;

exception Not_Unifiable

val _applyEnv : environment -> argument -> argument 

val applyEnv : environment -> clause -> clause 

val buildEnv : clause -> clause -> environment 
