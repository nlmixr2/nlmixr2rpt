# Implementing the model found here:
# http://doi.org/10.4161/mabs.29095
my_model <- function() {
ini({
   # Typical Value of System Parameters 
   TV_F1           = fixed(0.744)
   TV_ka           =   log(c(.01, 0.282, 3))
   TV_CL           =   log(c(.01, 0.200, 2))
   TV_Vc           =   log(c(.01, 3.61,  10))
   TV_Vp           =   log(c(.01, 2.75,  10))
   TV_Q            =   log(c(.01, 0.747, 3))
   TV_MW           = fixed(140)

   ETAka ~ .416
   ETACL ~ .098
   ETAVc ~ .116
   ETAVp ~ .079
   ETAQ  ~ .699

   # Error model parameters
   prop_err     =  c(.01, 0.1, 2)
   add_err      =  c(.01, 0.1, 2)

})
model({ 
   # System Parameters 
   F1          = TV_F1
   ka          = exp(TV_ka + ETAka)
   CL          = exp(TV_CL + ETACL)
   Vc          = exp(TV_Vc + ETAVc)
   Vp          = exp(TV_Vp + ETAVp)
   Cp          = Ap/(Vp)
   Cc          = Ac/Vc
   Q           = exp(TV_Q  + ETAQ)
   MW          = TV_MW

   # Static Secondary Parameters 
   WTTV        = 70 
   CL_IND      = CL         *(1.0+SEX_ID*.01)*(1.0+SUBTYPE_ID*.08) 
   kel         = CL_IND/Vc  *((WT/WTTV))^(-0.25) 
   kcp         = Q/Vc       *((WT/WTTV))^(-0.25) 
   kpc         = Q/Vp       *((WT/WTTV))^(-0.25) 



   # Defining ODEs
   d/dt(At)    = (-ka*At)
   d/dt(Ac)    = ((ka*At*F1/Vc  -kel*Cc - kcp*Cc  + kpc*Cp*Vp/Vc))*Vc
   d/dt(Ap)    = ((kcp*Cc*Vc/Vp - kpc*Cp))*Vp

   # Outputs and error models
   C_ng_ml     = Cc/1000
   C_ng_ml ~ add(add_err) + prop(prop_err)

})
}

