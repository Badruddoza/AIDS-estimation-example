# Example

data( Blanciforti86 )
# Data on food consumption are available only for the first 32 years
Blanciforti86 <- Blanciforti86[ 1:32, ]
bestA0 <- aidsBestA0( c( "pFood1", "pFood2", "pFood3", "pFood4" ),
c( "wFood1", "wFood2", "wFood3", "wFood4" ), "xFood",
data = Blanciforti86, useMatrix = FALSE )
# may take some time (argument 'useMatrix = FALSE' decreases
# the computation time only if the model and data set are small)
print( bestA0$alpha0 )
plot( bestA0$allValues ) # this should be convex


data( Blanciforti86 )
# Data on food consumption are available only for the first 32 years
Blanciforti86 <- Blanciforti86[ 1:32, ]
priceNames <- c( "pFood1", "pFood2", "pFood3", "pFood4" )
shareNames <- c( "wFood1", "wFood2", "wFood3", "wFood4" )
## LA-AIDS
estResult <- aidsEst( priceNames, shareNames, "xFood",
data = Blanciforti86, priceIndex = "S" )
# using observed shares in the Stone index
lnp <- aidsPx( "S", priceNames, Blanciforti86, shareNames )
fitted <- aidsCalc( priceNames, "xFood", coef = coef( estResult ),
data = Blanciforti86, priceIndex = lnp )
fitted$shares # equal to estResult$wFitted
fitted$quant # equal to estResult$qFitted
# now the same with the predict method
fitted2 <- predict( estResult, observedShares = TRUE )
all.equal( fitted, fitted2 )
# using fitted shares in the Stone index
fitted <- aidsCalc( priceNames, "xFood", coef = estResult$coef,
data = Blanciforti86, priceIndex = "S" )
# now the same with the predict method
fitted2 <- predict( estResult )
all.equal( fitted, fitted2 )
## AIDS
estResult <- aidsEst( priceNames, shareNames, "xFood",
data = Blanciforti86, method = "IL" )
fitted <- aidsCalc( priceNames, "xFood", coef = coef( estResult ),
data = Blanciforti86 )
fitted$shares # equal to estResult$wFitted
fitted$quant # equal to estResult$qFitted
fitted2 <- predict( estResult )
all.equal( fitted, fitted2 )


data( Blanciforti86 )
# Data on food consumption are available only for the first 32 years
Blanciforti86 <- Blanciforti86[ 1:32, ]
priceNames <- c( "pFood1", "pFood2", "pFood3", "pFood4" )
shareNames <- c( "wFood1", "wFood2", "wFood3", "wFood4" )
# estimate the AIDS
estResult <- aidsEst( priceNames, shareNames, "xFood",
data = Blanciforti86, method = "IL" )
# check concavity with fitted shares
aidsConcav( priceNames, "xFood", coef = estResult$coef,
data = Blanciforti86 )
# check concavity with observed shares
aidsConcav( priceNames, "xFood", coef = estResult$coef,
data = Blanciforti86, shareNames = shareNames )

## Check consistency
data( Blanciforti86 )
# Data on food consumption are available only for the first 32 years
Blanciforti86 <- Blanciforti86[ 1:32, ]
priceNames <- c( "pFood1", "pFood2", "pFood3", "pFood4" )
shareNames <- c( "wFood1", "wFood2", "wFood3", "wFood4" )
estResult <- aidsEst( priceNames, shareNames, "xFood",
data = Blanciforti86, method = "IL" )
aidsConsist( priceNames, "xFood", data = Blanciforti86,
coef = estResult$coef )
# the same can be obtained in an easier way
checkConsist( estResult )

## Elasticities
data( Blanciforti86 )
# Data on food consumption are available only for the first 32 years
Blanciforti86 <- Blanciforti86[ 1:32, ]
estResult <- aidsEst( c( "pFood1", "pFood2", "pFood3", "pFood4" ),
c( "wFood1", "wFood2", "wFood3", "wFood4" ), "xFood",
data = Blanciforti86 )
wMeans <- colMeans( Blanciforti86[ , c( "wFood1", "wFood2",
"wFood3", "wFood4" ) ] )
aidsElas( estResult$coef, shares = wMeans, method = "Ch",
priceIndex = "S" )
## Repeating the evaluation of different elasticity formulas of
## Green & Alston (1990)
priceNames <- c( "pFood1", "pFood2", "pFood3", "pFood4" )
shareNames <- c( "wFood1", "wFood2", "wFood3", "wFood4" )
# AIDS estimation and elasticities
estResultA <- aidsEst( priceNames, shareNames, "xFood",
data = Blanciforti86[ -1, ],
method = "IL", maxiter = 100 )
diag( elas( estResultA, method = "AIDS" )$marshall )
summary( elas( estResultA, method = "AIDS" ) )

# LA-AIDS estimation
estResultLA <- aidsEst( priceNames, shareNames, "xFood",
data = Blanciforti86, priceIndex = "SL", maxiter = 100 )
# LA-AIDS + formula of AIDS
diag( elas( estResultLA, method = "AIDS" )$marshall )
# LA-AIDS + formula of Eales + Unnevehr
diag( elas( estResultLA, method = "EU" )$marshall )
# LA-AIDS + formula of Goddard or Chalfant:
diag( elas( estResultLA, method = "Go" )$marshall )
diag( elas( estResultLA, method = "Ch" )$marshall )
# LA-AIDS + formula of Green + Alston (= 1st of Buse):
diag( elas( estResultLA, method = "GA" )$marshall )

# Using data published in Blanciforti, Green & King (1986)
data( Blanciforti86 )
# Data on food consumption are available only for the first 32 years
Blanciforti86 <- Blanciforti86[ 1:32, ]
## Repeating the demand analysis of Blanciforti, Green & King (1986)
## Note: Blanciforti, Green & King (1986) use scaled data,
## which leads to slightly different results
estResult <- aidsEst( c( "pFood1", "pFood2", "pFood3", "pFood4" ),
c( "wFood1", "wFood2", "wFood3", "wFood4" ), "xFood",
data = Blanciforti86, priceIndex = "SL", maxiter = 100 )
print( estResult )
elas( estResult )
## Estimations with a demand shifter: linear trend
priceNames <- c( "pFood1", "pFood2", "pFood3", "pFood4" )
shareNames <- c( "wFood1", "wFood2", "wFood3", "wFood4" )
Blanciforti86$trend <- c( 0:( nrow( Blanciforti86 ) - 1 ) )
estResult <- aidsEst( priceNames, shareNames, "xFood",
data = Blanciforti86, shifterNames = "trend" )
print( estResult )
# Estimations with two demand shifters: linear + quadratic trend
Blanciforti86$trend2 <- c( 0:( nrow( Blanciforti86 ) - 1 ) )^2
estResult <- aidsEst( priceNames, shareNames, "xFood",
data = Blanciforti86, shifterNames = c( "trend", "trend2" ) )
print( estResult )
