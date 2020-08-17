#' ---
#' title: JuliaCall and JellyMe4
#' author: Phillip M. Alday
#' date: 12 May 2020
#' ---

#' This is an extension of the work [here in
#' R](https://github.com/palday/jlme/blob/master/R/hello.R), which later
#' transformed into the [JellyMe4 package in
#' Julia](https://github.com/palday/JellyMe4.jl).

if (FALSE) {
    library("JuliaCall")
    library("lme4")

    julia_setup()  # this can take a while
    
    julia_install_package_if_needed("MixedModels")
    julia_install_package_if_needed("JellyMe4")

    julia_library("MixedModels")
    julia_library("RCall")
    julia_library("JellyMe4")

#' if we want to use JuliaCall directly, then this what we would do (but if
#' you're skilled enough to write this code out yourself, I would recommend just
#' using RCall and then using`$` to swap to an R REPL as needed)
julia_assign("insteval", InstEval)  # copy data.frame to Julia
julia_command("@time fm1 = fit!(LinearMixedModel(@formula(y ~ 1 + service + (1|s) + (1|d) + (1|dept)), insteval),REML=false)")
#' refit for timing.  First Julia fit is slower due to Just-In-Time (JIT)
#' compilation
system.time(julia_command("@time fm1 = fit!(LinearMixedModel(@formula(y ~ 1 + service + (1|s) + (1|d) + (1|dept)), insteval));"))

#' I'm using JellyMe4's infrastructure here. If you're using RCall from within
#' Julia, then you would use `@rput` (see the JellyMe4 README for me info)
#' instead of an explicit call to `robject`. For reasons I don't understand at
#' the moment, `@rput` isn't playing nice with JuliaCall, at least on Julia 1.4.
#'
#' note that there is some overhead in moving the data back and forth across
#' languages, which JellyMe4 does.
jm1 <- julia_eval("robject(:lmerMod, Tuple([fm1,insteval]));",need_return="R")

#' You could also extract the theta vector and use that as the starting value in
#' R, which saves scooting data back and forth, but then you're responsible for
#' all the bookkeeping. (Incidentally, it's the bookkeeping that's currently
#' limiting GLMM support.)
system.time(fm1 <- lmer(y ~ 1 + service + (1|s) + (1|d) + (1|dept), InstEval,
                        REML=FALSE,
                        control=lmerControl(calc.derivs=FALSE,
                                            optCtrl=list(start=julia_eval("fm1.theta"),
                                                         maxeval=1))))


class(fm1)
class(jm1)

summary(fm1)
summary(jm1)
}

# NB: you can also start the Julia console from within R with julia_console(),
# but none of the special REPL modes (RCall, Pkg, Shell) will work

## note that this will *not* preserve contrast coding at the time of writing
## don't deal with/export this until we know what to do with it
jmer2 <- function(formula, data, REML=TRUE){
    # to simplify maintainence here (in the hopes of turning this into a real
    # package), I'm depending on JellyMe4, which copies the dataframe back with
    # the model this is of course what you want if you're primarily working in
    # Julia and just using RCall for the the R ecosystem of extras for
    # MixedModels, but it does create an unnecessary copy if you're starting
    # with your data in R.
    #
    # Also, this means we suffer/benefit from the same level of compatibility in
    # the formula as in JellyMe4, i.e. currently no support for the ||

    jf <- deparse(formula)
    jreml = ifelse(REML, "true", "false")

    julia_assign("jmerdat",data)
    julia_command(sprintf("jmermod = fit!(LinearMixedModel(@formula(%s),jmerdat),REML=%s);",jf,jreml))

    julia_eval("robject(:lmerMod, Tuple([jmermod,jmerdat]));",need_return="R")
}

#' example based on https://stats.stackexchange.com/questions/465699/analyzing-a-partially-crossed-design/465864#465864
if (FALSE) {
dat <- read.csv("https://raw.githubusercontent.com/ilzl/i/master/d.csv")
dat$item_type <- factor(dat$item_type)
dat$gender <- factor(dat$gender)
dat$person_id <- factor(dat$person_id)
dat$item_id <- factor(dat$item_id)

system.time(fm2 <- lmer(y ~ 1 + item_type + (1|item_id) + (1|person_id), dat, REML=FALSE,control=lmerControl(calc.derivs=FALSE)))
system.time(jm2 <- jmer(y ~ 1 + item_type + (1|item_id) + (1|person_id), dat, REML=FALSE))

summary(fm2)
summary(jm2)

fm2@optinfo
jm2@optinfo
}
