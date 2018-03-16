# survival analysis competing methods
library(mlr)
library(survival)

makeRLearner.surv.coxph = function() {
    makeRLearnerSurv(
        cl = "surv.cforest",
        package = "survival",
        par.set = makeParamSet(
            makeDiscreteLearnerParam(id = "ties", default = "efron", values = c("efron", "breslow", "exact")),
            makeLogicalLearnerParam(id = "singular.ok", default = TRUE),
            makeNumericLearnerParam(id = "eps", default = 1e-09, lower = 0),
            makeNumericLearnerParam(id = "toler.chol", default = .Machine$double.eps^0.75, lower = 0),
            makeIntegerLearnerParam(id = "iter.max", default = 20L, lower = 1L),
            makeNumericLearnerParam(id = "toler.inf", default = sqrt(.Machine$double.eps^0.75), lower = 0),
            makeIntegerLearnerParam(id = "outer.max", default = 10L, lower = 1L),
            makeLogicalLearnerParam(id = "model", default = FALSE, tunable = FALSE),
            makeLogicalLearnerParam(id = "x", default = FALSE, tunable = FALSE),
            makeLogicalLearnerParam(id = "y", default = TRUE, tunable = FALSE)
        ),
        properties = c("missings", "numerics", "factors", "weights", "prob", "rcens"),
        name = "Cox Proportional Hazard Model",
        short.name = "coxph",
        note = ""
    )
}

trainLearner.surv.coxph = function(.learner, .task, .subset, .weights = NULL, ...) {
    f = getTaskFormula(.task)
    data = getTaskData(.task, subset = .subset)
    if (is.null(.weights)) {
        survival::coxph(formula = f, data = data, ...)
    } else {
        survival::coxph(formula = f, data = data, weights = .weights, ...)
    }
}

data(lung, package = "survival")
lung$status = (lung$status == 2) # convert to logical
surv.task = makeSurvTask(data = lung, target = c("time", "status"))
surv.task

# get the info about the learning task
getTaskDesc(surv.task)
getTaskId(classif.task)
getTaskType(classif.task)
getTaskSize(classif.task)
getTaskNFeats(classif.task)

# modify the learning task
## Select observations and/or features
cluster.task = subsetTask(cluster.task, subset = 4:17)
removeConstantFeatures(cluster.task)
dropFeatures(surv.task, c("meal.cal", "wt.loss"))

# overview of a task data
summary(getTaskData(surv.task))

# make learner
surv.lrn = makeLearner("surv.cforest", id = "cph")

# get the hyperparameters
surv.lrn$par.vals

# training
mod = train(surv.lrn, surv.task)
mod

# predicting
task.pred = predict(mod, task = surv.task)
task.pred
