# libraries
library(tidymodels)

# Create synthetic data
x <- seq(1, 12)
epsilon <- rnorm(12)
a <- 5
b <- 1.5

y <- a * x ^ b + epsilon

df <- data.frame(x, y)


# 
# Set up resampling
# 
set.seed(57)
kfold_number <- 5
kfold_repeats <- 1

splits <- vfold_cv(df, v = kfold_number, repeats = kfold_repeats)

# 
# Register new parsnip model based on nls()
# 
set_new_model("nonlinear_reg")
set_model_mode(model = "nonlinear_reg", mode = "regression")
set_model_engine(
  "nonlinear_reg", 
  mode = "regression", 
  eng = "nls"
)
set_dependency("nonlinear_reg", eng = "nls", pkg = "stats")

# 
# Create the model function and add fit method and encoding details
# 
nonlinear_reg <-
  function(mode = "regression",  start = NULL) {
    # Check for correct mode
    if (mode  != "regression") {
      rlang::abort("`mode` should be 'regression'")
    }
    
    # Capture the arguments in quosures
    args <- list(start = rlang::enquo(start))
    
    # Save some empty slots for future parts of the specification
    new_model_spec(
      "nonlinear_reg",
      args = args,
      mode = mode,
      engine = NULL,
      eng_args = NULL,
      method = NULL
    )
  }

set_fit(
  model = "nonlinear_reg",
  eng = "nls",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data"),
    func = c(pkg = "stats", fun = "nls"),
    defaults = list()
  )
)

set_encoding(
  model = "nonlinear_reg",
  eng = "nls",
  mode = "regression",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

# 
# Add modules for prediction
# 
response_info <- 
  list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      # These lists should be of the form:
      # {predict.nls argument name} = {values provided from parsnip objects}
      list(
        # We don't want the first two arguments evaluated right now
        # since they don't exist yet. 
        object = quote(object$fit),
        newdata = quote(new_data)
      )
  )

set_pred(
  model = "nonlinear_reg",
  eng = "nls",
  mode = "regression",
  type = "numeric",
  value = response_info
)

# 
# Show that the new nls based parsnip model works if we call fit with 
# entire data set or with some subset generated by vfold_cv()

# Need starting values for nls()
init_nls <- c(b1=1, b2=2)
nls_mod <- nonlinear_reg() %>% set_engine(engine = "nls", start = init_nls)

# Call fit with a specific model formula - a power function

nls_mod_formula <- y ~ b1 * (x ^ b2) 

nls_mod_fit <- 
  nls_mod %>% 
  fit(formula = nls_mod_formula, data = df)

nls_mod_fit

# Things also work with some arbitrary split

nls_mod_fit2 <- 
  nls_mod %>% 
  fit(formula = nls_mod_formula, data = as.data.frame(splits$splits[[1]]))

nls_mod_fit2


# 
# Now try to use the new parsnip model with resampling. It seems that
# tune::fit_resamples() does some formula checking that parsnip::fit()
# does not and tune::fit_resamples() does not like nonlinear models such as power functions
# in which one of the parameters to estimate is a power. I thought
# I might be able to use a recipe to get around this but the recipes
# package ends up with a similar error in that it thinks the formula
# uses inline functions (it does, it uses ^).
# 
# I can't see a way to bypass this formula checking in fit_resamples(). 
# The other alternative is to manually do the looping through the 
# splits object to fit models and assess performance with resampling.
# 

# Pipe our model through fit_resamples to see the error
nls_mod %>% 
  fit_resamples(preprocessor = nls_mod_formula, resamples = splits)

show_notes(.Last.tune.result)

# We would get same error if we used a workflow object
# nls_wflow <- 
#   workflow() %>% 
#   add_model(nls_mod) %>% 
#   add_formula(nls_mod_formula)
# 
# # Pipe our workflow through fit_resamples
# nls_wflow %>% 
#   fit_resamples(resamples = splits)


