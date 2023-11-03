### variable selection for loss and damage indicators
 
# import
merged_full <- read.csv("merged_full.csv")


# filtering out countries and indicators with more NAs
reg_data_countries <- merged_full %>% filter(!(country.name %in% c("Taiwan", 
"Venezuela", "Yemen",
"South Sudan")))
## loss indic - Non.economic.loss.and.damage (binary - non_econ_loss) & 
## Loss.and.damage.mentioned (binary - loss_damage)

## costs - Current.economic.loss.and.damage.figures (costs) & 
## Future.economic.loss.and.damage.figures (future_costs)

## LOSS -- BINARY ----------
reg_data_ld <- reg_data_countries %>% select(!c(fuel.pct, prev.leader, reelect, 
                                                turnover_a,
                                                Identification.of.children.as.a.vulnerable.group,
                                                Identification.of.young.people.as.a.vulnerable.group,
                                                Country.x,
                                                Country.y,
                                                Loss.and.damage.mentioned,
                                                Finan_Needs,
                                                Financial.needs.for.implementation,
                                                Other.non.financial.support.needs,
                                                Vul_Counts,
                                                Loss_Counts,
                                                Non.economic.loss.and.damage,
                                                country.code,
                                                Future.economic.loss.and.damage.figures,
                                                Current.economic.loss.and.damage.figures,
                                                Loss.and.damage.mentioned,
                                                Economic.loss.and.damage,
                                                year,
                                                country.name,
                                                Loss_Counts,
                                                Vul_Counts))
reg_data_ld <- reg_data %>% select(-c(X.1, Future.economic.loss.and.damage.figures,
                                   Current.economic.loss.and.damage.figures,
                                   Loss.and.damage.mentioned,
                                   Loss_Counts,
                                   Vul_Counts,
                                   Economic.loss.and.damage))

reg_loss <- na.omit(reg_data_ld)
reg_full_loss <- lm(loss_damage ~ ., 
                    data = reg_loss)
reg_null_loss <- lm(loss_damage ~ 1, data = reg_loss)

step_out_loss <- step(reg_null_loss, 
                     scope = list(lower = reg_null_loss, upper = reg_full_loss),
                     method = "forward")
summary(step_out_loss)



reg_l <- lm(loss_damage ~ ., data = reg_loss) 
x <- model.matrix(reg_l)
dim(x)
y <- reg_loss$loss_damage

set.seed(123)
lr_cv_l <- cv.glmnet(x, y)

plot(lr_cv_l)

coef(lr_cv_l, s = "lambda.min")

## NON ECON LOSS -- BINARY ----------


reg_loss <- na.omit(reg_data_ld)
reg_full_nonecon <- lm(non_econ_loss ~ ., 
                    data = reg_loss)
reg_null_nonecon <- lm(non_econ_loss ~ 1, data = reg_loss)

step_out_nonecon <- step(reg_null_nonecon, 
                      scope = list(lower = reg_null_nonecon, upper = reg_full_nonecon),
                      method = "forward")
summary(step_out_nonecon)



reg_non <- lm(non_econ_loss ~ ., data = reg_loss) 
x <- model.matrix(reg_non)
dim(x)
y <- reg_loss$non_econ_loss

set.seed(123)
lr_cv_non <- cv.glmnet(x, y)

plot(lr_cv_non)

coef(lr_cv_non)