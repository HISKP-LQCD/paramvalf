devtools::load_all()

bootstrap_param <- list(param = data.frame(boot_l = c(1, 2, 4)))
matrixfit_model <- list(param = data.frame(model = c('shifted', 'weighted')))

.func <- function (param, value) {
    list(foo = 2 * param$boot_l,
         bar = param$model)
}
pv1 <- pvcall(.func, bootstrap_param, matrixfit_model)
pv_save('.', pv1)

.func <- function (param, value) {
    list(baz = sin(2 * param$boot_l))
}
pv2 <- pvcall(.func, bootstrap_param)
pv_save('.', pv2)

.func <- function (param, value) {
    list(res = value$foo + value$baz)
}
pv3 <- pvcall(.func, pv1, pv2)
pv_save('.', pv3)

print(pv3)
