library(origami)
library(data.table)
context("Folds")

############################
# generic test should be applied to all generated fold vectors
# make sure training and validation sets don't overlap
find_overlap=function(fold){
	training_idx=training()
	validation_idx=validation()
	overlap=intersect(training_idx,validation_idx)
	list(overlap=overlap)
}

test_splits=function(folds){
	overlaps=cross_validate(find_overlap,folds)
	all_overlaps=unlist(overlaps)
	test_that("Training and Validation don't overlap",expect_length(all_overlaps,0))
}

############################
# generate v-fold fold vector
n=1000
folds=make_folds(n=n,fold_fun=folds_vfold)

test_splits(folds)

# make sure v fold validation sets are exhaustive, mutually exclusive
get_validation_sets=function(fold){
	list(fold_data=data.table(validation=validation(),fold=fold_index()))
}

validation_sets=cross_validate(get_validation_sets,folds)
validation_dt=rbindlist(validation_sets)
all_indicies=seq_len(n)
test_that("V-fold validation sets are exhaustive",expect_equivalent(sort(validation_dt$validation),all_indicies))

max_index_count=max(table(validation_dt$validation))
test_that("V-fold validation sets are mutually exclusive",expect_equal(max_index_count,1))

############################
# make sure ids all get put in the same validation set
# generate 100 subjects, each with n/100 replicates
ids=sample(seq_len(100),n,replace=T)

id_folds=make_folds(fold_fun=folds_vfold,cluster_id=ids)
test_splits(id_folds)

get_validation_sets_ids=function(fold,ids){
	list(fold_data=data.table(validation=validation(),fold=fold_index(),id=validation(ids)))
}

validation_sets=cross_validate(get_validation_sets_ids,id_folds,ids)
validation_dt=rbindlist(validation_sets)

idtab=table(validation_dt$id,validation_dt$fold)
fold_counts=rowSums(idtab>0)
max_fold_count=max(fold_counts)
test_that("Each ID only appears in one fold",expect_equal(max_fold_count,1))

############################
# make sure folds are roughly balanced in strata
# generate two strata, one much more rare than the other
set.seed(1)
strata_ids=rbinom(n,1,0.05)

strata_folds=make_folds(fold_fun=folds_vfold,strata_id=strata_ids)
nfolds=length(strata_folds)
test_splits(strata_folds)


validation_sets=cross_validate(get_validation_sets_ids,strata_folds,strata_ids)
validation_dt=rbindlist(validation_sets)
one_counts=validation_dt[,list(one_count=sum(strata_ids)),by=list(fold)]
count_range=diff(range(one_counts$one_count))
test_that("Strata are roughly balanced",expect_lte(count_range,nfolds))


############################
# Verify quoted fold scheme names work
set.seed(1)
folds=make_folds(n,fold_fun="vfold")
test_splits(folds)

set.seed(1)
folds2=make_folds(n,fold_fun=folds_vfold)
test_that("Quoted fold function names work",expect_equal(folds,folds2))

############################
# Verify quoted fold_fun names work
