save.image("D:/jdata/jdrecommend/commend.RData")
load("D:/jdata/jdrecommend/commend.RData")
library(data.table)
library(dplyr)
library(tidyr)
library(smbinning)
library(lubridate)
demo = fread('./data/demo.csv')
user = fread('./data/Jdata_User.csv')
action_4 = fread('./data/Jdata_Action_201604.csv')
action_3 = fread('./data/Jdata_Action_201603.csv')
action_3_extra = fread('./data/Jdata_Action_201603_extra.csv')
product = fread ('./data/Jdata_Product.csv')
comments = fread('./data/JData_Comment.csv')
desc_product=smbinning.eda(product)$eda
desc_user = smbinning.eda(user)$eda
desc_commet = smbinning.eda(comments)$eda
desc_action_4 = smbinning.eda(action_4)$eda
head(action_4)

#2016-04-16至2016-04-20这5天数据作为测试集
#线下2016-04-09至2016-04-13这5天数据作为验证集
#前一周 2016-03-01至2016-04-08这39天数据作为训练集

train_03 = rbind(action_3,action_3_extra) %>% mutate( day= substr(time,1,10)) %>% 
  filter(cate == 8) %>% arrange(user_id,time)

train_04 =  action_4 %>% mutate( day= substr(time,1,10)) %>% 
   filter(day>='2016-04-01'&day<='2016-04-08'&cate == 8) %>%
  arrange(user_id,time)

train_04_remain  = action_4 %>% mutate( day= substr(time,1,10)) %>% 
  filter(day>='2016-04-09'&cate == 8) %>%
  arrange(user_id,time)

train = rbind(train_03,train_04)

test =  action_4 %>% mutate( day= substr(time,1,10)) %>% 
  filter(day>='2016-04-09'&day<='2016-04-13'&cate == 8) %>%
  arrange(user_id,time)

transfrom = function(data){
  action_type = data %>% group_by(user_id,sku_id,brand,type) %>% 
    summarise(cnt = n()) %>% spread(type, cnt,fill = 0 )
  names(action_type)[4:9] = c('browse','add_trolley','delete_trolley',
                              'ordered','attention','click')
  action_type
}
train_type = transfrom(train)
test_type  = transfrom(test)

train_ordered = train_type[train_type$ordered>0,]
test_ordered   = test_type[test_type$ordered>0,]
train_test = train_type %>% full_join(test_ordered,by=c('user_id','sku_id','brand'))
buy  = na.omit(train_test)
## user_id + sku_id下周五天的购买度,即5天内购买的商品只有35%左右的商品在前7天内有用户行为记录
nrow(buy)/nrow(test_ordered)#0.346263
length(unique(buy$user_id))/length(unique(test_ordered$user_id))#0.3479087

## user_id + sku_id下周五天的购买度,即5天内购买的商品只有39%左右的商品在前14天内有用户行为记录
nrow(buy)/nrow(test_ordered)#0.3935667
length(unique(buy$user_id))/length(unique(test_ordered$user_id))#0.3935667
## user_id + sku_id下周五天的购买度,即5天内购买的商品只有41%左右的商品在前19天内有用户行为记录
nrow(buy)/nrow(test_ordered)#0.4124882
length(unique(buy$user_id))/length(unique(test_ordered$user_id))#0.41444
## user_id + sku_id下周五天的购买度,即5天内购买的商品只有45%左右的商品在前28天内有用户行为记录
nrow(buy)/nrow(test_ordered)#0.4437086
length(unique(buy$user_id))/length(unique(test_ordered$user_id))#0.4458175

## user_id + sku_id下周五天的购买度,即5天内购买的商品只有46%左右的商品在前40天内有用户行为记录
nrow(buy)/nrow(test_ordered)#0.461
length(unique(buy$user_id))/length(unique(test_ordered$user_id))#0.463

##===================
  sku_id action属性
##===================
sku_deal = function(data,...){

sku_poplation  = function( data){
  a = data %>% group_by(sku_id,type) %>% summarise(uv = n_distinct(user_id)) %>% 
  spread(type, uv,fill = 0 )
names(a)[2:7] = c('browse','add_trolley','delete_trolley',
                            'ordered','attention','click')
a
}

sku_type = sku_poplation(data) %>% left_join(comments[comments$dt=='2016-04-15',-1],
                                              by= 'sku_id')
summary(sku_type)
sku_type$ordered_per_click = round(sku_type$ordered/(sku_type$click+1),4)
sku_type$ordered_per_browse = round(sku_type$ordered/(sku_type$browse+1),4)

sku_type[is.na(sku_type)] = -1 
sku_type
}

##=========================
     user_id action属性
##=========================
user_deal = function(data,...){
  
user_habit = function(data){
  a = data %>% group_by(user_id,type) %>% summarise( sku_num= n_distinct(sku_id)) %>% 
    spread(type, sku_num,fill = 0 )
  names(a)[2:7] = c('browse','add_trolley','delete_trolley',
                    'ordered','attention','click')
  a
}

user_type = user_habit(data) %>% left_join(user,by= 'user_id')
user_type$age = factor(user_type$age)
levels(user_type$age) =c('0','1','2','3','4','5','6')
user_type$age =as.numeric(user_type$age)
user_type$user_reg_duration = interval(user_type$user_reg_dt,'2016-04-02') %>%
  time_length('day')

user_type$ordered_user_click = round(user_type$ordered/(user_type$click+1),4)
user_type$ordered_user_browse = round(user_type$ordered/(user_type$browse+1),4)
user_type
}

##构造带目标变量的训练训练样本
train_test = train_type %>% left_join(test_ordered[,c('user_id','sku_id','brand','ordered')],
                                      by=c('user_id','sku_id','brand'))
train_test$ordered.y = ifelse(is.na(train_test$ordered.y),0,1)
train_test = rename(train_test,y = ordered.y,ordered= ordered.x)
traindata = train_test %>% left_join(sku_type,by = 'sku_id') %>% 
  left_join(user_type,by='user_id')
traindata$user_reg_dt = NULL

label_pos = traindata[traindata$y==1,]
label_neg = subset(traindata,browse.x<max(label_pos$browse.x)&
                  add_trolley.x <max(label_pos$add_trolley.x)&
                  click.x<max(label_pos$click.x)&
                  delete_trolley.x<max(label_pos$delete_trolley.x)&
                   y==0)
ind <- sample(3, nrow(label_neg), replace=TRUE, prob=c(0.33, 0.33,0.33))
data_final =  rbind(label_pos,label_neg[ind==1,]) 
table(data_final$y)

xgb_train=function(data,eta=0.008,max_depth = 10,gamma = 0.05,lambda=50,
                   colsample_bytree=0.8,min_child_weight=0.1, nround=500,
                   scale_pos_weight=500){
  set.seed(1234)
  i<- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
  train =  data[i==1,]
  test = data[i==2,]
  dtrain <- xgb.DMatrix(as.matrix(train[,-c(1:3,10),drop =F]),
                        label = as.matrix(train[,c('y'),drop =F]))
  
  dtest <- xgb.DMatrix(as.matrix(test[,-c(1:3,10),drop =F]),
                        label = as.matrix(test[,c('y'),drop =F]))
  
  watchlist <- list(eval = dtest, train = dtrain)
  param <- list(max.depth = max_depth,
                eta = eta,
                silent = 1,
                min_child_weight=min_child_weight,
                objective="binary:logistic",
                colsample_bytree=colsample_bytree,
                gamma=gamma,
                eval_metric='logloss',
                early.stop.round=50,
                scale_pos_weight=scale_pos_weight,
                lambda = lambda
  )
  
  bst <- xgb.train(param, dtrain,
                   nthread = 4,
                   watchlist,
                   nround =nround
  )
  pred=predict(bst,dtest)
  list(pred = pred,model = bst)
}

pre = xgb_train(data_final)
summary(pre$pred)
boxplot(pre$pred)
sum(pre$pred>0.9)

set.seed(1234)
i<- sample(2, nrow(data_final), replace=TRUE, prob=c(0.7, 0.3))
test = data_final[i==2,]
dtest <- xgb.DMatrix(as.matrix(test[,-c(1:3,10),drop =F]),
                     label = as.matrix(test[,c('y'),drop =F]))

pred=predict(pre$model,as.matrix(test[,-c(1:3,10),drop =F]))

pre_label = ifelse(pred>0.9,1,0)
table(pre_label,label=test$y)

names<-dimnames(data.matrix(data_final[,-c(1,2,3)]))[[2]]
importance_matrix=xgb.importance(names,model = pre$model)
xgb.plot.importance(importance_matrix)


## 构造全样本集，用于最终的预测
full_train = rbind(train_03,train_04,train_04_remain) %>% transfrom()
full_sku = rbind(train_03,train_04,train_04_remain) %>% sku_deal()
full_user = rbind(train_03,train_04,train_04_remain) %>% user_deal()
fulldata = full_train %>% left_join(full_sku,by = 'sku_id') %>% 
  left_join(full_user,by='user_id')
fulldata$user_reg_dt = NULL
setdiff(names(traindata),names(fulldata))

fulldata2  = as.matrix(fulldata[,-c(1:3),drop =F])
                     
prob = predict(pre$model,fulldata2)

submit = cbind(fulldata[,c(1,2)],prob= prob)
sub = submit[submit$prob>=0.9,]
length(unique(sub$user_id)) #1304  
##
sub = submit[submit$prob>=0.91,]
length(unique(sub$user_id)) #1080 
length(unique(sub$sku_id)) #128
##预测结果去重
user_cnt  = sub %>% group_by(user_id) %>% summarise(cnt = n())
user_two = user_cnt$user_id[user_cnt$cnt>1]
sub_one = subset(sub,!user_id %in%user_two)
sub_two = subset(sub,user_id %in%user_two)
sub_tow2 = sub_two %>% group_by(user_id) %>% summarise(prob = max(prob)) %>% 
  inner_join(sub_two,by = c('user_id','prob'))

sub_final = rbind(sub_one[,c('user_id','sku_id')],sub_tow2[,c('user_id','sku_id')])
write.csv(sub_final,'./submit/pre_20170330.csv',row.names=F, fileEncoding = "UTF-8")
write.csv(sub_final,'./submit/pre_20170330_1.csv',row.names=F, fileEncoding = "UTF-8")
