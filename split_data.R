
dat = read.csv("/Users/swiecki/diss_2/data/TADMUSCoded.csv", stringsAsFactors = F)

ids = unique(dat$TEAMNUM)

split= sample(x = ids,size = 32,replace = F)

sub = dat$TEAMNUM %in% split

training = dat[sub,]

test = dat[!sub,]

write.csv(training, "TADMUS.training.csv")

write.csv(test, "TADMUS.test.csv")


#set rotation set param to the other rotation




