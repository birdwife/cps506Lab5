module Lab5 where

thirdLast x  = do
 let rev1 = reverse x
 let tail1 = tail rev1
 let tail2 = tail tail1
 head tail2

-- everyOther is a recursive function that is a wrapper for everyOtherR

everyOther x = do
 let len1 = (length x) + 1
 let x2 = reverse x
 everyOtherR x2 len1 []

everyOtherR oldList count newlist = do
 let count1 = count - 1
 let head1 = head oldList
 let tail1 = tail oldList
 if (count1 == 0) then do
  newlist
 else do
  if (rem count1 2) == 0 then do
   everyOtherR tail1 count1 newlist
  else do
   let newlist2 = head1 : newlist
   everyOtherR tail1 count1 newlist2


-- sumPosList is a wrapper function for sumPosListR

sumPosList x = do
 sumPosListR x 0

sumPosListR list1 sum1  = do
 let head1 = head list1
 let tail1 = tail list1
 let len1 = length tail1
 if len1 > 0 then do
  if head1 > 0 then do
   let sum2 = sum1 + head1
   sumPosListR tail1 sum2
  else do
   sumPosListR tail1 sum1
 else do
  sum1 + head1
 
