list1 = take 20 [1, 3 ..]
list2 = [1, 3 .. 39]
list3 = take 20 (filter odd [1 ..])

main = do
    print list1
    print list2
    print list3