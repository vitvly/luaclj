sum = 0
for j = 1, 99 do
  sum = sum + j
end

t = {1, 10, 30}
t['a'] = 9
key = 22
t1 = {a=2, b=3, c=4}
t2 = {[key]=10, ["a"]=20, ["b"]=30, ["c"]=40}
t2.a = 30

for i,v in ipairs(t2) do
  sum = sum + v
end
return sum
