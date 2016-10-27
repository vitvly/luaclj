i = 2
booleanValue = false
multi1, multi2 = 1, true

while i < 5 do
  i = i+1
end

repeat
  i = i + 1
until i > 10

if i > 10 then
  return i
elseif i > 3 then
  local l = 'local_var'
  return 3
elseif i == 2 then
  return 44
else
  return 55
end

sum = 0
for j = 1, 100 do
  sum = sum + j
end
