function fibrec(n)
  if n == 0 then
    return 1
  elseif n == 1 then
    return 1
  end
  return fibrec(n-1) + fibrec(n-2)
end

function fib(n)
  local a = 0
  local b = 1
  local temp
  for i=1,n-1 do
    temp = b
    b = a + b
    a = temp
  end
  return b
end

print("rec")
for i=1,10 do
  print(fibrec(i))
end

print("non-rec")
for i=20,50 do
  print(fib(i))
end
