function isDivisibleBy(n, div)
  while true do
    if n == 0 then
      return true
    elseif n <= 0 then
      return false
    end
    n = n - div
  end
end

function fizzbuzz(n)
  local div3 = isDivisibleBy(n, 3)
  local div5 = isDivisibleBy(n, 5)
  if div3 and div5 then
    print("fizzbuzz")
  elseif div3 then
    print("fizz")
  elseif div5 then
    print("buzz")
  else
    print(n)
  end
end

function fizzbuzzAlt(upTo)
  local n = 1
  local till3 = 2
  local till5 = 4
  repeat
    if till3 == 0 and till5 == 0 then
      print("fizzbuzz")
    elseif till3 == 0 then
      print("fizz")
    elseif till5 == 0 then
      print("buzz")
    else
      print(n)
    end
    if till3 == 0 then
      till3 = 3
    end
    if till5 == 0 then
      till5 = 5
    end
    till5 = till5-1
    till3 = till3-1
    n = n+1
  until upTo <= n
end

for i=1,100 do
  fizzbuzz(i)
end

fizzbuzzAlt(100)
