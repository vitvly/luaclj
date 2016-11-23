function test1()
  return 5
end

function test2()
  return 7
end


a_fn = function()
  return test1() + test2()
end
b_fn = function(arg)
  return arg + test1() + test2()
end

return a_fn() + b_fn(2)
