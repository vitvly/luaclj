retval = 0
some_fn = function(arg1, arg2)
  local d = {2, 20}
  for i = 1,10 do
    if (i >= arg1) and (i <= arg2) then
      retval = retval + i
    end
  end

  for i, v in ipairs(d) do
    retval = retval + v
  end
  return retval
end

return some_fn(3, 5)
