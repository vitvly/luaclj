function get_days_in_month(month, year)
  local days_in_month = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }   
  local d = days_in_month[month]
  --[[ multi-line comment
  continuing
  and continuing ]]
  -- check for leap year
  if (month == 2) then
    if (mod(year,4) == 0) then
     if (mod(year,100) == 0)then                
      if (mod(year,400) == 0) then                    
          d = 29
      end
     else                
      d = 29
     end
    end
  end

  return d  
end

return get_days_in_month(7, 2007)
