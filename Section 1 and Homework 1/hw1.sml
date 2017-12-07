use "hw1.sml"; 

fun is_older(date1 : int * int * int, date2 : int * int * int) =
  if #1 date1 < #1 date2
  then true
  else if #1 date1 = #1 date2
  then if #2 date1 < #2 date2
       then true
       else if #2 date1 = #2 date2
       then if #3 date1 < #3 date2
	    then true
	    else false
       else false
  else false

fun number_in_month(date_list : (int * int * int) list, month : int) =
  if null date_list
  then 0
  else if (#2 (hd date_list)) = month
  then 1 + number_in_month((tl date_list), month)
  else 0 + number_in_month((tl date_list), month)
			  
fun number_in_months(date_list : (int * int * int) list, months : int list) =
  if null months
  then 0
  else number_in_month(date_list, (hd months)) + number_in_months(date_list, (tl months))
								 
fun dates_in_month(date_list : (int * int * int) list, month : int) =
  if null date_list
  then []
  else if (#2 (hd date_list) = month)
  then (hd date_list) :: dates_in_month((tl date_list), month)
  else dates_in_month((tl date_list), month)

fun dates_in_months(date_list : (int * int * int) list, months : int list) =
  if null months
  then []
  else dates_in_month(date_list, (hd months)) @ dates_in_months(date_list, (tl months))

fun get_nth(strings : string list, num : int) =
  if num = 1
  then hd strings
  else get_nth((tl strings), (num-1))

fun date_to_string(date : (int * int * int)) =
  let
      val months = ["January", "February", "Marth", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
      get_nth(months, (#2 date)) ^ " " ^ Int.toString(#3 date) ^ ", " ^  Int.toString(#1 date)
  end

fun number_before_reaching_sum(sum : int, num_list : int list) = 
      if(sum <= (hd num_list))
      then 0
      else 1 + number_before_reaching_sum((sum - (hd num_list)), (tl num_list))

fun what_month(day : int) =
  let
      val days_in_months = [31, 30, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
      number_before_reaching_sum(day, days_in_months) + 1
  end

fun month_range(day1 : int, day2 : int) =
  if(day1 > day2)
  then []
  else what_month(day1) :: month_range((day1+1), day2)

fun oldest(date_list : (int * int * int) list) =
  if null date_list
  then NONE
  else let val oldest_date = oldest(tl date_list)
       in if isSome oldest_date andalso is_older((valOf oldest_date), (hd date_list))
	  then oldest_date
	  else SOME(hd date_list)
       end
	   
							  
				      
      
								      
						
