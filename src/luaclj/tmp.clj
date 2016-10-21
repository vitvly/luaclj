[:chunk
 [:block
  [:stat
   [:varlist [:var [:Name "i"]]]
   "="
   [:explist [:exp [:Numeral "2"]]]]
  [:stat
   [:varlist [:var [:Name "booleanValue"]]]
   "="
   [:explist [:exp [:prefixexp [:var [:Name "false"]]]]]]
  [:stat
   [:varlist [:var [:Name "multi1"]] "," [:var [:Name "multi2"]]]
   "="
   [:explist
    [:exp [:Numeral "1"]]
    ","
    [:exp [:prefixexp [:var [:Name "true"]]]]]]
  [:stat
   "if"
   [:exp
    [:exp [:prefixexp [:var [:Name "i"]]]]
    [:binop "<"]
    [:exp [:Numeral "3"]]]
   "then"
   [:block
    [:stat
     "local"
     [:namelist [:Name "l"]]
     "="
     [:explist [:exp [:LiteralString "'local_var'"]]]]
    [:retstat "return" [:explist [:exp [:Numeral "3"]]]]]
   "else"
   [:block [:retstat "return" [:explist [:exp [:Numeral "0"]]]]]
   "end"]
  [:stat
   [:varlist [:var [:Name "sum"]]]
   "="
   [:explist [:exp [:Numeral "0"]]]]
  [:stat
   "for"
   [:Name "j"]
   "="
   [:exp [:Numeral "1"]]
   ","
   [:exp [:Numeral "100"]]
   "do"
   [:block
    [:stat
     [:varlist [:var [:Name "sum"]]]
     "="
     [:explist
      [:exp
       [:exp [:prefixexp [:var [:Name "sum"]]]]
       [:binop "+"]
       [:exp [:prefixexp [:var [:Name "j"]]]]]]]]
   "end"]]]
nil

