
(  print "Enter a dollar ammount as a decimal number:"  )
(  setq ammount     
    (  read  )  )
(  defun runGoStart     
    (  ammount myList coinList  )     
    (  if coinList        
        (  let*            
            (                  
                (  coin                     
                    (  first coinList  )  )                 
                (  evenFit                     
                    (  floor                         
                        (  / ammount coin  )  )  )                 
                (  nextAmmount                     
                    (  - ammount                         
                        (  * evenFit coin  )  )  )                 
                (  nextList                     
                    (  cons evenFit myList  )  )                 
                (  nextCoinList                     
                    (  cdr coinList  )  )  )             
            (  runGoStart nextAmmount nextList nextCoinList  ) )        
        ( reverse myList ) ) )
( defun fancyPrint     
    ( myList myString coinNames )     
    ( if myList        
        (  let*            
            (                 
                ( coinName                     
                    ( first coinNames ) )                
                ( nextCoinNames                     
                    ( cdr coinNames ) )                
                ( coinCount                     
                    ( first myList ) )                
                ( myText                     
                    (  if                        
                        ( = coinCount 0 ) ""                         
                        ( concatenate 'string                             
                            ( write-to-string coincount ) coinName ) ) )                
                ( nextString                     
                    ( concatenate 'string myString myText) )                
                ( nextList                     
                    ( cdr myList ) ) )            
            ( fancyPrint nextList nextString nextCoinNames) )myString) )
(  print     
    ( fancyPrint         
        (  runGoStart             
            (  * ammount 100  ) Nil             
            (  List 25 10 5 1  )  ) ""         
        ( List " Quarters " " Dimes " " Nickles " " Pennies " ) ))