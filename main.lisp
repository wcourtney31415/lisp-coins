
(  print "Enter a dollar ammount as a decimal number:"  )
(  setq ammount     
    (  read  )  )


(  defun determineCoins     
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
            (  determineCoins nextAmmount nextList nextCoinList  ) )        
        ( reverse myList ) ) )


( defun formattedOutput     
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
            ( formattedOutput nextList nextString nextCoinNames) )myString) )


(  print     
    ( formattedOutput         
        (  determineCoins             
            (  * ammount 100  ) Nil             
            (  List 25 10 5 1  )  ) ""         
        ( List " Quarter(s) " " Dime(s) " " Nickle(s) " " Pennie(s) " ) ))