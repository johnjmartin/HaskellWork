-- Greetings.hs
-- Copyright (c) 2012 Carsten König 
-- 
 
module Greetings where 
import System.Environment
 
main :: IO ()
main = getArgs >>= print . greetings . head   
 
greetings s = "Hello! " ++ s