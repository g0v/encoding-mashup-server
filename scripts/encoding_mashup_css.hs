{-# LANGUAGE OverloadedStrings#-}
import Clay

main :: IO ()
main = putCss myStylesheet

myStylesheet :: Css
myStylesheet = do 
    star # byClass "char-info" ? do
        margin 2 2 2 2
        height $ px 40
        float sideLeft

    star # byClass "char-info-display" ? do
       float sideLeft 
       fontSize $ px 24
       lineHeight $ px 24
       height $ px 30
       width $ px 100
       border solid (px 2) black
       borderRadius $ px 8

    input ? do
        float sideLeft
