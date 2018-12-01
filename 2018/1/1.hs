-- stack --resolver lts-12.0 script

main = print . sum . map read . lines . filter (/= '+') =<< getContents
