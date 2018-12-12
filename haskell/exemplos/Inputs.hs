import Data.List
f=scanl(+)0(1:f)
main=do{n<-fmap read getLine;putStrLn$intercalate" "$fmap show$take n f}