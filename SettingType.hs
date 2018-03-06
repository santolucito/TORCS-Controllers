module SettingType where


--This could be a higher order datatype with fxns
data Setting = Setting String String deriving (Show,Eq)

asCode :: Setting -> String
asCode (Setting f i) = "Setting \""++f++"\" \""++i++"\" -> "++"arr "++f++" -< "++i
