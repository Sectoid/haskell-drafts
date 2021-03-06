module XML (xmlNode, xmlAttr, xsiType, noValue, noAttr)
    where

data XMLTagType = TagOpen
                | TagClosed
                | TagBoth

trimIndent :: String -> String
trimIndent [] = []
trimIndent xs = ' ' : xs

xmlTag :: XMLTagType -> String -> String
xmlTag TagOpen   value = '<'       : value ++ ">"
xmlTag TagClosed value = '<' : '/' : value ++ ">"
xmlTag TagBoth   value = '<'       : value ++ "/>"

xmlNode :: String -> String -> [(String, String)] -> String
xmlNode name [] attrList =
    xmlTag TagBoth (name ++ trimIndent (attrList >>= xmlAttr))

xmlNode name value attrList = 
    xmlTag TagOpen (name ++ trimIndent (attrList >>= xmlAttr)) ++
    value ++
    xmlTag TagClosed name

xmlAttr :: (String, String) -> String
xmlAttr (name, value) = name ++ "=\"" ++ value ++ "\" "

xsiType :: String -> (String, String)
xsiType value = ("xsi:type", value)

noValue :: String
noValue = []

noAttr :: [(String, String)]
noAttr = []
