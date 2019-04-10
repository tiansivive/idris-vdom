module src.vdom.vDom


-- import Data.SortedMap

-- data Props = SortedMap String String

public export
data EventListener : Type where
  On : String -> (Ptr -> JS_IO ()) -> EventListener

public export
Props : Type
Props = List (String, String)


checkPropEquality : Props -> Props -> Bool
checkPropEquality [] [] = True
checkPropEquality (x::xs) [] = False
checkPropEquality [] (y::ys) = False
checkPropEquality (x::xs) (y::ys) =
    if fst x == fst y && snd x == snd y then
      checkPropEquality xs ys
    else
      False


prettyPrint : Props -> String
prettyPrint (x::xs) = fst x ++ "=" ++ snd x ++ " " ++ end
  where end = if xs == [] then "" else prettyPrint xs


public export 
data VNode : Type where
  Element : (tag: String) 
    -> (props: Props) 
    -> (handlers: List EventListener)
    -> (children: List VNode)
    -> VNode 
  Text : (content: String) -> VNode 

-- prettyPrintChildren : List VNode -> String
-- prettyPrintChildren [] = ""
-- prettyPrintChildren (x::xs) = 
--   let start = 
--     case x of
--       Text content => content
--       otherwise => show x
--   in
--     start ++ prettyPrintChildren xs 


export
Eq VNode where
  (Element tag1 props1 _ _) == (Element tag2 props2 _ _) = 
    tag1 == tag2 && checkPropEquality props1 props2
  -- (Element _ _ _ children1) == (Element _ _ children2) = children1 == children2
  (Text str1) == (Text str2) = str1 == str2
  _ == _ = True


export
Show VNode where
  show (Element tag props handlers children) = "\n<" 
    ++ tag ++ " " 
    ++ prettyPrint props
    ++ ">\n\t" 
    ++ show children 
    ++ "\n</" ++ tag ++ ">" 
  show (Text content) = content






export
h : String -> Props -> List EventListener -> List VNode -> VNode
h = Element

export
div : Props -> List EventListener -> List VNode -> VNode
div = Element "div"

export
p : Props -> List EventListener -> List VNode -> VNode
p = Element "p"

export
span : Props -> List EventListener -> List VNode -> VNode
span = Element "span"

export
a : Props -> List EventListener -> List VNode -> VNode
a = Element "a"

export
input : Props -> List EventListener -> List VNode -> VNode
input = Element "input"

export
button : Props -> List EventListener -> List VNode -> VNode
button = Element "button"

export
ul : Props -> List EventListener -> List VNode -> VNode
ul = Element "ul"

export
li : Props -> List EventListener -> List VNode -> VNode
li = Element "li"
