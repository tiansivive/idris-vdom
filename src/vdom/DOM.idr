module src.vdom.DOM

import src.vdom.vDom

export
data Element = MkElem Ptr

export
getDocumentBody : JS_IO Element
getDocumentBody = map MkElem $ 
  foreign FFI_JS "document.body" (JS_IO Ptr)

export
createElement : String -> JS_IO Element
createElement tag = map MkElem $
  foreign FFI_JS "document.createElement(%0)" (String -> JS_IO Ptr) tag

export
createTextElement : String -> JS_IO Element
createTextElement content = map MkElem $
  foreign FFI_JS "document.createTextNode(%0)" (String -> JS_IO Ptr) content

export
getChild : Element -> Int -> JS_IO Element
getChild (MkElem parent) index = map MkElem $
  foreign FFI_JS "%0.childNodes[%1])" (Ptr -> Int -> JS_IO Ptr) parent index

export
appendChild : Element -> Element -> JS_IO Element
appendChild (MkElem parent) (MkElem child) = do
  foreign FFI_JS "%0.appendChild(%1)" (Ptr -> Ptr -> JS_IO Ptr) parent child
  pure $ MkElem parent

export
replaceChild : Element -> Element -> Int -> JS_IO Element
replaceChild (MkElem parent) (MkElem newChild) index = do
  (MkElem oldChild) <- getChild (MkElem parent) index
  foreign FFI_JS "%0.replaceChild(%1, %2)" (Ptr -> Ptr -> Ptr -> JS_IO Ptr) parent newChild oldChild
  pure $ MkElem parent

export
removeChild : Element -> Int -> JS_IO Element
removeChild (MkElem parent) index = do
  (MkElem child) <- getChild (MkElem parent) index
  foreign FFI_JS "%0.removeChild(%1)" (Ptr -> Ptr -> JS_IO Ptr) parent child
  pure $ MkElem parent

export
setAttribute : Element -> (String, String) -> JS_IO ()
setAttribute (MkElem ptr) (key, value) = 
  foreign FFI_JS "%0.setAttribute(%1, %2)" (Ptr -> String -> String -> JS_IO ()) ptr key value
  
export
addEventListener : Element -> EventListener -> JS_IO ()
addEventListener (MkElem ptr) (On ty cb) = 
  foreign FFI_JS "%0.addEventListener(%1, %2)" 
  (Ptr -> String -> JsFn (Ptr -> JS_IO ()) -> JS_IO ())
  ptr ty (MkJsFn cb)


export 
mapToDomElement : VNode -> JS_IO Element
mapToDomElement (Text content) = createTextElement content
mapToDomElement (vDom.Element tag props handlers children) = do
    root <- createElement tag
    traverse_ (setAttribute root) props
    traverse_ (addEventListener root) handlers
    childElements <- traverse mapToDomElement children
    foldlM appendChild root childElements


export
makeDomFromVirtual : VNode -> JS_IO ()
makeDomFromVirtual html = do
  el <- mapToDomElement html
  body <- getDocumentBody
  appendChild body el
  pure ()




maybeZip : List a -> List b -> List (Maybe a, Maybe b)
maybeZip (a::as) (b::bs) = (Just a , Just b ) :: maybeZip as bs
maybeZip (a::as) []      = (Just a , Nothing) :: maybeZip as []
maybeZip []      (b::bs) = (Nothing, Just b ) :: maybeZip [] bs
maybeZip _       _       = []
  

export
updateDom : Element -> Maybe VNode -> Maybe VNode -> Int -> JS_IO Element
updateDom parent old new index =
  case old of
    Nothing => 
      case new of
        Nothing => pure parent
        Just newNode => do
          domElem <- mapToDomElement newNode
          appendChild parent domElem
    Just oldNode => 
      case new of 
        Nothing => removeChild parent index
        Just newNode => 
          if not (oldNode == newNode) then 
            do
              newChild <- mapToDomElement newNode
              replaceChild parent newChild index
          else 
            if isElement newNode then 
              do
                let 
                  (vDom.Element _ _ _ oldChildren) = oldNode
                  (vDom.Element _ _ _ newChildren) = newNode
                  vNodes = maybeZip oldChildren newChildren
                  indexedNodes = zip [0 .. length vNodes] vNodes

                next <- getChild parent index
                traverse_ (\(i, (o, n)) => updateDom next o n i) indexedNodes
                pure parent
            else 
              pure parent  
  


  
