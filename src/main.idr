module Main

import src.vdom.DOM
import src.vdom.vDom


say: String -> (evt: Ptr) -> JS_IO ()
say content _ = foreign FFI_JS "console.log(%0)" (String -> JS_IO ()) content

html : vDom.VNode
html = 
  ul [("class", "out")] [ On "click" (say "I just got clicked") ]
    [ 
      li [("class", "nested1")] [] [(Text "Is this Working?")],
      li [("class", "nested2")] [] [(Text "Surely?")],
      li [("class", "nested3")] [] [(Text "It is?")]
    ]

main : JS_IO ()
main = do
  -- _ <- DOM.createElement "div"
  -- putStrLn $ show $ 
  makeDomFromVirtual html