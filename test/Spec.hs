import           Parser

expr = "let ift = " ++ ift ++ " in " ++ bool

bool =
    "~(b : {Boolean : ift.IFT..ift.IFT} ^ {true : ift.IFT} ^ {false : ift.IFT})"
        ++ "{ Boolean = ift.IFT }"
        ++ " ^ { true = \\(x : {A : Bot..Top})\\(t : x.A)\\(f : x.A) t }"
        ++ " ^ { false = \\(x : {A : Bot..Top})\\(t : x.A)\\(f : x.A) f }"

ift =
    let i = "\\(x : {A : Bot..Top})\\(t : x.A)\\(f : x.A) x.A "
    in  "~(x : {IFT : " ++ "Bot" ++ ".." ++ "Top" ++ "}) {IFT = " ++ i ++ "}"

main :: IO ()
main = do
    print $ regularParse term expr
    print $ regularParse term ift

