pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple mx = if mx <=0 then [] else do
    a <- [1..mx]
    b <- [1..mx]
    c <- [1..mx]
    if (a^2 + b^2 == c^2) && (a < b) then "Y" else []
    return (a,b,c)
