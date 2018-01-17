module Tree

public export
data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

-- This is for case splitting
%name Tree left, right

export
insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x oldTree@(Node left y right) =
  case compare x y of
    LT => Node (insert x left) y right
    EQ => oldTree
    GT => Node left y (insert x right)
