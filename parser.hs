module Parse ( Parser, Result(Good, Bad), parse ) where


data Result = Good String | Bad deriving Show
type Parser = Result -> Result


-- operators

(<.>) f g r
  | isbad k = Bad
  | otherwise = f k
  where k = g r


(<|>) f g r = v (f r) (g r)
  where v (Good a) _ = (Good a)
        v Bad b = b


-- generics

nothing r = r


match c = matchif (== c) 


skip p = p <|> nothing


until' c = some (matchif (/= c))


some f r
  | isbad k = r
  | otherwise = some f k
  where k = f r


matchif p Bad = Bad
matchif p (Good []) = Bad

matchif p (Good s)
  | p (head s) = Good (tail s)
  | otherwise = Bad


matches [] r = r

matches s r
  | isbad k = Bad
  | otherwise = matches (tail s) k
  where k = match (head s) r


isbad Bad = True
isbad (Good _) = False


-- whitespace

space = match ' '
    <|> match '\n'
    <|> match '\t'


-- attributes

attributes = (until' '>') <.> space  


-- tags

opentag name = (match '>') 
           <.> (skip attributes) 
           <.> (matches name) 
           <.> (match '<')


closetag name = (match '>')
            <.> (matches name)
            <.> (matches "</")


-- nodes

pad p = (some space) <.> p <.> (some space)


node name p = (pad (closetag name)) <.> p <.> (pad (opentag name))


letter = matchif (\c -> c /= '<' && c /= '>')
text = (some letter) <.> letter


leaveopen = (opentag "br")
        <|> (opentag "hr")


-- contexts

child = text
    <|> leaveopen
    <|> (node "h1" (some child1))
    <|> (node "h2" (some child1))
    <|> (node "h3" (some child1))
    <|> (node "a" (some child1))
    <|> (node "ul" (some child1))
    <|> (node "li" (some child1))


child1 = child 
     <|> (node "p" (some child))
     <|> (node "div" (some child1))


child2 = child1 <|> (node "title" (some child1))


child3 = (node "body" (some child1)) 
     <.> (some child1) 
     <.> (node "head" (some child2))
     <.> (some child1)


-- root

parse = node "html" child3
