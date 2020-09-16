module Neat where

import Control.Monad.Random
import Control.Monad.State

data NodeType
  = Input | Output | Hidden
    deriving (Show, Eq)

data Node
  = Node
    { nodeType :: NodeType
    , uniqueID :: Int
    }
    deriving (Show, Eq)

data Gene
  = Gene
    { inNode  :: Node
    , outNode :: Node
    , weight  :: Float
    , enabled :: Bool
    , innovID :: Int
    }
    deriving (Show)

instance Eq Gene where
  gene0 == gene1
    = innovID gene0 == innovID gene1

type Genome
  = [Gene]

type Innovations
  = [(Node, Node)]

perturbWeight :: RandomGen g => Gene -> Rand g Gene
perturbWeight gene
  = do
    adjustment <- getRandomR (-1.0, 1.0)
    return $ gene {weight = weight gene + adjustment}

perturbWeights :: RandomGen g => Genome -> Rand g Genome
perturbWeights
  = mapM perturbWeight

newHiddenNode :: Genome -> Node
newHiddenNode genome
  = Node Hidden $ 1 + (maximum $ map uniqueID nodes)
    where
      nodes = map inNode genome ++ map outNode genome

getInnovationID :: Monad m => (Node, Node) -> StateT Innovations m Int
getInnovationID link
  = do
    innovations <- get
    put (innovations ++ [link])
    return $ length innovations

createInOutGenes :: Monad m => Gene -> Node -> StateT Innovations m (Gene, Gene)
createInOutGenes gene newNode
  = do
    inInnovID <- getInnovationID (inNode gene, newNode)
    outInnovID <- getInnovationID (newNode, outNode gene)
    return ( Gene (inNode gene) newNode 1.0 True inInnovID
           , Gene newNode (outNode gene) (weight gene) True outInnovID
           )

mutateNode :: RandomGen g => Genome -> StateT Innovations (Rand g) Genome
mutateNode genome
  = do
    index <- getRandomR (0, length genome - 1)
    let (before, gene : after) = splitAt index genome
    (geneIn, geneOut) <- createInOutGenes gene $ newHiddenNode genome
    return $ geneOut : geneIn : before ++ [gene {enabled = False}] ++ after
