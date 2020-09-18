{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Neat where

import Control.Monad.Random
import Control.Monad.State

newtype InnovationID
  = InnovationID Int deriving (Show, Eq, Ord, Enum)

newtype NodeID
  = NodeID Int deriving (Show, Eq, Ord, Enum)

data Link
  = Link
    { inNode  :: NodeID
    , outNode :: NodeID
    } deriving (Show, Eq)

data Gene
  = Gene
    { link         :: Link
    , weight       :: Float
    , enabled      :: Bool
    , innovationID :: InnovationID
    }
    deriving (Show)

instance Eq Gene where
  gene0 == gene1
    = innovationID gene0 == innovationID gene1

data Genome
  = Genome
    { genes   :: [Gene]
    , inputs  :: [NodeID]
    , outputs :: [NodeID]
    , hidden  :: [NodeID]
    }
    deriving (Show, Eq)

type Innovations
  = [(Link, InnovationID)]

perturbWeight :: MonadRandom m => Gene -> m Gene
perturbWeight gene
  = do
    adjustment <- getRandomR (-1.0, 1.0)
    return $ gene {weight = weight gene + adjustment}

perturbWeights :: MonadRandom m => Genome -> m Genome
perturbWeights genome
  = do
    genes' <- mapM perturbWeight $ genes genome
    return $ genome {genes = genes'}

addHiddenNode :: Genome -> (NodeID, Genome)
addHiddenNode genome
  = case hidden genome of
      []    -> let nodeCount = (length $ inputs genome) + (length $ outputs genome)
                in (NodeID nodeCount, genome {hidden = [NodeID nodeCount]})
      nodes -> (succ $ maximum nodes, genome)

getInnovationID :: MonadRandom m => Link -> StateT Innovations m InnovationID
getInnovationID link
  = do
    innovations <- get
    case lookup link innovations of
      Just innovID -> return innovID
      Nothing -> do
        let innovID = InnovationID $ length innovations
        put $ innovations ++ [(link, innovID)]
        return innovID

addLink :: MonadRandom m => Link -> Float -> Genome -> StateT Innovations m Genome
addLink link weight genome
  = do
    innovID <- getInnovationID link
    let gene = Gene link weight True innovID
     in return $ genome {genes = gene : genes genome}

disable :: Gene -> Gene
disable gene
  = gene {enabled = False}

selectAndDisableRandomGene :: MonadRandom m => Genome -> m (Gene, Genome)
selectAndDisableRandomGene genome
  = do
    index <- getRandomR (0, (length $ genes genome) - 1)
    let (before, gene : after) = splitAt index $ genes genome
        genome' = genome {genes = before ++ [disable gene] ++ after}
    return (gene, genome')

mutateNode :: MonadRandom m => Genome -> StateT Innovations m Genome
mutateNode genome
  = do
    (gene, genome') <- selectAndDisableRandomGene genome
    let (Link inNode outNode) = link gene
        (hiddenNode, genome'') = addHiddenNode genome'
        addInGene = addLink (Link inNode hiddenNode) 1.0
        addOutGene = addLink (Link hiddenNode outNode) (weight gene)
    (addInGene >=> addOutGene) genome''

getIncommingNodes :: Genome -> NodeID -> [NodeID]
getIncommingNodes genome node
  = let immediate = map inNode $ filter ((== node) . outNode ) $ map link $ genes genome
     in immediate ++ concatMap (getIncommingNodes genome) immediate

isNotCyclic :: Genome -> Link -> Bool
isNotCyclic genome (Link nodeIn nodeOut)
  = nodeIn /= nodeOut && nodeOut `notElem` (getIncommingNodes genome nodeIn)

isValidLink :: Genome -> Link -> Bool
isValidLink genome link'
  = let existingLinks = map link $ genes genome
     in isNotCyclic genome link' && link' `notElem` existingLinks

getUnlinked :: Genome -> [Link]
getUnlinked genome
  = let allLinks = zipWith Link (inputs genome ++ hidden genome) (outputs genome ++ hidden genome)
     in filter (isValidLink genome) allLinks

addRandomLink :: MonadRandom m => Link -> Genome -> StateT Innovations m Genome
addRandomLink link genome
  = do
    weight <- getRandomR (-2.0, 2.0)
    addLink link weight genome

mutateLink :: MonadRandom m => Genome -> StateT Innovations m Genome
mutateLink genome
  = do
    maybeLink <- uniformMay $ getUnlinked genome
    case maybeLink of
      Nothing -> return genome -- genome is fully connected already
      Just link -> do
        weight <- getRandomR (-2.0, 2.0)
        addLink link weight genome
