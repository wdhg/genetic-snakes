module Neat where

import Control.Monad       ((>=>))
import Control.Monad.State
import System.Random
import Utils               (getIndex, replaceAt)

-- config
-- weight mutation
mutateWeightsChance = 0.8 :: Float
perturbChance = 0.9       :: Float
maxPerturbAmount = 0.2    :: Float
-- reenable mutation
reenableChance = 0.1 :: Float

data Node
  = Input Int
  | Output Int
  | Hidden Int
    deriving (Show, Eq, Ord)

type Link
  = (Node, Node)

data Gene
  = Gene
    { link         :: Link
    , weight       :: Float
    , enabled      :: Bool
    , innovationID :: Int
    }
    deriving (Show)

instance Eq Gene where
  gene1 == gene2
    = (innovationID gene1) == (innovationID gene2)

instance Ord Gene where
  compare gene1 gene2
    = compare (innovationID gene1) (innovationID gene2)

data Genome
  = Genome
    { genes   :: [Gene]
    , inputs  :: Int
    , outputs :: Int
    , hidden  :: Int
    }
    deriving (Show)

data SimulationState
  = SimulationState
    { gen         :: StdGen
    , innovations :: [Link]
    }
    deriving (Show)

type Mutation m
  = m -> State SimulationState m

chanceMutations :: Float -> Mutation a -> Mutation a -> Mutation a
chanceMutations chance mutationThen mutationElse mutable
  = do
    sim <- get
    let (value, gen') = randomR (0, 1) (gen sim) :: (Float, StdGen)
    put (sim {gen = gen'})
    if value <= chance
       then mutationThen mutable
       else mutationElse mutable

chanceMutation :: Float -> Mutation a -> Mutation a
chanceMutation chance mutationThen
  = chanceMutations chance mutationThen (\m -> state $ \s -> (m, s))

perturbGeneWeight :: Mutation Gene
perturbGeneWeight gene
  = state $ \sim ->
      let (offset, gen')
            = randomR (-maxPerturbAmount, maxPerturbAmount) (gen sim)
       in (gene {weight = weight gene + offset}, sim {gen = gen'})

reassignGeneWeight :: Mutation Gene
reassignGeneWeight gene
  = state $ \sim ->
      let (weight', gen') = randomR (0, 1) (gen sim)
       in (gene {weight = weight'}, sim {gen = gen'})

mutateWeight :: Mutation Gene
mutateWeight
  = chanceMutations perturbChance perturbGeneWeight reassignGeneWeight

mutateWeights :: Mutation [Gene]
mutateWeights
  = chanceMutation mutateWeightsChance $ mapM mutateWeight

setEnabledTo :: Bool -> Gene -> Gene
setEnabledTo isEnabled gene
  = gene {enabled = isEnabled}

reenableGenes :: Mutation [Gene]
reenableGenes
  = mapM $ chanceMutation reenableChance $ return . setEnabledTo True

getInnovationID :: Link -> State SimulationState Int
getInnovationID link
  = do
    sim <- get
    case getIndex link $ innovations sim of
      Just index -> return index
      Nothing -> do
        put (sim {innovations = innovations sim ++ [link]}) -- append to end
        return $ length $ innovations sim

pickRandomGene :: Genome -> State SimulationState Int
pickRandomGene genome
  = do
    sim <- get
    let geneCount     = length $ genes genome
        (index, gen') = randomR (0, geneCount - 1) $ gen sim
    put (sim {gen = gen'})
    return index

mutateNode :: Mutation Genome
mutateNode genome
  = do
    sim <- get
    index <- pickRandomGene genome
    let newNode           = Hidden $ hidden genome
        gene              = setEnabledTo False $ genes genome !! index
        (inNode, outNode) = link gene
        linkIn            = (inNode, newNode)
        linkOut           = (newNode, outNode)
    innovationIn  <- getInnovationID linkIn
    innovationOut <- getInnovationID linkOut
    let geneIn  = Gene linkIn 1.0 True innovationIn
        geneOut = Gene linkOut (weight gene) True innovationOut
    return $ genome
      { genes = geneOut : geneIn : (replaceAt index gene $ genes genome)
      , hidden = hidden genome + 1
      }

getIncommingNodes :: Genome -> Node -> [Node]
getIncommingNodes genome outputNode
  = let incomming
          = map (fst . link) $
            filter ((== outputNode) . snd . link) $
            genes genome
     in incomming ++ (concatMap (getIncommingNodes genome) incomming)

isCyclic :: Genome -> Link -> Bool
isCyclic genome (inNode, outNode)
  = outNode `elem` (getIncommingNodes genome inNode)

genValidLinks :: Genome -> [Link]
genValidLinks genome
  = let inputNodes  = map Input $ [0..inputs genome - 1]
        outputNodes = map Output $ [0..outputs genome - 1]
        hiddenNodes = map Hidden $ [0..hidden genome - 1]
        linkInputs  = inputNodes ++ hiddenNodes
        linkOutputs = outputNodes ++ hiddenNodes
        allLinks
          = [(linkIn, linkOut) | linkIn <- linkInputs, linkOut <- linkOutputs]
        existingLinks
          = map link $ genes genome
     in filter (\l -> (not $ isCyclic genome l) && (l `notElem` existingLinks)) allLinks

mutateLink :: Mutation Genome
mutateLink genome
  = do
    sim <- get
    case genValidLinks genome of
      []          -> return genome
      validLinks  -> do
        let (index, gen') = randomR (0, length validLinks - 1) $ gen sim
            newLink       = validLinks !! index
        put (sim {gen = gen'})
        innovationID <- getInnovationID newLink
        newGene <- reassignGeneWeight $ Gene newLink 0.0 True innovationID
        return (genome {genes = newGene : genes genome})

mutateGenome :: Mutation Genome
mutateGenome genome
  = do
    let mutation = (mutateWeights >=> reenableGenes)
    genes' <- mutation $ genes genome
    return $ genome {genes = genes'}

alignGenes :: [Gene] -> [Gene] -> [(Maybe Gene, Maybe Gene)]
alignGenes genes1 []
  = map (\gene -> (Just gene, Nothing)) genes1
alignGenes [] genes2
  = map (\gene -> (Nothing, Just gene)) genes2
alignGenes genes1@(gene1:genes1') genes2@(gene2:genes2')
  | gene1 < gene2 = (Just gene1, Nothing) : alignGenes genes1' genes2
  | gene1 > gene2 = (Nothing, Just gene2) : alignGenes genes1 genes2'
  | otherwise     = (Just gene1, Just gene2) : alignGenes genes1' genes2'
