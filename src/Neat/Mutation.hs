module Neat.Mutation
  ( mutateGenome
  ) where

import Control.Monad       ((>=>))
import Control.Monad.State
import Neat.Base
import Neat.Utils          (getIndex, replaceAt)
import System.Random

type Chance
  = Float

type Mutation m
  = m -> State SimulationState m

chanceMutations :: Chance -> Mutation a -> Mutation a -> Mutation a
chanceMutations chance mutationThen mutationElse mutable
  = do
    sim <- get
    let (value, gen') = randomR (0, 1) (gen sim) :: (Float, StdGen)
    put (sim {gen = gen'})
    if value <= chance
       then mutationThen mutable
       else mutationElse mutable

chanceMutation :: Chance -> Mutation a -> Mutation a
chanceMutation chance mutationThen
  = chanceMutations chance mutationThen (\m -> state $ \s -> (m, s))

perturbGeneWeight :: Mutation Gene
perturbGeneWeight gene
  = state $ \sim ->
      let maxPerturbAmount = 0.2
          (offset, gen')
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
    where
      perturbChance = 0.9

mutateWeights :: Mutation Genome
mutateWeights
  = mapM mutateWeight

setEnabledTo :: Bool -> Gene -> Gene
setEnabledTo isEnabled gene
  = gene {enabled = isEnabled}

reenableGenes :: Mutation Genome
reenableGenes
  = mapM (return . setEnabledTo True)

getInnovationID :: Link -> State SimulationState Int
getInnovationID link
  = do
    sim <- get
    case getIndex link $ innovations sim of
      Just index -> return index
      Nothing -> do
        put (sim {innovations = innovations sim ++ [link]}) -- append to end
        return $ length $ innovations sim

pickRandomGene :: Organism -> State SimulationState Int
pickRandomGene organism
  = do
    sim <- get
    let geneCount     = length $ genome organism
        (index, gen') = randomR (0, geneCount - 1) $ gen sim
    put (sim {gen = gen'})
    return index

mutateNode :: Mutation Organism
mutateNode organism
  = do
    sim <- get
    index <- pickRandomGene organism
    let newNode           = Hidden $ hidden organism
        gene              = setEnabledTo False $ genome organism !! index
        (inNode, outNode) = link gene
        linkIn            = (inNode, newNode)
        linkOut           = (newNode, outNode)
    innovationIn  <- getInnovationID linkIn
    innovationOut <- getInnovationID linkOut
    let geneIn  = Gene linkIn 1.0 True innovationIn
        geneOut = Gene linkOut (weight gene) True innovationOut
    return $ organism
      { genome = geneOut : geneIn : (replaceAt index gene $ genome organism)
      , hidden = hidden organism + 1
      }

getIncommingNodes :: Organism -> Node -> [Node]
getIncommingNodes organism outputNode
  = let incomming
          = map (fst . link)
            $ filter ((== outputNode) . snd . link)
            $ genome organism
     in incomming ++ (concatMap (getIncommingNodes organism) incomming)

isCyclic :: Organism -> Link -> Bool
isCyclic organism (inNode, outNode)
  = outNode `elem` (getIncommingNodes organism inNode)

genValidLinks :: Organism -> [Link]
genValidLinks organism
  = let inputNodes  = map Input $ [0..inputs organism - 1]
        outputNodes = map Output $ [0..outputs organism - 1]
        hiddenNodes = map Hidden $ [0..hidden organism - 1]
        linkInputs  = inputNodes ++ hiddenNodes
        linkOutputs = outputNodes ++ hiddenNodes
        allLinks
          = [(linkIn, linkOut) | linkIn <- linkInputs, linkOut <- linkOutputs]
        existingLinks
          = map link $ genome organism
     in filter (\l -> (not $ isCyclic organism l) && (l `notElem` existingLinks)) allLinks

mutateLink :: Mutation Organism
mutateLink organism
  = do
    sim <- get
    case genValidLinks organism of
      []          -> return organism
      validLinks  -> do
        let (index, gen') = randomR (0, length validLinks - 1) $ gen sim
            newLink       = validLinks !! index
        put (sim {gen = gen'})
        innovationID <- getInnovationID newLink
        newGene <- reassignGeneWeight $ Gene newLink 0.0 True innovationID
        return (organism {genome = newGene : genome organism})

mutateGenome :: Mutation Organism
mutateGenome organism
  = do
    let genomeMutation
          = (chanceMutation 0.8 mutateWeights >=> chanceMutation 0.1 reenableGenes)
        organismMutation
          = (chanceMutation 0.03 mutateNode >=> chanceMutation 0.05 mutateNode)
    genome' <- genomeMutation $ genome organism
    let organism' = organism {genome = genome'}
    organismMutation organism'
