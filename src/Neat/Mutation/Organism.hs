module Neat.Mutation.Organism where

import Neat.Mutation.Base
import Neat.Mutation.Gene
import Neat.Mutation.Utils

getIncommingNodes :: Organism -> Node -> [Node]
getIncommingNodes organism outputNode
  = let incomming
          = map (fst . link)
            $ filter ((== outputNode) . snd . link)
            $ genome organism
     in incomming ++ (concatMap (getIncommingNodes organism) incomming)

isNotCyclic :: Organism -> Link -> Bool
isNotCyclic organism (inNode, outNode)
  = inNode /= outNode && outNode `notElem` (getIncommingNodes organism inNode)

genValidLinks :: Organism -> [Link]
genValidLinks organism
  = let hiddenNodes = map Hidden $ [0..hidden organism - 1]
        linkInputs  = (map Input $ [0..inputs organism -1]) ++ hiddenNodes
        linkOutputs = (map Output $ [0..outputs organism - 1]) ++ hiddenNodes
        allLinks
          = [(linkIn, linkOut) | linkIn <- linkInputs, linkOut <- linkOutputs]
        existingLinks
          = map link $ genome organism
     in filter (\l -> (isNotCyclic organism l) && (l `notElem` existingLinks)) allLinks

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
  = case genome organism of
      [] -> return organism
      _  -> do
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


mutateWeights :: Mutation Organism
mutateWeights organism
  = do
      genome' <- mapM mutateWeight $ genome organism
      return (organism {genome = genome'})

mutateReenableGenes :: Mutation Organism
mutateReenableGenes organism
  = do
      genome' <- mapM reenableGene $ genome organism
      return (organism {genome = genome'})

mutate :: Mutation Organism
mutate
  = chanceMutation 0.8 mutateWeights >=>
    chanceMutation 0.1 mutateReenableGenes >=>
    chanceMutation 0.03 mutateNode >=>
    chanceMutation 0.05 mutateNode
