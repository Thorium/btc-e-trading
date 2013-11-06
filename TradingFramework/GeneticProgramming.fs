(*
    Copyright (C) 2013  Matthew Mcveigh

    This file is part of F# Unaffiliated BTC-E Trading Framework.

    F# Unaffiliated BTC-E Trading Framework is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    F# Unaffiliated BTC-E Trading Framework is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with F# Unaffiliated BTC-E Trading Framework. If not, see <http://www.gnu.org/licenses/>.
*)

namespace TradingFramework

module GeneticProgramming =

    open System

    open TaLib

    type TreeNodeValue<'t> = { 
        BranchNumber: int
        LeafNumber: int
        NumberOfBranches: int
        NumberOfLeafs: int
        Data: 't
    }

    type TreeNode<'t, 'u> = 
        | Leaf of TreeNodeValue<'u>
        | Branch of TreeNodeValue<'t> * TreeNode<'t, 'u> list

    type EvaluationTree<'t, 'u> = { root: TreeNode<'t, 'u> }

    /// Fitness must be return an integer greater than zero
    let selectWithRandomNumberGenerator randomNumberGenerator (population: 'a list) fitness =
        assert (population.Length > 1)

        let fitness program = 
            let fitness = fitness program
            assert(fitness > 0)
            double(fitness)

        let sumOfFitness = List.sumBy (fun program -> fitness(program)) population
    
        let populationWithProbability = List.map (fun program -> (program, fitness(program) / sumOfFitness)) population

        // Descending order
        let populationWithProbability = List.sortBy (fun (_, probability) -> probability) populationWithProbability
    
        let randomNumber = randomNumberGenerator()

        assert (randomNumber >= 0.0 && randomNumber < 1.0)

        let rec selectPopulate populationWithProbability accumulator =
            match populationWithProbability with
                | (program, probability) :: tail ->  
                    if randomNumber < probability + accumulator then
                        program
                    else
                        selectPopulate tail (probability + accumulator)
                | [] -> 
                    failwith "Unreachable"

        selectPopulate populationWithProbability 0.0

    let select population fitness =
        let randomNumberGenerator = new Random()
        selectWithRandomNumberGenerator (fun () -> randomNumberGenerator.NextDouble()) population fitness

    let generateChildren grow numberOfChildren depth branchesToTheLeft leavesToTheLeft  =
        let rec generateChild numberOfChildren children =
            let branchesToTheLeft = (List.sumBy (fun x -> match x with | Branch(x, _) -> x.NumberOfBranches | _ -> 0) children) + branchesToTheLeft
            let leavesToTheLeft = (List.sumBy (fun x -> match x with | Branch(x, _) -> x.NumberOfLeafs | Leaf(_) -> 1) children) + leavesToTheLeft

            let child = grow (depth + 1) branchesToTheLeft leavesToTheLeft

            if numberOfChildren = 1 then
                [child]
            else
                child :: generateChild (numberOfChildren - 1) (child :: children)

        generateChild numberOfChildren []

    let createNode createBranchFunction createLeafFunction branchesToTheLeft leavesToTheLeft = function
        | Some(children) -> 
            let branchesInChildren = (List.sumBy (fun x -> match x with | Branch(x, _) -> x.NumberOfBranches | _ -> 0) children)
            Branch({
                    BranchNumber = branchesToTheLeft + branchesInChildren + 1
                    LeafNumber = 0
                    NumberOfBranches = branchesInChildren + 1
                    NumberOfLeafs = List.sumBy (fun x -> match x with | Branch(x, _) -> x.NumberOfLeafs | Leaf(_) -> 1) children
                    Data = createBranchFunction()
            }, children)
        | None ->
            Leaf({
                    BranchNumber = 0
                    LeafNumber = leavesToTheLeft + 1
                    NumberOfBranches = 0
                    NumberOfLeafs = 1
                    Data = createLeafFunction()
            })

    let populateByGrowth branchGenerator leafGenerator randomNumberGenerator maxDepth maxChildren chanceOfLeaf =
        assert(maxDepth > 0 && maxChildren > 0 && chanceOfLeaf > 0)

        let rec grow depth branchesToTheLeft leavesToTheLeft =
            let createNode = createNode branchGenerator leafGenerator branchesToTheLeft leavesToTheLeft

            if depth = maxDepth || randomNumberGenerator chanceOfLeaf = 0 then
                createNode None
            else
                let numberOfChildren = (randomNumberGenerator maxChildren) + 1
                let children = generateChildren grow numberOfChildren (depth + 1) branchesToTheLeft leavesToTheLeft

                createNode <| Some(children)

        { root = grow 0 0 0 }

    type NodePosition =
        {
            BranchNumber: int
            LeafNumber: int
            NumberOfBranches: int
            NumberOfLeafs: int
        }

    let extractNodeValue = function
        | Leaf(value) -> 
            {
                BranchNumber = value.BranchNumber
                LeafNumber = value.LeafNumber
                NumberOfBranches = value.NumberOfBranches
                NumberOfLeafs = value.NumberOfLeafs
            }
        | Branch(value, _) -> 
            {
                BranchNumber = value.BranchNumber
                LeafNumber = value.LeafNumber
                NumberOfBranches = value.NumberOfBranches
                NumberOfLeafs = value.NumberOfLeafs
            }

    let rec copyChildren provideOwnCopy leavesToTheLeft branchesToTheLeft mutate copyNode = function
        | child :: tail ->
            let child = copyNode provideOwnCopy leavesToTheLeft branchesToTheLeft child mutate

            let childValue = extractNodeValue child
            let leavesToTheLeft = leavesToTheLeft + childValue.NumberOfLeafs
            let branchesToTheLeft = branchesToTheLeft + childValue.NumberOfBranches

            child :: copyChildren provideOwnCopy leavesToTheLeft branchesToTheLeft mutate copyNode tail
        | [] -> []

    let rec copyNode copyLeaf copyBranch provideOwnCopy leavesToTheLeft branchesToTheLeft node mutate =
        match node with
            | Leaf(node) -> 
                match provideOwnCopy node.LeafNumber node.BranchNumber leavesToTheLeft branchesToTheLeft true with
                    | Some(copy) -> copy
                    | None ->
                        mutate <| copyLeaf leavesToTheLeft node
            | Branch(node, children) -> 
                match provideOwnCopy node.LeafNumber node.BranchNumber leavesToTheLeft branchesToTheLeft false with
                    | Some(copy) -> copy
                    | None ->
                        mutate <| copyBranch leavesToTheLeft branchesToTheLeft node children provideOwnCopy mutate

    let copyLeaf leavesToTheLeft leafNode =
        let leaf = {
            BranchNumber = 0
            LeafNumber = leavesToTheLeft + 1
            NumberOfBranches = 0
            NumberOfLeafs = 1
            Data = leafNode.Data
        }
        TreeNode.Leaf(leaf)

    let rec copyBranch leavesToTheLeft branchesToTheLeft branchNode children provideOwnCopy mutate =
        let copyNode = copyNode copyLeaf copyBranch
        let children = copyChildren provideOwnCopy leavesToTheLeft branchesToTheLeft mutate copyNode children
        let branchesInChildren = List.sumBy (fun x -> match x with | TreeNode.Branch(x, _) -> x.NumberOfBranches | _ -> 0) children
        let branchNode = {
            BranchNumber = branchesToTheLeft + branchesInChildren + 1
            LeafNumber = 0
            NumberOfBranches = branchesInChildren + 1
            NumberOfLeafs = List.sumBy (fun x -> match x with | TreeNode.Branch(x, _) -> x.NumberOfLeafs | TreeNode.Leaf(_) -> 1) children
            Data = branchNode.Data
        }
        TreeNode.Branch(branchNode, children)

    /// <summary>
    /// Combines two trees (programs) together at a random point. The root of the rhs tree replaces a node somewhere within the lhs tree.
    /// </summary>
    /// <param name="chanceOfLeafNode">1 in chanceOfLeafNode chance of the trees being combined on a leaf node. e.g. If you want a chance of 1 in 10 then you'd pass 10.</param>
    /// <param name="randomNumberGenerator">Function that's expected to generate a random number in the range of 0..argument-1</param>
    /// <param name="mutate">This function is applied to all nodes in the tree, if you return a new node then the node passed to the function will be replaced in the combined tree with the new node.</param>
    /// <returns>Combined tree, all the nodes in this tree are copies, so mutate the lhs or rhs trees will not mutate the combined tree.</returns>
    let combine lhs rhs chanceOfLeafNode randomNumberGenerator mutate =
        let combineOnLeaf = randomNumberGenerator chanceOfLeafNode = 1

        let root = extractNodeValue lhs.root

        let nodeToSelect = 
            if combineOnLeaf then
                randomNumberGenerator root.NumberOfLeafs
            else
                randomNumberGenerator root.NumberOfBranches

        let nodeToSelect = nodeToSelect + 1 // range of 1..n rather than 0..n - 1

        let provideOwnCopy leaf branch leafsToTheLeft branchesToTheLeft isLeaf = 
            if (combineOnLeaf && isLeaf && nodeToSelect = leaf) || (not combineOnLeaf && not isLeaf && nodeToSelect = branch) then
                Some(copyNode copyLeaf copyBranch (fun _ _ _ _ _ -> None) leafsToTheLeft branchesToTheLeft rhs.root mutate)
            else
                None

        { root = copyNode copyLeaf copyBranch provideOwnCopy 0 0 lhs.root mutate }