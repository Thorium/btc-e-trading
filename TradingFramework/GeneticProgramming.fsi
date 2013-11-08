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

    type TreeNodeValue<'t> = { 
        BranchNumber: int
        LeafNumber: int
        NumberOfBranches: int
        NumberOfLeafs: int
        Data: 't
    }

    /// Branch contains a tuple of: value, children
    type TreeNode<'t, 'u> = 
        | Leaf of TreeNodeValue<'u>
        | Branch of TreeNodeValue<'t> * TreeNode<'t, 'u> list

    /// An instance of this is a program
    type EvaluationTree<'t, 'u> = { root: TreeNode<'t, 'u> }

    /// <summary>
    /// Fitness proportionate selection.
    /// </summary>
    val public selectWithRandomNumberGenerator: randomNumberGenerator:(unit -> double) -> population:'a list -> fitness:('a -> int) -> 'a

    /// <summary>
    /// Wrapper around selectWithRandomNumberGenerator where random number generator is supplied with System.Random's NextDouble method.
    /// </summary>
    val public select: population:'a list -> fitness:('a -> int) -> 'a

    /// <summary>
    /// Generates a program using the grow method.
    /// </summary>
    /// <param name="randomNumberGenerator">Function that's expected to generate a random number in the range of 0..argument-1</param>
    val public populateByGrowth: branchGenerator:(unit -> 'a) -> leafGenerator:(unit -> 'b) -> randomNumberGenerator:(int -> int) -> maxDepth:int -> maxChildren:int -> chanceOfLeaf:int -> EvaluationTree<'a, 'b>

    /// <summary>
    /// Combines two trees (programs) together at a random point. The root of the rhs tree replaces a node somewhere within the lhs tree.
    /// </summary>
    /// <param name="chanceOfLeafNode">1 in chanceOfLeafNode chance of the trees being combined on a leaf node. e.g. If you want a chance of 1 in 10 then you'd pass 10.</param>
    /// <param name="randomNumberGenerator">Function that's expected to generate a random number in the range of 0..argument-1</param>
    /// <param name="mutate">This function is applied to all nodes in the tree, if you return a new node then the node passed to the function will be replaced in the combined tree with the new node.</param>
    /// <returns>Combined tree, all the nodes in this tree are copies, so mutate the lhs or rhs trees will not mutate the combined tree.</returns>
    val public combine: lhsNode:TreeNode<'a,'b> -> lhsNodeToCombineOn:TreeNode<'a,'b> -> rhsNode:TreeNode<'a,'b> -> mutate:(TreeNode<'a,'b> -> TreeNode<'a,'b>) -> EvaluationTree<'a, 'b>

    /// <summary>
    /// Finds a random node within a tree.
    /// </summary>
    /// <param name="chanceOfLeafNode">1 in chanceOfLeafNode chance of the trees being combined on a leaf node. e.g. If you want a chance of 1 in 10 then you'd pass 10.</param>
    /// <param name="randomNumberGenerator">Function that's expected to generate a random number in the range of 0..argument-1</param>
    val public selectNode: chanceOfLeafNode:int -> randomNumberGenerator:(int -> int) -> tree:EvaluationTree<'a, 'b> -> TreeNode<'a,'b> option

    /// <summary>
    /// Cut and splice crossover of two trees lhs and rhs.
    /// </summary>
    /// <param name="selectNode">Function that's expected to return a node to perform the crossover on when given a tree.</param>
    /// <param name="mutate">This function is applied to all nodes in the trees, if you return a new node then the node passed to the function will be replaced in the crossed over trees with the new node.</param>
    val public crossover: lhs:EvaluationTree<'a, 'b> -> rhs:EvaluationTree<'a, 'b> -> selectNode:(EvaluationTree<'a, 'b> -> TreeNode<'a,'b> option) -> mutate:(TreeNode<'a,'b> -> TreeNode<'a,'b>) -> EvaluationTree<'a, 'b> * EvaluationTree<'a, 'b>