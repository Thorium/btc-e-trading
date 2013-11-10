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

module TestGeneticProgramming

open NUnit.Framework

open TradingFramework.GeneticProgramming

let printTree tree printData =
    let rec printChildren printNode depth = function
        | child :: tail -> 
            printNode depth child
            printChildren printNode depth tail
        | [] -> ()

    let rec printNode depth node =
        let rec addSpacing spacing = function
            | 0 -> spacing
            | spaces -> addSpacing ("    " + spacing) (spaces - 1)

        match node with
            | TreeNode.Leaf(node) -> 
                let output = "LEAF " + "Number of branches: " + node.NumberOfBranches.ToString()
                             + " Number of leaves: " + node.NumberOfLeafs.ToString()
                             + " Branch number: " + node.BranchNumber.ToString()
                             + " Leaf number: " + node.LeafNumber.ToString()
                             + " Data: " + (printData node.Data)
                System.Console.WriteLine(addSpacing output depth)
            | TreeNode.Branch(node, children) -> 
                printChildren printNode (depth + 1) children
                let output = "BRANCH " + "Number of branches: " + node.NumberOfBranches.ToString()
                             + " Number of leaves: " + node.NumberOfLeafs.ToString()
                             + " Branch number: " + node.BranchNumber.ToString()
                             + " Leaf number: " + node.LeafNumber.ToString()
                             + " Number of Children: " + children.Length.ToString()
                             + " Data: " + (printData node.Data)
                System.Console.WriteLine(addSpacing output depth)
        
    printNode 0 tree.root

[<TestFixture>]
type TestGeneticProgramming() = class
(*
    [<Test>]
    member self.debugPrintGrow() = 
        let rng = new System.Random()
        let rngFunc = (fun x -> rng.Next(x))

        let f = PatternRecognitionGP.growPatternRecogniserTree rngFunc 6 5 10
        printTree f (fun _ -> "")
        ()

    [<Test>]
    member self.debugPrintCombine() = 
        let rng = new System.Random()
        let rngFunc = (fun x -> rng.Next(x))

        System.Console.WriteLine("\nTREE 1\n")
        let f = PatternRecognitionGP.growPatternRecogniserTree rngFunc 10 2 100
        printTree f

        System.Console.WriteLine("\nTREE 2\n")
        let y = PatternRecognitionGP.growPatternRecogniserTree rngFunc 10 2 100
        printTree y

        System.Console.WriteLine("\nCOMBINED TREE\n")
        printTree (GeneticProgramming.combine f y 10 rngFunc (fun x -> x))
        *)
    [<Test>]
    member self.grow() = 

        let i = ref 0

        let array = [| 0..100 |]

        let generator = (fun _ -> 
            incr i
            array.[!i])

        let maxDepth, maxChildren, chanceOfLeaf = 5, 3, 1

        let randomNumberGenerator = fun x -> 
            if x = maxChildren then 0
            else if x = chanceOfLeaf then 0
            else failwith "Unexpected value given to random number generator."

        let evaluationTree = populateByGrowth generator generator randomNumberGenerator maxDepth maxChildren chanceOfLeaf



        Assert.Fail()

    [<Test>]
    member self.combineOnLeaf() = 
        let leaf1 = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=2;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let leaf3 = Leaf({BranchNumber=0;LeafNumber=3;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch1 = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=3;Data=1}, [leaf1;leaf2;leaf3])

        let leaf1 = Leaf({BranchNumber=0;LeafNumber=4;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=5;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let leaf3 = Leaf({BranchNumber=0;LeafNumber=6;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch2 = Branch({BranchNumber=2;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=3;Data=1}, [leaf1;leaf2;leaf3])
        let nodeToCombineOn = leaf3

        let leaf1 = Leaf({BranchNumber=0;LeafNumber=7;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=8;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let leaf3 = Leaf({BranchNumber=0;LeafNumber=9;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch3 = Branch({BranchNumber=3;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=3;Data=1}, [leaf1;leaf2;leaf3])
        
        let lhs = Branch({BranchNumber=4;LeafNumber=0;NumberOfBranches=4;NumberOfLeafs=9;Data=1}, [branch1;branch2;branch3])

        let lhs = { root = lhs }

        let leaf1 = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=2;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf3 = Leaf({BranchNumber=0;LeafNumber=3;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let branch1 = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=3;Data=2}, [leaf1;leaf2;leaf3])

        let leaf1 = Leaf({BranchNumber=0;LeafNumber=4;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=5;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf3 = Leaf({BranchNumber=0;LeafNumber=6;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let branch2 = Branch({BranchNumber=2;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=3;Data=2}, [leaf1;leaf2;leaf3])

        let leaf1 = Leaf({BranchNumber=0;LeafNumber=7;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=8;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf3 = Leaf({BranchNumber=0;LeafNumber=9;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let branch3 = Branch({BranchNumber=3;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=3;Data=2}, [leaf1;leaf2;leaf3])

        let rhs = Branch({BranchNumber=4;LeafNumber=0;NumberOfBranches=4;NumberOfLeafs=9;Data=2}, [branch1;branch2;branch3])

        let rhs = { root = rhs }

        let combinedTree = combine lhs.root nodeToCombineOn rhs.root (fun x -> x)

        let leaf1 = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=2;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let leaf3 = Leaf({BranchNumber=0;LeafNumber=3;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch1 = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=3;Data=1}, [leaf1;leaf2;leaf3])

        let leaf1 = Leaf({BranchNumber=0;LeafNumber=6;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=7;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf3 = Leaf({BranchNumber=0;LeafNumber=8;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let innerbranch1 = Branch({BranchNumber=2;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=3;Data=2}, [leaf1;leaf2;leaf3])

        let leaf1 = Leaf({BranchNumber=0;LeafNumber=9;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=10;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf3 = Leaf({BranchNumber=0;LeafNumber=11;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let innerbranch2 = Branch({BranchNumber=3;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=3;Data=2}, [leaf1;leaf2;leaf3])

        let leaf1 = Leaf({BranchNumber=0;LeafNumber=12;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=13;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf3 = Leaf({BranchNumber=0;LeafNumber=14;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let innerbranch3 = Branch({BranchNumber=4;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=3;Data=2}, [leaf1;leaf2;leaf3])

        let leaf1 = Leaf({BranchNumber=0;LeafNumber=4;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=5;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let innerbranch = Branch({BranchNumber=5;LeafNumber=0;NumberOfBranches=4;NumberOfLeafs=9;Data=2}, [innerbranch1;innerbranch2;innerbranch3])
        let branch2 = Branch({BranchNumber=6;LeafNumber=0;NumberOfBranches=5;NumberOfLeafs=11;Data=1}, [leaf1;leaf2;innerbranch])
        
        let leaf1 = Leaf({BranchNumber=0;LeafNumber=15;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=16;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let leaf3 = Leaf({BranchNumber=0;LeafNumber=17;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch3 = Branch({BranchNumber=7;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=3;Data=1}, [leaf1;leaf2;leaf3])

        let resultingTree = Branch({BranchNumber=8;LeafNumber=0;NumberOfBranches=8;NumberOfLeafs=17;Data=1}, [branch1;branch2;branch3])

        let resultingTree = { root = resultingTree }

        Assert.AreEqual(resultingTree, combinedTree)

    [<Test>]
    member self.combineOnBranch() = 
        let leaf1 = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=2;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let leaf3 = Leaf({BranchNumber=0;LeafNumber=3;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch1 = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=3;Data=1}, [leaf1;leaf2;leaf3])

        let leaf1 = Leaf({BranchNumber=0;LeafNumber=4;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=5;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let leaf3 = Leaf({BranchNumber=0;LeafNumber=6;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch2 = Branch({BranchNumber=2;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=3;Data=1}, [leaf1;leaf2;leaf3])

        let leaf1 = Leaf({BranchNumber=0;LeafNumber=7;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=8;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let leaf3 = Leaf({BranchNumber=0;LeafNumber=9;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch3 = Branch({BranchNumber=3;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=3;Data=1}, [leaf1;leaf2;leaf3])
        let nodeToCombineOn = branch3

        let lhs = Branch({BranchNumber=4;LeafNumber=0;NumberOfBranches=4;NumberOfLeafs=9;Data=1}, [branch1;branch2;branch3])

        let lhs = { root = lhs }

        let leaf1 = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=2;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf3 = Leaf({BranchNumber=0;LeafNumber=3;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let branch1 = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=3;Data=2}, [leaf1;leaf2;leaf3])

        let leaf1 = Leaf({BranchNumber=0;LeafNumber=4;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=5;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf3 = Leaf({BranchNumber=0;LeafNumber=6;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let branch2 = Branch({BranchNumber=2;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=3;Data=2}, [leaf1;leaf2;leaf3])

        let leaf1 = Leaf({BranchNumber=0;LeafNumber=7;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=8;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf3 = Leaf({BranchNumber=0;LeafNumber=9;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let branch3 = Branch({BranchNumber=3;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=3;Data=2}, [leaf1;leaf2;leaf3])

        let rhs = Branch({BranchNumber=4;LeafNumber=0;NumberOfBranches=4;NumberOfLeafs=9;Data=2}, [branch1;branch2;branch3])

        let rhs = { root = rhs }

        let combinedTree = combine lhs.root nodeToCombineOn rhs.root (fun x -> x)

        let leaf1 = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=2;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let leaf3 = Leaf({BranchNumber=0;LeafNumber=3;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch1 = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=3;Data=1}, [leaf1;leaf2;leaf3])

        let leaf1 = Leaf({BranchNumber=0;LeafNumber=4;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=5;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let leaf3 = Leaf({BranchNumber=0;LeafNumber=6;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch2 = Branch({BranchNumber=2;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=3;Data=1}, [leaf1;leaf2;leaf3])

        let leaf1 = Leaf({BranchNumber=0;LeafNumber=7;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=8;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf3 = Leaf({BranchNumber=0;LeafNumber=9;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let innerbranch1 = Branch({BranchNumber=3;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=3;Data=2}, [leaf1;leaf2;leaf3])

        let leaf1 = Leaf({BranchNumber=0;LeafNumber=10;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=11;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf3 = Leaf({BranchNumber=0;LeafNumber=12;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let innerbranch2 = Branch({BranchNumber=4;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=3;Data=2}, [leaf1;leaf2;leaf3])

        let leaf1 = Leaf({BranchNumber=0;LeafNumber=13;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=14;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let leaf3 = Leaf({BranchNumber=0;LeafNumber=15;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let innerbranch3 = Branch({BranchNumber=5;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=3;Data=2}, [leaf1;leaf2;leaf3])

        let branch3 = Branch({BranchNumber=6;LeafNumber=0;NumberOfBranches=4;NumberOfLeafs=9;Data=2}, [innerbranch1;innerbranch2;innerbranch3])

        let resultingTree = Branch({BranchNumber=7;LeafNumber=0;NumberOfBranches=7;NumberOfLeafs=15;Data=1}, [branch1;branch2;branch3])

        let resultingTree = { root = resultingTree }

        Assert.AreEqual(resultingTree, combinedTree)
        (*
    [<Test>]
    member self.combineBranchRootToTree() = 
        let leafNode = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=1})

        let rootNode = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=1}, [leafNode])

        let tree = { root = rootNode }

        let combinedTree = combine tree tree 999 (fun x -> x) (fun x -> x)

        Assert.Fail()

    [<Test>]
    member self.combineLeafRootToTree() = 
        let leafNode = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=1})

        let rootNode = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=1}, [leafNode])

        let tree = { root = rootNode }

        let combinedTree = combine tree tree 999 (fun x -> x) (fun x -> x)

        Assert.Fail()
        *)
    [<Test>]
    member self.combineOnFirstBranch() = 
        let leaf = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch1 = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=1}, [leaf])
        let nodeToCombineOn = branch1

        let leaf = Leaf({BranchNumber=0;LeafNumber=2;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch2 = Branch({BranchNumber=2;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=1}, [leaf])

        let lhs = Branch({BranchNumber=3;LeafNumber=0;NumberOfBranches=3;NumberOfLeafs=2;Data=1}, [branch1;branch2])
        let lhs = { root = lhs }

        let leaf = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let branch1 = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=2}, [leaf])

        let leaf = Leaf({BranchNumber=0;LeafNumber=2;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let branch2 = Branch({BranchNumber=2;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=2}, [leaf])

        let rhs = Branch({BranchNumber=3;LeafNumber=0;NumberOfBranches=3;NumberOfLeafs=2;Data=2}, [branch1;branch2])
        let rhs = { root = rhs }

        let combinedTree = combine lhs.root nodeToCombineOn rhs.root (fun x -> x)

        let leaf = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let branch1 = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=2}, [leaf])

        let leaf = Leaf({BranchNumber=0;LeafNumber=2;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let branch2 = Branch({BranchNumber=2;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=2}, [leaf])

        let rhs = Branch({BranchNumber=3;LeafNumber=0;NumberOfBranches=3;NumberOfLeafs=2;Data=2}, [branch1;branch2])

        let leaf = Leaf({BranchNumber=0;LeafNumber=3;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch2 = Branch({BranchNumber=4;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=1}, [leaf])

        let lhs = Branch({BranchNumber=5;LeafNumber=0;NumberOfBranches=5;NumberOfLeafs=3;Data=1}, [rhs;branch2])
        let resultingTree = { root = lhs }

        printTree combinedTree (fun x -> x.ToString())
        printTree resultingTree (fun x -> x.ToString())

        Assert.AreEqual(resultingTree, combinedTree)

    [<Test>]
    member self.combineOnFirstLeaf() = 
        let leaf = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let nodeToCombineOn = leaf;
        let branch1 = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=1}, [leaf])

        let leaf = Leaf({BranchNumber=0;LeafNumber=2;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch2 = Branch({BranchNumber=2;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=1}, [leaf])

        let lhs = Branch({BranchNumber=3;LeafNumber=0;NumberOfBranches=3;NumberOfLeafs=2;Data=1}, [branch1;branch2])
        let lhs = { root = lhs }

        let leaf = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let branch1 = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=2}, [leaf])

        let leaf = Leaf({BranchNumber=0;LeafNumber=2;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let branch2 = Branch({BranchNumber=2;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=2}, [leaf])

        let rhs = Branch({BranchNumber=3;LeafNumber=0;NumberOfBranches=3;NumberOfLeafs=2;Data=2}, [branch1;branch2])
        let rhs = { root = rhs }

        let combinedTree = combine lhs.root nodeToCombineOn rhs.root (fun x -> x)

        let leaf = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let branch1 = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=2}, [leaf])

        let leaf = Leaf({BranchNumber=0;LeafNumber=2;NumberOfBranches=0;NumberOfLeafs=1;Data=2})
        let branch2 = Branch({BranchNumber=2;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=2}, [leaf])

        let rhs = Branch({BranchNumber=3;LeafNumber=0;NumberOfBranches=3;NumberOfLeafs=2;Data=2}, [branch1;branch2])

        let branch1 = Branch({BranchNumber=4;LeafNumber=0;NumberOfBranches=4;NumberOfLeafs=2;Data=1}, [rhs])

        let leaf = Leaf({BranchNumber=0;LeafNumber=3;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch2 = Branch({BranchNumber=5;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=1}, [leaf])

        let lhs = Branch({BranchNumber=6;LeafNumber=0;NumberOfBranches=6;NumberOfLeafs=3;Data=1}, [branch1;branch2])
        let resultingTree = { root = lhs }

        Assert.AreEqual(resultingTree, combinedTree)

    [<Test>]
    member self.combineAndMutate() = 
        let leaf = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch1 = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=1}, [leaf])
        let nodeToCombineOn = branch1

        let leaf = Leaf({BranchNumber=0;LeafNumber=2;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch2 = Branch({BranchNumber=2;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=1}, [leaf])

        let lhs = Branch({BranchNumber=3;LeafNumber=0;NumberOfBranches=3;NumberOfLeafs=2;Data=1}, [branch1;branch2])
        let lhs = { root = lhs }

        let leaf1 = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=10})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=2;NumberOfBranches=0;NumberOfLeafs=1;Data=10})
        let branch1 = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=2;Data=10}, [leaf1;leaf2])

        let leaf = Leaf({BranchNumber=0;LeafNumber=3;NumberOfBranches=0;NumberOfLeafs=1;Data=10})
        let branch2 = Branch({BranchNumber=2;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=10}, [leaf])

        let rhs = Branch({BranchNumber=3;LeafNumber=0;NumberOfBranches=3;NumberOfLeafs=3;Data=10}, [branch1;branch2])
        let rhs = { root = rhs }

        let mutate = (fun (x: TreeNode<int, int>) -> 
            match x with
                | Leaf(v) -> 
                    Leaf({
                            BranchNumber = v.BranchNumber
                            LeafNumber = v.LeafNumber
                            NumberOfBranches = v.NumberOfBranches
                            NumberOfLeafs = v.NumberOfLeafs
                            Data = v.Data * 10
                    })
                | Branch(v, c) -> 
                    Branch({
                            BranchNumber = v.BranchNumber
                            LeafNumber = v.LeafNumber
                            NumberOfBranches = v.NumberOfBranches
                            NumberOfLeafs = v.NumberOfLeafs
                            Data = v.Data * 10
                    }, c))

        // Combine on first branch
        let combinedTree = combine lhs.root nodeToCombineOn rhs.root mutate

        let leaf1 = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=100})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=2;NumberOfBranches=0;NumberOfLeafs=1;Data=100})
        let branch1 = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=2;Data=100}, [leaf1;leaf2])

        let leaf = Leaf({BranchNumber=0;LeafNumber=3;NumberOfBranches=0;NumberOfLeafs=1;Data=100})
        let branch2 = Branch({BranchNumber=2;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=100}, [leaf])

        let branch1 = Branch({BranchNumber=3;LeafNumber=0;NumberOfBranches=3;NumberOfLeafs=3;Data=100}, [branch1;branch2])

        let leaf = Leaf({BranchNumber=0;LeafNumber=4;NumberOfBranches=0;NumberOfLeafs=1;Data=10})
        let branch2 = Branch({BranchNumber=4;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=10}, [leaf])

        let rootNode = Branch({BranchNumber=5;LeafNumber=0;NumberOfBranches=5;NumberOfLeafs=4;Data=10}, [branch1;branch2])

        let resultingTree = { root = rootNode }

        printTree combinedTree (fun x -> x.ToString())

        System.Console.WriteLine("")

        printTree resultingTree (fun x -> x.ToString())

        Assert.AreEqual(resultingTree, combinedTree)

    [<Test>]
    member self.crossover() = 
        let leaf = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch1 = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=1}, [leaf])
        let lhsNodeToCombineOn = branch1

        let leaf = Leaf({BranchNumber=0;LeafNumber=2;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch2 = Branch({BranchNumber=2;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=1}, [leaf])

        let lhs = Branch({BranchNumber=3;LeafNumber=0;NumberOfBranches=3;NumberOfLeafs=2;Data=1}, [branch1;branch2])
        let lhs = { root = lhs }

        let leaf1 = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=10})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=2;NumberOfBranches=0;NumberOfLeafs=1;Data=10})
        let branch1 = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=2;Data=10}, [leaf1;leaf2])
        let rhsNodeToCombineOn = branch1

        let leaf = Leaf({BranchNumber=0;LeafNumber=3;NumberOfBranches=0;NumberOfLeafs=1;Data=10})
        let branch2 = Branch({BranchNumber=2;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=10}, [leaf])

        let rhs = Branch({BranchNumber=3;LeafNumber=0;NumberOfBranches=3;NumberOfLeafs=3;Data=10}, [branch1;branch2])
        let rhs = { root = rhs }

        let (leftTree, rightTree) = crossover lhs rhs (fun x -> 
            if x = lhs then Some(lhsNodeToCombineOn) else Some(rhsNodeToCombineOn)) (fun x -> x)

        // Left tree
        let leaf1 = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=10})
        let leaf2 = Leaf({BranchNumber=0;LeafNumber=2;NumberOfBranches=0;NumberOfLeafs=1;Data=10})
        let branch1 = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=2;Data=10}, [leaf1;leaf2])

        let leaf = Leaf({BranchNumber=0;LeafNumber=3;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch2 = Branch({BranchNumber=2;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=1}, [leaf])

        let lhs = Branch({BranchNumber=3;LeafNumber=0;NumberOfBranches=3;NumberOfLeafs=3;Data=1}, [branch1;branch2])
        let lhs = { root = lhs }

        Assert.AreEqual(lhs, leftTree)

        // Right tree
        let leaf = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch1 = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=1}, [leaf])

        let leaf = Leaf({BranchNumber=0;LeafNumber=2;NumberOfBranches=0;NumberOfLeafs=1;Data=10})
        let branch2 = Branch({BranchNumber=2;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=10}, [leaf])

        let rhs = Branch({BranchNumber=3;LeafNumber=0;NumberOfBranches=3;NumberOfLeafs=2;Data=10}, [branch1;branch2])
        let rhs = { root = rhs }

        Assert.AreEqual(rhs, rightTree)

    [<Test>]
    member self.selectFirstLeafNode() = 
        let (leaf1: TreeNode<int, int>) = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch1 = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=1}, [leaf1])

        let leaf2 = Leaf({BranchNumber=0;LeafNumber=2;NumberOfBranches=0;NumberOfLeafs=1;Data=10})
        let branch2 = Branch({BranchNumber=2;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=10}, [leaf2])

        let root = Branch({BranchNumber=3;LeafNumber=0;NumberOfBranches=3;NumberOfLeafs=2;Data=10}, [branch1;branch2])
        let tree = { root = root }

        let selectedNode = selectNode 999 (fun x -> if x = 999 then 1 else 0) tree

        let print = function
        | Some(node) -> 
            match node with
            | Leaf(node) -> node.BranchNumber.ToString() + " " + node.LeafNumber.ToString() + " " + node.Data.ToString() + " " + node.NumberOfLeafs.ToString() + " " + node.NumberOfBranches.ToString()
            | _ -> ""
        | _ -> ""

        System.Console.WriteLine(print selectedNode)
        System.Console.WriteLine(print <| Some(leaf1))

        Assert.AreEqual(Some(leaf1), selectedNode)

    [<Test>]
    member self.selectFirstBranchNode() = 
        let leaf = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch1 = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=1}, [leaf])

        let leaf = Leaf({BranchNumber=0;LeafNumber=2;NumberOfBranches=0;NumberOfLeafs=1;Data=10})
        let branch2 = Branch({BranchNumber=2;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=10}, [leaf])

        let root = Branch({BranchNumber=3;LeafNumber=0;NumberOfBranches=3;NumberOfLeafs=2;Data=10}, [branch1;branch2])
        let tree = { root = root }

        let selectedNode = selectNode 999 (fun x -> 0) tree

        Assert.AreEqual(Some(branch1), selectedNode)

    [<Test>]
    member self.selectNoNode() = 
        let leaf = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch1 = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=1}, [leaf])

        let leaf = Leaf({BranchNumber=0;LeafNumber=2;NumberOfBranches=0;NumberOfLeafs=1;Data=10})
        let branch2 = Branch({BranchNumber=2;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=10}, [leaf])

        let root = Branch({BranchNumber=3;LeafNumber=0;NumberOfBranches=3;NumberOfLeafs=2;Data=10}, [branch1;branch2])
        let tree = { root = root }

        // Branch
        Assert.Throws((fun _ -> (selectNode 999 (fun x -> 3) tree) |> ignore)) |> ignore

        // Leaf
        Assert.Throws((fun _ -> (selectNode 999 (fun x -> if x = 999 then 1 else 2) tree) |> ignore)) |> ignore

    [<Test>]
    member self.selectRootNode() = 
        let leaf = Leaf({BranchNumber=0;LeafNumber=1;NumberOfBranches=0;NumberOfLeafs=1;Data=1})
        let branch1 = Branch({BranchNumber=1;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=1}, [leaf])

        let leaf = Leaf({BranchNumber=0;LeafNumber=2;NumberOfBranches=0;NumberOfLeafs=1;Data=10})
        let branch2 = Branch({BranchNumber=2;LeafNumber=0;NumberOfBranches=1;NumberOfLeafs=1;Data=10}, [leaf])

        let root = Branch({BranchNumber=3;LeafNumber=0;NumberOfBranches=3;NumberOfLeafs=2;Data=10}, [branch1;branch2])
        let tree = { root = root }

        let selectedNode = selectNode 999 (fun x -> 2) tree

        Assert.AreEqual(Some(root), selectedNode)

    [<Test>]
    member self.select() = 
        let getFitness (fitnessValues: 'a []) (selectionValues: int list) value = 
            let i = List.findIndex (fun v -> v = value) selectionValues
            fitnessValues.[i]

        let values = [1;2;3;4]

        let fitness = getFitness [| 10;20;30;40 |] values

        let selectedValue = selectWithRandomNumberGenerator (fun _ -> 0.0) values fitness
        Assert.AreEqual(1, selectedValue)

        let selectedValue = selectWithRandomNumberGenerator (fun _ -> 0.09) values fitness
        Assert.AreEqual(1, selectedValue)

        let selectedValue = selectWithRandomNumberGenerator (fun _ -> 0.1) values fitness
        Assert.AreEqual(2, selectedValue)

        let selectedValue = selectWithRandomNumberGenerator (fun _ -> 0.3) values fitness
        Assert.AreEqual(2, selectedValue)

        let selectedValue = selectWithRandomNumberGenerator (fun _ -> 0.31) values fitness
        Assert.AreEqual(3, selectedValue)

        let selectedValue = selectWithRandomNumberGenerator (fun _ -> 0.6) values fitness
        Assert.AreEqual(3, selectedValue)

        let selectedValue = selectWithRandomNumberGenerator (fun _ -> 0.61) values fitness
        Assert.AreEqual(4, selectedValue)

        let selectedValue = selectWithRandomNumberGenerator (fun _ -> 0.99) values fitness
        Assert.AreEqual(4, selectedValue)
end