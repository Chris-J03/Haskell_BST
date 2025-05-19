import Test.HUnit

import BinarySearchTree

runTestForOutputText :: Test -> IO String
runTestForOutputText theTest = do
    (_, testOutputAccumulator) <- runTestText putTextToShowS theTest
    let testOutput = testOutputAccumulator ""
    return testOutput

main :: IO ()
main = do
    searchTestOutput <- runTestForOutputText searchTests
    insertTestOutput <- runTestForOutputText insertTests
    deleteTestOutput <- runTestForOutputText deleteTests
    displayTestOutput <- runTestForOutputText displayTests
    let combinedOutput = searchTestOutput ++ "\n" ++ insertTestOutput ++ "\n" ++ displayTestOutput ++ "\n" ++ deleteTestOutput ++ "\n"
    writeFile "testOutput.txt" combinedOutput
    putStrLn searchTestOutput
    putStrLn insertTestOutput
    putStrLn displayTestOutput
    putStrLn deleteTestOutput
    return ()

-- Search function tests
searchTests :: Test
searchTests = TestList [
    TestCase (assertEqual "Identifying first number in BST" (Just "Five") (search 5 myTree)),
    TestCase (assertEqual "Identifying smaller number that is in BST" (Just "Three") (search 3 myTree)),
    TestCase (assertEqual "Identifying Larger number that is in BST" (Just "Eight") (search 8 myTree)),
    TestCase (assertEqual "Identifying number that is not in the BST" Nothing (search 12 myTree)),
    TestCase (assertEqual "Search in empty tree" Nothing (search 1 Leaf))
    ]

-- Insert function tests
insertTests :: Test
insertTests = TestList [
    TestCase (assertEqual "Insert smaller number into BST" (Just "Four") (search 4 (insert 4 "Four" myTree))),
    TestCase (assertEqual "Insert larger number into BST" (Just "Nine") (search 9 (insert 9 "Nine" myTree))),
    TestCase (assertEqual "Insert the same number into BST" (Just "Five") (search 5 (insert 5 "Five" myTree))),
    TestCase (assertEqual "Insert into empty tree" (Just "Root") (search 10 (insert 10 "Root" Leaf))),

    let t = insert 7 "Seven" $ insert 2 "Two" myTree
    in TestCase (assertEqual "Display after inserts" "1:One 2:Two 3:Three 5:Five 7:Seven 8:Eight" (displayTree t))
    ]

-- Display function tests
displayTests :: Test
displayTests = TestList [
    TestCase (assertEqual "Display the default tree" "1:One 3:Three 5:Five 8:Eight" (displayTree myTree)),
    TestCase (assertEqual "Display empty tree" "" (displayTree Leaf))
    ] 

-- Delete function tests
deleteTests :: Test 
deleteTests = TestList [
    TestCase (assertEqual "Remove node with no child" Nothing (search 8 (delete 8 myTree))),
    TestCase (assertEqual "Remove node with 1one child" Nothing (search 3 (delete 3 myTree))),
    TestCase (assertEqual "Remove node not in tree" Nothing (search 12 (delete 12 myTree))),
    TestCase (assertEqual "Delete root with two children" Nothing (search 5 (delete 5 myTree)))
    ]