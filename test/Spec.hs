import Test.Hspec
import Data.Vector
import Day01
import Day02

main = hspec $ do
    describe "Day01" $ do
        let sampleMasses = [1969, 100756]
        it "solution 1 computes the total requirement for multiple modules" $ 
            answer01a sampleMasses `shouldBe` 34237

        it "solution 2 computes the fuel-included requirement for multiple modules" $
            answer01b sampleMasses `shouldBe` 51312

    describe "Day02" $ do 
        let addProgram = [1, 0, 0, 0, 99]
        let vectorAddProg = (fromList addProgram, 0)
        describe "addStep" $
            it "generates the correct modification" $
                addStep vectorAddProg `shouldBe` [(0, 2)]

        describe "answer02a" $ do
            let sampleProgram1 = [1,1,1,4,99,5,6,0,99]
            let sampleProgram2 = [2,4,4,5,99,0]
            let sampleProgram3 = [2,3,0,3,99]
            let sampleProgram4 = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,19,6,23,2,6,23,27,2,27,9,31,1,5,31,35,1,35,10,39,2,39,9,43,1,5,43,47,2,47,10,51,1,51,6,55,1,5,55,59,2,6,59,63,2,63,6,67,1,5,67,71,1,71,9,75,2,75,10,79,1,79,5,83,1,10,83,87,1,5,87,91,2,13,91,95,1,95,10,99,2,99,13,103,1,103,5,107,1,107,13,111,2,111,9,115,1,6,115,119,2,119,6,123,1,123,6,127,1,127,9,131,1,6,131,135,1,135,2,139,1,139,10,0,99,2,0,14,0]
            let sampleProgram5 = [1,9,10,3,2,3,11,0,99,30,40,50]
            it "computes multiple steps correctly" $
                answer02a sampleProgram1 [] `shouldBe` [30,1,1,4,2,5,6,0,99]
            it "computes multiplication correctly" $
                answer02a sampleProgram2 [] `shouldBe` [2,4,4,5,99,9801]
            it "computes addition correctly" $
                answer02a sampleProgram3 [] `shouldBe` [2,3,0,6,99]
            it "computes medium programs correctly" $
                answer02a sampleProgram5 [] `shouldBe` [3500,9,10,70,2,3,11,0,99,30,40,50]
