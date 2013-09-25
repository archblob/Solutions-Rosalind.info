module HAMM (
      countPointMutations
    , countForSamples
    ) where

-- [1] Simple implementation

type DNAString = String
type Nucleotide = Char

countPointMutations:: DNAString -> DNAString -> Int
countPointMutations dna1 dna2 = go dna1 dna2 0
    where
        go [] [] c = c
        go (x:xs) (y:ys) c
            | x /= y = go xs ys (c+1)
            | otherwise = go xs ys c

sampleOne::DNAString
sampleOne = "CACGCGTGTTATTTTATATCTATTAACTGGAGGGAACGTTAACGCACTCCAAGGCGTTATGTGAGAGTCCACTCCACGAGCTATGTGATATTAGACAATCCTCACGCCGGCTCTGACAGCTCCGGCGCCGCTTAGGTCGATCAGCTCGTGAGCTCCTTGGACAAAACTCTTTTAGCCTGGGTGGTATATATAAGTGAGCGATTGAGTAGTTTTATGGCTTGCTGTTTATATAACGACATGATACTCCGGATATTGAATGGGCCGTGTCCTAGAACCTCCCCCAACAGTGTAACGCAAGCGTCTGTTGCTCGTAAAACTTGCGGGGCATTAGAACCAAAGCTTTCAAGTAGCCTTGTTCGAACCACTGGCGAGGTGTCCTCCAACGCGGTGAGAGTCCCCTTGTGATCCCCGCCGTGGCGCGACTTAGTCACCTATTTACGTAAGGTTCTAACATACCTTTAAGATCCCGAGGCTGATCTAAATACATCTAGTAGTACCATAGAGCACCTTAGCCACTTTAATGTAGCAGACACTTACCCAGTCCGTGGGTATTGGCGACCGTATTGTGTGCGAGAACAAGCCAGCCACGTTTCCTAGGACAGCTTCAGGGTCGCTACACAACTAATGAATCAGAGTCTTTACACTATCGCACCATGATTGCAAGTGTCGTGAGGCCGGGTACTAGAGCTAGAGGCACACAGAAGGGAACGGGTCCCATGCTCATCGACGGTAGAGACCCCCTGTTATCCTAGCCCGGAACAGTTGGTCACCGTCCATTTATAAACATTGCCCAGCGGGTCGCATTAACTCATGTGCGGATTTTGCATTGTTTAATATTTACCATGCTAGTTGAAGTTAGTTCTATTACACACAGCCTATACAAACAGTTAATGGAGGATGTGAGCATGCATTCTAACAAGTTACGTACACCAATTCGTGCCAT"

sampleTwo::DNAString
sampleTwo = "CACCGGCGTGATATGCCGTTTAGAGAGGGCTGTCAACGTTAACGCATTCAACGGAGCTATTAGAGTGCTCCGTGAACGGACTTTGTGATACTTGACGTGTCTTGAGCCGGCGCTCACGACTGCAAAGCCTTGCCAGTAAGTCGGGCAGTAAAATACTTTTATTCTACTTGAGTGCTTGAAGGGTCATGCATCGTTGCACGATTGCGTTCGTGTTGGGGTATTTGTTACATGGACGGCTTCTTAAGGAGTTACCCTGACGGCGCGCGGCCAAGGAATCCCATTCACTACGATATCTTAGCGGCGGTGAATGGCGAGATTGGCTGGGGATCAAAATCACATGTCGGAGGTATGATCCTTATGGTCCCCGTTGATGTGTGCTCAAAGGTCAACCCTACCCGCTTCTGGTCCGAGCTATCGCGGCGTTTCATCAGTTGGCTATGGGTAGGACTCTCCGTGTTATGGGCTCTCGTGCAGGTTATTCGCACAATTGGGAGGTGAGTAGAGCCCTTTATCCACTATAGTATCAGAATACCGGACAGACTATATCACAATCTGTCATCGCATTGTCTCCTCGAACAATCGTACAAGCAATGCTTGGCCTGGGCCTGGGACTCTTTCCTAGACCTCATCACAAGCTTTTCGACTGTCGCTTCCTGATGGGAAGGGTACTGTGACCAGAGAGTACTAAGGGGCGAACAGTGACTTCGACGCCTCACACGCCCACCGGCACTAGAGACACGCCGTTACACTATCGCCCAACCGTAGGCTGCGGTGGATTTTTCCACCATACCATGAAGATAGTATTATCAAAAGTCCCGGTCTTGCATACCACGATGATATTCTGGTGGGTGGAGTTTTTATGTATGCTATCCCGCCAGTTAATTTATCATGAGGCGGTTGTGTCGATTTAATTATTCAATTTCCGTAGGCGTAAACCTACCAG"

countForSamples = countPointMutations sampleOne sampleTwo