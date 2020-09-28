-- given a list of words, construct a sentence that indicates the first letter of word for spelling of word
-- i.e. speller ["lion", "tiger", "cheetah"] -> "l is for lion, and t is for tiger, and c is for cheetah"
speller :: [[Char]] -> [Char]
speller words = joinPhrases ", and " (letterPhraseFromWord words)

-- form list of phrases from first letter of each word in list i.e. letterPhraseFromWord ["lion", "tiger", "cheetah"] -> ["l is for lion", "t is for tiger", "c is for cheetah"]
letterPhraseFromWord :: [[Char]] -> [[Char]]
letterPhraseFromWord words = map (\word -> [head word] ++ " is for " ++ word) words

-- concat phrases of letter from word with separator i.e. joinPhrases ", and " ["l is for lion", "t is for tiger", "c is for cheetah"] -> "l is for lion, and t is for tiger, and c is for cheetah"
joinPhrases :: [Char] -> [[Char]] -> [Char] 
joinPhrases separator phrases = foldr (\acc phrase -> acc ++ if phrase=="" then phrase else separator ++ phrase) "" phrases
